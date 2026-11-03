{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      : Smirk.Core.Quote
Description : Compile-time QuasiQuoters for PCRE expressions and search-and-replace editors
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Provides Template Haskell QuasiQuoters that validate regular expressions and
stream-editor replacement templates at compile time, eliminating runtime parse
failures and overhead.

=== Available QuasiQuoters

* '[re|...|]': Compile-time validated PCRE regular expressions.
* '[ed|search///replace|]': Compile-time validated sed-style stream editor.
-}
module Smirk.Core.Quote
  ( -- * QuasiQuoters
    re
  , ed

    -- * Editor Definition & Application
  , Editor (..)
  , applyEditor
  , replaceMatch
  ) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Compiler (compileToRegex)
import Smirk.Core.Parser (parsePCRE)
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM (MatchResult (..), findAllMatches)

-- | QuasiQuoter for compile-time verified PCRE regular expressions.
-- Rejects invalid PCRE patterns at compile time with descriptive syntax errors.
--
-- === Example
--
-- @
-- let r = [re|(?<protocol>https?)://(?<domain>[a-z.]+)|]
-- @
re :: QuasiQuoter
re = QuasiQuoter
  { quoteExp  = quoteRegexExp
  , quotePat  = error "Smirk.re: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.re: Type quasiquote not supported"
  , quoteDec  = error "Smirk.re: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for regex patterns.
quoteRegexExp :: String -> Q Exp
quoteRegexExp patternStr =
  case parsePCRE (T.pack patternStr) of
    Left err -> fail ("Smirk regex compile error: " <> T.unpack err)
    Right _  ->
      [| case parsePCRE (T.pack $(stringE patternStr)) of
           Right ast -> compileToRegex (T.pack $(stringE patternStr)) ast
           Left e    -> error ("Smirk regex internal error: " <> T.unpack e)
       |]

-- | Structured find-and-replace editor record.
data Editor s = Editor
  { editRegex       :: !Regex   -- ^ Compiled search regular expression
  , editReplacement :: !Text    -- ^ Replacement template string
  , editGlobal      :: !Bool    -- ^ If True, replace all non-overlapping occurrences; if False, first only
  } deriving (Eq, Show, Generic)

-- | QuasiQuoter for compile-time verified search-and-replace editors using @///@ delimiters.
--
-- Format: @[ed|search_pattern///replacement_string|]@ or @[ed|search///replace///1|]@ (for single replacement).
--
-- === Example
--
-- @
-- let edScript = [ed|foo([0-9]+)///bar${1}|]
-- applyEditor edScript "foo42" == "bar42"
-- @
ed :: QuasiQuoter
ed = QuasiQuoter
  { quoteExp  = quoteEditorExp
  , quotePat  = error "Smirk.ed: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.ed: Type quasiquote not supported"
  , quoteDec  = error "Smirk.ed: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for search-and-replace editor templates.
quoteEditorExp :: String -> Q Exp
quoteEditorExp rawStr =
  case T.splitOn "///" (T.pack rawStr) of
    [searchPat, replaceStr] -> validateAndBuild searchPat replaceStr True
    [searchPat, replaceStr, opts] ->
      let isGlobal = not ("1" `T.isInfixOf` opts)
      in validateAndBuild searchPat replaceStr isGlobal
    _ ->
      fail "Smirk.ed: Expected format [ed|search_pattern///replacement_template|]"
  where
    validateAndBuild searchPat replaceStr isGlobal =
      case parsePCRE searchPat of
        Left err -> fail ("Smirk editor search regex error: " <> T.unpack err)
        Right _  ->
          [| Editor
               { editRegex = case parsePCRE (T.pack $(stringE (T.unpack searchPat))) of
                   Right ast -> compileToRegex (T.pack $(stringE (T.unpack searchPat))) ast
                   Left e    -> error ("Smirk editor search regex error: " <> T.unpack e)
               , editReplacement = T.pack $(stringE (T.unpack replaceStr))
               , editGlobal = $([| isGlobal |])
               }
           |]

-- | Apply an 't:Editor' transformation to a target sequence.
-- Finds matching slices and substitutes them according to the replacement template.
applyEditor :: RegexTarget s => Editor s -> s -> s
applyEditor editor target =
  let matches = findAllMatches editor.editRegex target
      selectedMatches = if editor.editGlobal then matches else take 1 matches
  in if null selectedMatches
       then target
       else
         let pieces = reconstructSlices target 0 selectedMatches editor.editReplacement
         in seqFromText (T.concat pieces)

-- | Reconstruct a sequence by interleaving untouched slices with substituted replacements.
reconstructSlices :: RegexTarget s => s -> Int -> [MatchResult s] -> Text -> [Text]
reconstructSlices target curIdx [] _ =
  let remLen = seqLength target - curIdx
  in [seqToText (seqSlice curIdx remLen target)]
reconstructSlices target curIdx (m : ms) replTemplate =
  let prefixLen = m.matchOffset - curIdx
      prefix = seqToText (seqSlice curIdx prefixLen target)
      replaced = replaceMatch m replTemplate
      nextIdx = m.matchOffset + m.matchLength
  in prefix : replaced : reconstructSlices target nextIdx ms replTemplate

-- | Expand capture group variables (${1}, ${name}, \1, $0, $&) in a replacement template.
replaceMatch :: RegexTarget s => MatchResult s -> Text -> Text
replaceMatch res = go
  where
    go txt
      | T.null txt = ""
      | "${" `T.isPrefixOf` txt =
          let rest = T.drop 2 txt
              (varName, after) = T.breakOn "}" rest
              varVal = lookupVar (T.unpack varName)
          in if "}" `T.isPrefixOf` after
               then varVal <> go (T.drop 1 after)
               else "${" <> go rest
      | "\\" `T.isPrefixOf` txt && T.length txt >= 2 =
          let digitChar = T.index txt 1
          in if isDigit digitChar
               then lookupVar [digitChar] <> go (T.drop 2 txt)
               else "\\" <> go (T.drop 1 txt)
      | "$&" `T.isPrefixOf` txt || "$0" `T.isPrefixOf` txt =
          seqToText res.matchedSlice <> go (T.drop 2 txt)
      | otherwise =
          let (chunk, rest) = T.break (\c -> c == '$' || c == '\\') txt
          in if T.null chunk
               then T.take 1 txt <> go (T.drop 1 txt)
               else chunk <> go rest

    lookupVar str = case reads str :: [(Int, String)] of
      [(idx, "")] -> maybe "" seqToText (Map.lookup idx res.captureGroups)
      _           -> maybe "" seqToText (Map.lookup (T.pack str) res.namedGroups)
