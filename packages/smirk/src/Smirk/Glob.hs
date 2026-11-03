{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      : Smirk.Glob
Description : Native POSIX and Git-style globbing engine with compile-time QuasiQuoters
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Implements native globbing and wildcard pattern compilation directly on top of
Smirk's PCRE engine and AST compiler.

=== Supported Glob Syntax

* Single-level wildcards: @*@ matches any characters except directory separators (@/@).
* Recursive path wildcards: @**@ matches across directory hierarchies.
* Single-character wildcard: @?@ matches any single non-separator character.
* Character ranges: @[a-z]@, @[0-9]@, with negation @[!a-z]@ or @[^a-z]@.
* Alternation braces: @{src,test}/**/*.hs@.
* Escape sequences: @\\*@, @\\?@, @\\[@, @\\{@.
-}
module Smirk.Glob
  ( -- * Glob Compilation & Matching
    compileGlob
  , compileGlobWithOptions
  , compileWildcard
  , GlobOptions (..)
  , defaultGlobOptions
  , wildcardGlobOptions
  , globToAST
  , matchGlob

    -- * Compile-time QuasiQuoter
  , glob

    -- * Parser Combinators
  , GlobParser
  , pGlob
  , pGlobItem
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Megaparsec
import Text.Megaparsec.Char

import Smirk.Core.AST
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Compiler (compileToRegex)
import Smirk.Engine.Sequence (RegexTarget)
import Smirk.Engine.VM (MatchStatus (..), runMatch, Fuel (Unlimited))

-- | Configuration options for glob compilation.
data GlobOptions = GlobOptions
  { matchPathSeparators :: !Bool -- ^ If True, '*' does not cross '/' boundaries and '**' is recursive
  , caseSensitive       :: !Bool -- ^ If True, matching is case-sensitive
  } deriving (Eq, Show, Generic)

-- | Standard filesystem glob configuration (POSIX/Git style).
-- Enforces that '*' stops at '/' boundaries and '**' matches arbitrarily deep subdirectories.
defaultGlobOptions :: GlobOptions
defaultGlobOptions = GlobOptions
  { matchPathSeparators = True
  , caseSensitive       = True
  }

-- | Unconstrained wildcard configuration (for tool names, namespaces, e.g. "mcp__*").
-- Allows '*' to match any character including directory separators.
wildcardGlobOptions :: GlobOptions
wildcardGlobOptions = GlobOptions
  { matchPathSeparators = False
  , caseSensitive       = True
  }

-- | Megaparsec parser type for glob strings over strict Text.
type GlobParser = Parsec Void Text

-- | Compile a filesystem glob pattern (e.g. "src/**/*.hs", "test/Spec?.hs")
-- into an anchored Smirk 'Regex' using 'defaultGlobOptions'.
compileGlob :: Text -> Either Text Regex
compileGlob = compileGlobWithOptions defaultGlobOptions

-- | Compile an unconstrained wildcard pattern where '*' matches all characters.
compileWildcard :: Text -> Either Text Regex
compileWildcard = compileGlobWithOptions wildcardGlobOptions

-- | Compile a glob pattern with explicit 't:GlobOptions'.
compileGlobWithOptions :: GlobOptions -> Text -> Either Text Regex
compileGlobWithOptions opts input = do
  ast <- globToAST opts input
  pure (compileToRegex input ast)

-- | Convert a glob pattern string into a typed Smirk 'AST'.
globToAST :: GlobOptions -> Text -> Either Text AST
globToAST opts input =
  case runParser (pGlob opts) "glob" input of
    Left err  -> Left (T.pack $ errorBundlePretty err)
    Right ast -> Right ast

-- | Test if a target sequence matches a compiled glob regex.
matchGlob :: RegexTarget s => Regex -> s -> Bool
matchGlob regex target =
  case runMatch regex target Unlimited of
    Matched _ -> True
    _         -> False

-- | Top-level glob parser wrapping items between start and end anchors.
pGlob :: GlobOptions -> GlobParser AST
pGlob opts = do
  items <- many (pGlobItem opts)
  eof
  pure $ Seq (AnchorAssert AnchorStart : items ++ [AnchorAssert AnchorEnd])

-- | Parse individual glob items: recursive wildcards, single wildcards, braces, classes, escapes.
pGlobItem :: GlobOptions -> GlobParser AST
pGlobItem opts =
  pSlashDoubleStarSlash opts
  <|> pDoubleStar opts
  <|> pSingleStar opts
  <|> pQuestionMark opts
  <|> pBraceExpansion opts
  <|> pCharClass
  <|> pEscapedChar
  <|> pLiteralChar

-- | Parse @/**/@ representing zero or more directory levels.
pSlashDoubleStarSlash :: GlobOptions -> GlobParser AST
pSlashDoubleStarSlash opts
  | opts.matchPathSeparators = try $ do
      void (string "/**/")
      pure $ Alt
        [ Lit '/'
        , Seq [Lit '/', Repeat 0 Nothing RepGreedy (Class ClassAny), Lit '/']
        ]
  | otherwise = empty

-- | Parse @**/@ or terminal @**@ recursive wildcard.
pDoubleStar :: GlobOptions -> GlobParser AST
pDoubleStar opts
  | opts.matchPathSeparators = try $ do
      void (string "**/")
      pure $ Alt
        [ Empty
        , Seq [Repeat 0 Nothing RepGreedy (Class ClassAny), Lit '/']
        ]
      <|> (string "**" >> pure (Repeat 0 Nothing RepGreedy (Class ClassAny)))
  | otherwise = string "**" >> pure (Repeat 0 Nothing RepGreedy (Class ClassAny))

-- | Parse single-level @*@ wildcard matching within directory boundaries.
pSingleStar :: GlobOptions -> GlobParser AST
pSingleStar opts
  | opts.matchPathSeparators =
      char '*' >> pure (Repeat 0 Nothing RepGreedy (Class (ClassSet [RangeSingle '/'] True)))
  | otherwise =
      char '*' >> pure (Repeat 0 Nothing RepGreedy (Class ClassAny))

-- | Parse @?@ matching any single character except directory separator.
pQuestionMark :: GlobOptions -> GlobParser AST
pQuestionMark opts
  | opts.matchPathSeparators =
      char '?' >> pure (Class (ClassSet [RangeSingle '/'] True))
  | otherwise =
      char '?' >> pure (Class ClassAny)

-- | Parse brace expansion @{a,b,c}@ into an alternation.
pBraceExpansion :: GlobOptions -> GlobParser AST
pBraceExpansion opts = between (char '{') (char '}') $ do
  branches <- sepBy1 (many (pBraceInnerItem opts)) (char ',')
  pure $ Alt (map Seq branches)

-- | Parse inner items within a brace expansion.
pBraceInnerItem :: GlobOptions -> GlobParser AST
pBraceInnerItem opts =
  pSlashDoubleStarSlash opts
  <|> pDoubleStar opts
  <|> pSingleStar opts
  <|> pQuestionMark opts
  <|> pCharClass
  <|> pEscapedChar
  <|> (Lit <$> noneOf ("*?{}[],;/\\" :: String))
  <|> (char '/' >> pure (Lit '/'))
  <|> (char ';' >> pure (Lit ';'))

-- | Parse character classes with optional negation: @[a-z]@, @[!0-9]@, @[^A-Z]@.
pCharClass :: GlobParser AST
pCharClass = between (char '[') (char ']') $ do
  negated <- (char '!' >> pure True) <|> (char '^' >> pure True) <|> pure False
  ranges <- many pRange
  pure $ Class (ClassSet ranges negated)
  where
    pRange = try pBetween <|> pSingle
    pBetween = do
      s <- noneOf [']', '-']
      void (char '-')
      e <- noneOf [']']
      pure $ RangeBetween s e
    pSingle = RangeSingle <$> noneOf [']']

-- | Parse an escaped character: @\\*@, @\\?@, etc.
pEscapedChar :: GlobParser AST
pEscapedChar = do
  void (char '\\')
  Lit <$> anySingle

-- | Parse literal characters that do not carry special glob syntax meaning.
pLiteralChar :: GlobParser AST
pLiteralChar =
  (Lit <$> noneOf ("*?{}[],;/\\" :: String))
  <|> (char '/' >> pure (Lit '/'))
  <|> (char ',' >> pure (Lit ','))
  <|> (char ';' >> pure (Lit ';'))

-- | QuasiQuoter for compile-time verified glob patterns.
--
-- === Example
--
-- @
-- let r = [glob|src\/**\/*.hs|]
-- matchGlob r "src/Smirk/Core/AST.hs" == True
-- @
glob :: QuasiQuoter
glob = QuasiQuoter
  { quoteExp  = quoteGlobExp
  , quotePat  = error "Smirk.glob: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.glob: Type quasiquote not supported"
  , quoteDec  = error "Smirk.glob: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for glob strings.
quoteGlobExp :: String -> Q Exp
quoteGlobExp globStr =
  case compileGlob (T.pack globStr) of
    Left err -> fail ("Smirk glob compile error: " <> T.unpack err)
    Right _  ->
      [| case compileGlob (T.pack $(stringE globStr)) of
           Right r -> r
           Left e  -> error ("Smirk glob internal error: " <> T.unpack e)
       |]
