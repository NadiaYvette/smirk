{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
defaultGlobOptions :: GlobOptions
defaultGlobOptions = GlobOptions
  { matchPathSeparators = True
  , caseSensitive       = True
  }

-- | Unconstrained wildcard configuration (for tool names, namespaces, e.g. "mcp__*").
wildcardGlobOptions :: GlobOptions
wildcardGlobOptions = GlobOptions
  { matchPathSeparators = False
  , caseSensitive       = True
  }

type GlobParser = Parsec Void Text

-- | Compile a filesystem glob pattern (e.g. "src/**/*.hs", "test/Spec?.hs").
compileGlob :: Text -> Either Text Regex
compileGlob = compileGlobWithOptions defaultGlobOptions

-- | Compile an unconstrained wildcard pattern where '*' matches all characters.
compileWildcard :: Text -> Either Text Regex
compileWildcard = compileGlobWithOptions wildcardGlobOptions

-- | Compile a glob with custom 'GlobOptions'.
compileGlobWithOptions :: GlobOptions -> Text -> Either Text Regex
compileGlobWithOptions opts input = do
  ast <- globToAST opts input
  pure (compileToRegex input ast)

-- | Convert a glob pattern string into a typed Smirk 'AST'.
globToAST :: GlobOptions -> Text -> Either Text AST
globToAST opts input =
  case runParser (pGlob opts) "glob" input of
    Left err -> Left (T.pack $ errorBundlePretty err)
    Right ast -> Right ast

-- | Test if a target sequence matches a compiled glob regex.
matchGlob :: RegexTarget s => Regex -> s -> Bool
matchGlob regex target =
  case runMatch regex target Unlimited of
    Matched _ -> True
    _         -> False

pGlob :: GlobOptions -> GlobParser AST
pGlob opts = do
  items <- many (pGlobItem opts)
  eof
  pure $ Seq ([AnchorAssert AnchorStart] ++ items ++ [AnchorAssert AnchorEnd])

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

pSlashDoubleStarSlash :: GlobOptions -> GlobParser AST
pSlashDoubleStarSlash opts
  | opts.matchPathSeparators = try $ do
      void (string "/**/")
      -- Matches "/" or "/.../"
      pure $ Alt
        [ Lit '/'
        , Seq [Lit '/', Repeat 0 Nothing RepGreedy (Class ClassAny), Lit '/']
        ]
  | otherwise = empty

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

pSingleStar :: GlobOptions -> GlobParser AST
pSingleStar opts
  | opts.matchPathSeparators =
      char '*' >> pure (Repeat 0 Nothing RepGreedy (Class (ClassSet [RangeSingle '/'] True)))
  | otherwise =
      char '*' >> pure (Repeat 0 Nothing RepGreedy (Class ClassAny))

pQuestionMark :: GlobOptions -> GlobParser AST
pQuestionMark opts
  | opts.matchPathSeparators =
      char '?' >> pure (Class (ClassSet [RangeSingle '/'] True))
  | otherwise =
      char '?' >> pure (Class ClassAny)

pBraceExpansion :: GlobOptions -> GlobParser AST
pBraceExpansion opts = between (char '{') (char '}') $ do
  branches <- sepBy1 (many (pBraceInnerItem opts)) (char ',')
  pure $ Alt (map Seq branches)

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

pEscapedChar :: GlobParser AST
pEscapedChar = do
  void (char '\\')
  c <- anySingle
  pure (Lit c)

pLiteralChar :: GlobParser AST
pLiteralChar =
  (Lit <$> noneOf ("*?{}[],;/\\" :: String))
  <|> (char '/' >> pure (Lit '/'))
  <|> (char ',' >> pure (Lit ','))
  <|> (char ';' >> pure (Lit ';'))

-- | QuasiQuoter for compile-time verified glob patterns.
--
-- Example:
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

quoteGlobExp :: String -> Q Exp
quoteGlobExp globStr =
  case compileGlob (T.pack globStr) of
    Left err -> fail ("Smirk glob compile error: " <> T.unpack err)
    Right _ ->
      [| case compileGlob (T.pack $(stringE globStr)) of
           Right r -> r
           Left e  -> error ("Smirk glob internal error: " <> T.unpack e)
       |]
