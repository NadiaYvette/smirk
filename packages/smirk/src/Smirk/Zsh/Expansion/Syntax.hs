{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Smirk.Zsh.Expansion.Syntax
Description : Zsh-style parameter expansion typed AST and Megaparsec parser
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a typed AST and parser for Zsh parameter expansion syntax, supporting:
* Leading expansion flags: @(@)@, @(s/sep/)@, @(j/sep/)@, @(M)@, @(R)@
* Nested inside-out expansions: @${${(@)foo}[1]}@
* Double-quote context tracking: @"${(@)${foo}[1]}"@
* 1-based and range subscripts: @[1]@, @[1,3]@
* Prefix/suffix pattern stripping: @#@, @##@, @%@, @%%@
* Pattern substitution: @:s/pat/rep/@, @:gs/pat/rep/@
-}
module Smirk.Zsh.Expansion.Syntax
  ( -- * Typed AST
    ZshFlag (..)
  , Subscript (..)
  , StripOp (..)
  , SubstOp (..)
  , ZshParamExpansion (..)

    -- * Parser Combinators
  , parseZshParamExpansion
  ) where

import Control.Applicative (empty, (<|>))
import Data.Char (isAlphaNum, isDigit)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Zsh parameter expansion modifier flags.
data ZshFlag
  = FlagAtArrayPreserve       -- ^ @(@)@ Preserve array words under quoting
  | FlagSplit !Text           -- ^ @(s/sep/)@ Split scalar into words by delimiter
  | FlagJoin !Text            -- ^ @(j/sep/)@ Join words into scalar by delimiter
  | FlagMatchedPortionOnly    -- ^ @(M)@ Keep matched portion
  | FlagRestPortionOnly       -- ^ @(R)@ Keep unmatched rest portion
  deriving stock (Eq, Show, Generic)

-- | 1-based array and scalar subscript bounds.
data Subscript
  = SubscriptSingle !Int        -- ^ @[1]@ Single element or character
  | SubscriptRange !Int !Int    -- ^ @[1,3]@ Range of elements or slice
  deriving stock (Eq, Show, Generic)

-- | Greedy and non-greedy prefix and suffix stripping operators.
data StripOp
  = StripPrefixNonGreedy !Text  -- ^ @#pat@ Shortest prefix strip
  | StripPrefixGreedy !Text     -- ^ @##pat@ Longest prefix strip
  | StripSuffixNonGreedy !Text  -- ^ @%pat@ Shortest suffix strip
  | StripSuffixGreedy !Text     -- ^ @%%pat@ Longest suffix strip
  deriving stock (Eq, Show, Generic)

-- | Parameter pattern substitution operator.
data SubstOp = SubstOp
  { substGlobal  :: !Bool -- ^ True for @:gs@, False for @:s@
  , substPattern :: !Text -- ^ Search pattern
  , substReplace :: !Text -- ^ Replacement text
  } deriving stock (Eq, Show, Generic)

-- | Typed AST for a parsed Zsh parameter expansion expression.
data ZshParamExpansion = ZshParamExpansion
  { expQuoted     :: !Bool                           -- ^ True if enclosed in double quotes
  , expFlags      :: ![ZshFlag]                      -- ^ Leading modifier flags
  , expVarOrInner :: !(Either Text ZshParamExpansion) -- ^ Variable name or nested expansion
  , expSubscript  :: !(Maybe Subscript)              -- ^ Optional subscript indexing
  , expStrip      :: !(Maybe StripOp)                -- ^ Optional strip modifier
  , expSubst      :: !(Maybe SubstOp)                -- ^ Optional substitution modifier
  } deriving stock (Eq, Show, Generic)

type Parser = Parsec Void Text

-- | Parse a single Zsh modifier flag inside @(...)@.
pZshFlag :: Parser ZshFlag
pZshFlag =
      (char '@' $> FlagAtArrayPreserve)
  <|> (char 'M' $> FlagMatchedPortionOnly)
  <|> (char 'R' $> FlagRestPortionOnly)
  <|> pDelimitedFlag 's' FlagSplit
  <|> pDelimitedFlag 'j' FlagJoin

-- | Parse a delimiter-enclosed flag parameter such as @s/sep/@ or @j:,:@.
pDelimitedFlag :: Char -> (Text -> ZshFlag) -> Parser ZshFlag
pDelimitedFlag indicator con = do
  _ <- char indicator
  delim <- anySingle
  content <- takeWhileP (Just "delimiter content") (/= delim)
  _ <- char delim
  pure (con content)

-- | Parse the flags block @(...)@ if present.
pFlagsBlock :: Parser [ZshFlag]
pFlagsBlock =
  between (char '(') (char ')') (some pZshFlag) <|> pure []

-- | Parse integer value for subscript bounds.
pInt :: Parser Int
pInt = do
  sign <- (char '-' $> (-1)) <|> pure 1
  digits <- takeWhile1P (Just "digits") isDigit
  case reads (T.unpack digits) of
    [(v, "")] -> pure (sign * v)
    _         -> empty

-- | Parse a subscript expression @[i]@ or @[i,j]@.
pSubscript :: Parser Subscript
pSubscript = between (char '[') (char ']') $ do
  start <- pInt
  mEnd <- optional (char ',' *> pInt)
  pure $ case mEnd of
    Just end -> SubscriptRange start end
    Nothing  -> SubscriptSingle start

-- | Parse strip operators: ##, #, %%, %.
pStripOp :: Parser StripOp
pStripOp =
      (string "##" *> (StripPrefixGreedy <$> takeWhileP (Just "pattern") (/= '}')))
  <|> (char '#' *> (StripPrefixNonGreedy <$> takeWhileP (Just "pattern") (/= '}')))
  <|> (string "%%" *> (StripSuffixGreedy <$> takeWhileP (Just "pattern") (/= '}')))
  <|> (char '%' *> (StripSuffixNonGreedy <$> takeWhileP (Just "pattern") (/= '}')))

-- | Parse substitution operators: :s/pat/rep/ or :gs/pat/rep/.
pSubstOp :: Parser SubstOp
pSubstOp = do
  _ <- char ':'
  isGlob <- (string "gs" $> True) <|> (char 's' $> False)
  delim <- anySingle
  pat <- takeWhileP (Just "pattern") (/= delim)
  _ <- char delim
  rep <- takeWhileP (Just "replacement") (/= delim)
  _ <- optional (char delim)
  pure (SubstOp isGlob pat rep)

-- | Parse parameter variable identifier name.
pIdentifier :: Parser Text
pIdentifier = takeWhile1P (Just "identifier") (\c -> isAlphaNum c || c == '_')

-- | Parse unquoted parameter expansion starting with @${@ and closing with @}@.
pRawExpansion :: Parser ZshParamExpansion
pRawExpansion = do
  _ <- string "${"
  flags <- pFlagsBlock
  inner <- (Right <$> try pRawExpansion) <|> (Left <$> pIdentifier)
  sub1 <- optional pSubscript
  mStrip <- optional pStripOp
  mSubst <- optional pSubstOp
  sub2 <- optional pSubscript
  _ <- char '}'
  let finalSub = sub1 <|> sub2
  pure (ZshParamExpansion False flags inner finalSub mStrip mSubst)

-- | Top-level parser supporting optional enclosing double-quotes @\"${...}\"@.
pTopExpansion :: Parser ZshParamExpansion
pTopExpansion = do
  mQuote <- optional (char '"')
  case mQuote of
    Just _ -> do
      expNode <- pRawExpansion
      _ <- char '"'
      pure expNode { expQuoted = True }
    Nothing -> pRawExpansion

-- | Parse an entire input string into a 'ZshParamExpansion' AST.
parseZshParamExpansion :: Text -> Either Text ZshParamExpansion
parseZshParamExpansion input =
  case runParser (pTopExpansion <* eof) "zsh-expansion" (T.strip input) of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right ast -> Right ast
