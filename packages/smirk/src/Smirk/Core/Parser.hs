{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Smirk.Core.Parser
Description : Recursive descent PCRE parser based on Megaparsec
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Implements a standard recursive descent parser for Perl-Compatible Regular
Expressions (PCRE) using Megaparsec.

=== Operator Precedence Hierarchy

1. __Alternation ('pAlternation'):__ Lowest precedence; separated by @|@.
2. __Concatenation ('pSequence'):__ Sequential concatenation of terms.
3. __Repetition ('pRepeatItem'):__ Suffix quantifiers: @*@, @+@, @?@, @{m,n}@,
   with optional greediness modifiers (@?@ for lazy, @+@ for possessive).
4. __Atoms ('pAtom'):__ Groups, character classes, anchors, backreferences,
   escaped characters, and character literals.
-}
module Smirk.Core.Parser
  ( -- * High-Level Parsing API
    parsePCRE
  , parsePCREWithFlags
  , ParserError

    -- * Grammar Combinators
  , Parser
  , pRegex
  , pAlternation
  , pSequence
  , pRepeatItem
  , pRepetition
  , pAtom
  , pGroup
  , pNamedGroup
  , pClassSet
  , pRangeItem
  , pBackRef
  , pEscaped
  , pLiteral
  ) where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Smirk.Core.AST

-- | Megaparsec parser type over strict Text with Void error components.
type Parser = Parsec Void Text

-- | Pretty-printable Megaparsec error bundle type.
type ParserError = ParseErrorBundle Text Void

-- | Parse a PCRE regular expression string using default compilation flags.
-- Returns 'Right' 'AST' on success, or 'Left' errorMessage on failure.
parsePCRE :: Text -> Either Text AST
parsePCRE = parsePCREWithFlags defaultFlags

-- | Parse a PCRE regular expression string with explicit initial flags.
parsePCREWithFlags :: Flags -> Text -> Either Text AST
parsePCREWithFlags initFlags input =
  case runParser (pRegex initFlags) "pcre" input of
    Left err  -> Left (T.pack $ errorBundlePretty err)
    Right ast -> Right ast

-- | Top-level regular expression parser consuming to end-of-input.
pRegex :: Flags -> Parser AST
pRegex _initFlags = do
  res <- pAlternation
  eof
  pure res

-- | Parse an alternation branch (@branch1 | branch2 | ...@).
pAlternation :: Parser AST
pAlternation = do
  branches <- sepBy1 pSequence (char '|')
  pure $ case branches of
    [singleBranch] -> singleBranch
    multiple       -> Alt multiple

-- | Parse a sequence of concatenated items (@item1 item2 ...@).
pSequence :: Parser AST
pSequence = do
  items <- many pRepeatItem
  pure $ case items of
    []           -> Empty
    [singleItem] -> singleItem
    multiple     -> Seq multiple

-- | Parse an atom with an optional repetition suffix.
pRepeatItem :: Parser AST
pRepeatItem = do
  atom <- pAtom
  mRep <- optional pRepetition
  pure $ case mRep of
    Nothing                 -> atom
    Just (minN, maxN, kind) -> Repeat minN maxN kind atom

-- | Parse repetition quantifiers: @*@, @+@, @?@, or range @{min,max}@,
-- accompanied by an optional greediness modifier (@?@ for lazy, @+@ for possessive).
pRepetition :: Parser (Int, Maybe Int, RepKind)
pRepetition = do
  (minN, maxN) <- pRepRange
  kind <- pRepModifier
  pure (minN, maxN, kind)
  where
    -- Basic repetition range
    pRepRange =
      (char '*' >> pure (0, Nothing))
      <|> (char '+' >> pure (1, Nothing))
      <|> (char '?' >> pure (0, Just 1))
      <|> pBraceRep

    -- Bounded repetition syntax: {n}, {n,}, or {n,m}
    pBraceRep = between (char '{') (char '}') $ do
      n <- L.decimal
      mComma <- optional (char ',')
      case mComma of
        Nothing -> pure (n, Just n)
        Just _ -> do
          mMax <- optional L.decimal
          pure (n, mMax)

    -- Greediness modifier: default is greedy, '?' is lazy, '+' is possessive
    pRepModifier =
      (char '?' >> pure RepLazy)
      <|> (char '+' >> pure RepPossessive)
      <|> pure RepGreedy

-- | Parse an atomic expression: group, class set, dot, anchor, backreference, or literal.
pAtom :: Parser AST
pAtom =
  pGroup
  <|> pClassSet
  <|> pDot
  <|> pAnchor
  <|> pBackRef
  <|> pEscaped
  <|> pLiteral

-- | Parse @.@ matching any character (subject to dotAll flag).
pDot :: Parser AST
pDot = char '.' >> pure (Class ClassAny)

-- | Parse zero-width position anchors: @^@, @$@, @\\b@, @\\B@.
pAnchor :: Parser AST
pAnchor =
  (char '^' >> pure (AnchorAssert AnchorStart))
  <|> (char '$' >> pure (AnchorAssert AnchorEnd))
  <|> (string "\\b" >> pure (AnchorAssert AnchorWordBoundary))
  <|> (string "\\B" >> pure (AnchorAssert AnchorNotWordBoundary))

-- | Parse parenthesized group constructs, handling both capturing and non-capturing/lookaround variants.
pGroup :: Parser AST
pGroup = between (char '(') (char ')') $ do
  mSpecial <- optional (char '?')
  case mSpecial of
    Nothing -> Group Nothing Nothing <$> pAlternation
    Just _ ->
      (string ":" >> (NonCapturing <$> pAlternation))
        <|> (string "=" >> (Lookaround Lookahead True <$> pAlternation))
        <|> (string "!" >> (Lookaround Lookahead False <$> pAlternation))
        <|> (string "<=" >> (Lookaround Lookbehind True <$> pAlternation))
        <|> (string "<!" >> (Lookaround Lookbehind False <$> pAlternation))
        <|> pNamedGroup

-- | Parse a named capture group: @(?<name>pattern)@.
pNamedGroup :: Parser AST
pNamedGroup = do
  void (char '<')
  name <- takeWhile1P (Just "group name") (/= '>')
  void (char '>')
  Group (Just name) Nothing <$> pAlternation

-- | Parse a bracketed character set: @[abc]@, @[a-z0-9]@, or negated @[^a-z]@.
pClassSet :: Parser AST
pClassSet = between (char '[') (char ']') $ do
  negated <- (char '^' >> pure True) <|> pure False
  ranges <- many pRangeItem
  pure $ Class (ClassSet ranges negated)

-- | Parse a character range within a set: either @a-z@ or a single character @a@.
pRangeItem :: Parser CharRange
pRangeItem = try pBetweenRange <|> pSingleChar
  where
    pBetweenRange = do
      startC <- noneOf [']', '-']
      void (char '-')
      endC <- noneOf [']']
      pure $ RangeBetween startC endC
    pSingleChar = RangeSingle <$> noneOf [']']

-- | Parse a numbered backreference: @\\1@, @\\2@, etc.
pBackRef :: Parser AST
pBackRef = try $ do
  void (char '\\')
  d <- satisfy isDigit
  pure $ BackRef (read [d])

-- | Parse an escape sequence: character classes (\d, \w, \s) or control characters (\n, \t, \r).
pEscaped :: Parser AST
pEscaped = do
  void (char '\\')
  c <- anySingle
  pure $ case c of
    'd'   -> Class ClassDigit
    'D'   -> Class ClassNotDigit
    'w'   -> Class ClassWord
    'W'   -> Class ClassNotWord
    's'   -> Class ClassSpace
    'S'   -> Class ClassNotSpace
    'n'   -> Lit '\n'
    'r'   -> Lit '\r'
    't'   -> Lit '\t'
    other -> Lit other

-- | Parse a plain literal character, disallowing special PCRE metacharacters.
pLiteral :: Parser AST
pLiteral = Lit <$> noneOf ("^$\\.*+?()[]{}|" :: String)
