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
  , pAlternationWithFlags
  , pSequence
  , pSequenceWithFlags
  , pRepeatItem
  , pRepeatItemWithFlags
  , pRepetition
  , pAtom
  , pAtomWithFlags
  , pGroup
  , pGroupWithFlags
  , pNamedGroup
  , pNamedGroupWithFlags
  , pCallout
  , pIntervalBounds
  , pIntervalGroup
  , pStandaloneInterval
  , pClassSet
  , pRangeItem
  , pBackRef
  , pEscaped
  , pLiteral
  ) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word64)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Smirk.Core.AST

{- Note [Lucene Decimal Natural Interval Syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-lucene-interval-syntax]
Lucene INTERVAL expressions match decimal natural numbers within an inclusive
range [min..max] deterministically in O(digits) time with zero backtracking.
Smirk supports:
  1. Parenthesized interval syntax: (?<min..max>) (always available)
  2. Standalone interval syntax: <min..max> (when Flags.intervalFlag is enabled)

The parser disambiguates (?<min..max>) from named capture groups (?<name>pattern)
by attempting to parse digits separated by '..' before the closing '>'. If the
content is not a valid decimal interval, it backtracks and parses as a named group.
Valid intervals enforce min <= max; min > max is rejected as a syntax error.
-}

{- Note [PCRE Callout Syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-pcre-callout-syntax]
PCRE callouts allow inspection and control points during pattern matching.
Smirk supports standard PCRE callout forms:
  1. Default callout: (?C) (index 0, no tag)
  2. Numbered callout: (?Cn) (index n, no tag)
  3. Tagged callout: (?C"tag") or (?C'tag') (index 0, tag text)
  4. Numbered and tagged callout: (?Cn"tag") (index n, tag text)

Callouts compile to the OpCallout / ICallout opcode, executing as zero-width
inspection points in the VM.
-}

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
pRegex flags = do
  res <- pAlternationWithFlags flags
  eof
  pure res

-- | Parse an alternation branch (@branch1 | branch2 | ...@) with default flags.
pAlternation :: Parser AST
pAlternation = pAlternationWithFlags defaultFlags

-- | Parse an alternation branch (@branch1 | branch2 | ...@) with explicit flags.
pAlternationWithFlags :: Flags -> Parser AST
pAlternationWithFlags flags = do
  branches <- sepBy1 (pSequenceWithFlags flags) (char '|')
  pure $ case branches of
    [singleBranch] -> singleBranch
    multiple       -> Alt multiple

-- | Parse a sequence of concatenated items (@item1 item2 ...@) with default flags.
pSequence :: Parser AST
pSequence = pSequenceWithFlags defaultFlags

-- | Parse a sequence of concatenated items (@item1 item2 ...@) with explicit flags.
pSequenceWithFlags :: Flags -> Parser AST
pSequenceWithFlags flags = do
  items <- many (pRepeatItemWithFlags flags)
  pure $ case items of
    []           -> Empty
    [singleItem] -> singleItem
    multiple     -> Seq multiple

-- | Parse an atom with an optional repetition suffix with default flags.
pRepeatItem :: Parser AST
pRepeatItem = pRepeatItemWithFlags defaultFlags

-- | Parse an atom with an optional repetition suffix with explicit flags.
pRepeatItemWithFlags :: Flags -> Parser AST
pRepeatItemWithFlags flags = do
  atom <- pAtomWithFlags flags
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

-- | Parse an atomic expression: group, interval, class set, dot, anchor, backreference, or literal.
pAtom :: Parser AST
pAtom = pAtomWithFlags defaultFlags

-- | Parse an atomic expression with explicit 'Flags'.
pAtomWithFlags :: Flags -> Parser AST
pAtomWithFlags flags =
  pGroupWithFlags flags
  <|> (if intervalFlag flags then pStandaloneInterval else empty)
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

-- | Parse parenthesized group constructs with default flags.
pGroup :: Parser AST
pGroup = pGroupWithFlags defaultFlags

-- | Parse parenthesized group constructs, handling capturing, non-capturing,
-- lookaround, callout, and interval variants.
pGroupWithFlags :: Flags -> Parser AST
pGroupWithFlags flags = between (char '(') (char ')') $ do
  mSpecial <- optional (char '?')
  case mSpecial of
    Nothing -> Group Nothing Nothing <$> pAlternationWithFlags flags
    Just _ ->
      (string ":" >> (NonCapturing <$> pAlternationWithFlags flags))
        <|> (string "=" >> (Lookaround Lookahead True <$> pAlternationWithFlags flags))
        <|> (string "!" >> (Lookaround Lookahead False <$> pAlternationWithFlags flags))
        <|> (string "<=" >> (Lookaround Lookbehind True <$> pAlternationWithFlags flags))
        <|> (string "<!" >> (Lookaround Lookbehind False <$> pAlternationWithFlags flags))
        <|> pCallout
        <|> pIntervalGroup
        <|> pNamedGroupWithFlags flags

-- | Parse PCRE Callout syntax: @(?C)@, @(?Cn)@, @(?C"tag")@, or @(?C'tag')@.
-- [ref:note-pcre-callout-syntax]
pCallout :: Parser AST
pCallout = do
  void (char 'C')
  mNum <- optional L.decimal
  mTag <- optional (pQuotedString '"' <|> pQuotedString '\'')
  let num = fromMaybe 0 mNum
  pure (Callout num mTag)
  where
    pQuotedString q = between (char q) (char q) (takeWhileP (Just "callout tag") (/= q))

-- | Parse Lucene interval bounds: @<min..max>@.
-- [ref:note-lucene-interval-syntax]
pIntervalBounds :: Parser (Word64, Word64)
pIntervalBounds = do
  void (char '<')
  minN <- L.decimal
  void (string "..")
  maxN <- L.decimal
  void (char '>')
  if minN > maxN
    then fail "invalid interval bounds: minimum bound cannot exceed maximum bound"
    else pure (minN, maxN)

-- | Parse a parenthesized decimal natural interval: @(?<min..max>)@.
-- [ref:note-lucene-interval-syntax]
pIntervalGroup :: Parser AST
pIntervalGroup = try $ do
  (minN, maxN) <- pIntervalBounds
  pure (IntervalNat minN maxN)

-- | Parse a standalone decimal natural interval: @<min..max>@.
-- Active when 'intervalFlag' is enabled in 'Flags'.
-- [ref:note-lucene-interval-syntax]
pStandaloneInterval :: Parser AST
pStandaloneInterval = try $ do
  (minN, maxN) <- pIntervalBounds
  pure (IntervalNat minN maxN)

-- | Parse a named capture group: @(?<name>pattern)@ with default flags.
pNamedGroup :: Parser AST
pNamedGroup = pNamedGroupWithFlags defaultFlags

-- | Parse a named capture group: @(?<name>pattern)@ with explicit flags.
pNamedGroupWithFlags :: Flags -> Parser AST
pNamedGroupWithFlags flags = do
  void (char '<')
  firstChar <- satisfy (\c -> isAlpha c || c == '_')
  restChars <- takeWhileP (Just "group name characters") (\c -> isAlphaNum c || c == '_')
  let name = T.cons firstChar restChars
  void (char '>')
  Group (Just name) Nothing <$> pAlternationWithFlags flags

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
