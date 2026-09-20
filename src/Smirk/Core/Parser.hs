{-# LANGUAGE OverloadedStrings #-}

module Smirk.Core.Parser
  ( parsePCRE
  , parsePCREWithFlags
  , ParserError
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

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

-- | Parse a PCRE regular expression string with default flags.
parsePCRE :: Text -> Either Text AST
parsePCRE = parsePCREWithFlags defaultFlags

-- | Parse a PCRE regular expression string with explicit initial flags.
parsePCREWithFlags :: Flags -> Text -> Either Text AST
parsePCREWithFlags initFlags input =
  case runParser (pRegex initFlags) "pcre" input of
    Left err -> Left (T.pack $ errorBundlePretty err)
    Right ast -> Right ast

pRegex :: Flags -> Parser AST
pRegex _initFlags = do
  res <- pAlternation
  eof
  pure res

pAlternation :: Parser AST
pAlternation = do
  branches <- sepBy1 pSequence (char '|')
  pure $ case branches of
    [singleBranch] -> singleBranch
    multiple       -> Alt multiple

pSequence :: Parser AST
pSequence = do
  items <- many pRepeatItem
  pure $ case items of
    []           -> Empty
    [singleItem] -> singleItem
    multiple     -> Seq multiple

pRepeatItem :: Parser AST
pRepeatItem = do
  atom <- pAtom
  mRep <- optional pRepetition
  pure $ case mRep of
    Nothing -> atom
    Just (minN, maxN, kind) -> Repeat minN maxN kind atom

pRepetition :: Parser (Int, Maybe Int, RepKind)
pRepetition = do
  (minN, maxN) <- pRepRange
  kind <- pRepModifier
  pure (minN, maxN, kind)
  where
    pRepRange =
      (char '*' >> pure (0, Nothing))
      <|> (char '+' >> pure (1, Nothing))
      <|> (char '?' >> pure (0, Just 1))
      <|> pBraceRep

    pBraceRep = between (char '{') (char '}') $ do
      n <- L.decimal
      mComma <- optional (char ',')
      case mComma of
        Nothing -> pure (n, Just n)
        Just _ -> do
          mMax <- optional L.decimal
          pure (n, mMax)

    pRepModifier =
      (char '?' >> pure RepLazy)
      <|> (char '+' >> pure RepPossessive)
      <|> pure RepGreedy

pAtom :: Parser AST
pAtom =
  pGroup
  <|> pClassSet
  <|> pDot
  <|> pAnchor
  <|> pBackRef
  <|> pEscaped
  <|> pLiteral

pDot :: Parser AST
pDot = char '.' >> pure (Class ClassAny)

pAnchor :: Parser AST
pAnchor =
  (char '^' >> pure (AnchorAssert AnchorStart))
  <|> (char '$' >> pure (AnchorAssert AnchorEnd))
  <|> (string "\\b" >> pure (AnchorAssert AnchorWordBoundary))
  <|> (string "\\B" >> pure (AnchorAssert AnchorNotWordBoundary))

pGroup :: Parser AST
pGroup = between (char '(') (char ')') $ do
  mSpecial <- optional (char '?')
  case mSpecial of
    Nothing -> do
      inner <- pAlternation
      pure $ Group Nothing Nothing inner
    Just _ -> do
      mPcreType <- (string ":" >> (NonCapturing <$> pAlternation))
        <|> (string "=" >> (Lookaround Lookahead True <$> pAlternation))
        <|> (string "!" >> (Lookaround Lookahead False <$> pAlternation))
        <|> (string "<=" >> (Lookaround Lookbehind True <$> pAlternation))
        <|> (string "<!" >> (Lookaround Lookbehind False <$> pAlternation))
        <|> pNamedGroup
      pure mPcreType

pNamedGroup :: Parser AST
pNamedGroup = do
  void (char '<')
  name <- takeWhile1P (Just "group name") (/= '>')
  void (char '>')
  inner <- pAlternation
  pure $ Group (Just name) Nothing inner

pClassSet :: Parser AST
pClassSet = between (char '[') (char ']') $ do
  negated <- (char '^' >> pure True) <|> pure False
  ranges <- many pRangeItem
  pure $ Class (ClassSet ranges negated)

pRangeItem :: Parser CharRange
pRangeItem = try pBetweenRange <|> pSingleChar
  where
    pBetweenRange = do
      startC <- noneOf [']', '-']
      void (char '-')
      endC <- noneOf [']']
      pure $ RangeBetween startC endC
    pSingleChar = RangeSingle <$> noneOf [']']

pBackRef :: Parser AST
pBackRef = try $ do
  void (char '\\')
  d <- satisfy isDigit
  pure $ BackRef (read [d])

pEscaped :: Parser AST
pEscaped = do
  void (char '\\')
  c <- anySingle
  pure $ case c of
    'd' -> Class ClassDigit
    'D' -> Class ClassNotDigit
    'w' -> Class ClassWord
    'W' -> Class ClassNotWord
    's' -> Class ClassSpace
    'S' -> Class ClassNotSpace
    'n' -> Lit '\n'
    'r' -> Lit '\r'
    't' -> Lit '\t'
    other -> Lit other

pLiteral :: Parser AST
pLiteral = Lit <$> noneOf ("^$\\.*+?()[]{}|" :: String)
