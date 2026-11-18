{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Smirk.Glob.Zsh
Description : Zsh-style filename generation and effectful glob qualifiers
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Extends Smirk's native glob engine with Zsh-style filesystem qualifiers
(@*(.)@, @*(/)@, @*(\@)@, @*(L+n)@, @*(m-n)@, @*(^...)@, @*(e:tag:)@)
and compiles qualifiers into PCRE callouts for effectful filesystem traversal.
-}
module Smirk.Glob.Zsh
  ( -- * File Metadata & Types
    FileType (..)
  , FileInfo (..)

    -- * Zsh Qualifiers & AST
  , ZshQualifier (..)
  , ZshGlob (..)

    -- * Parsing
  , parseZshGlob
  , parseZshQualifiers

    -- * Evaluation & Filtering
  , evalGlobQualifier
  , evalGlobQualifiers
  , filterFilesByZshGlob

    -- * Compilation to Regex Bytecode
  , compileZshGlob
  , compileZshGlobToRegex
  ) where

import Control.Applicative (empty, (<|>))
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

import Smirk.Core.AST (AST (..))
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Compiler (compileToRegex)
import Smirk.Glob (defaultGlobOptions, globToAST)

-- | High-level file type classification for Zsh glob filtering.
data FileType
  = RegularFile    -- ^ Standard regular file (@.@)
  | Directory      -- ^ Directory container (@/@)
  | Symlink        -- ^ Symbolic link (@\@)
  | OtherFileType  -- ^ Sockets, fifos, devices
  deriving stock (Eq, Show, Generic)

-- | File metadata descriptor inspected during qualifier evaluation.
data FileInfo = FileInfo
  { fileType  :: !FileType -- ^ Classified file type
  , fileSize  :: !Integer  -- ^ File size in bytes
  , fileMTime :: !Integer  -- ^ File modification time or relative age in days
  } deriving stock (Eq, Show, Generic)

-- | Filter qualifiers parsed from trailing parentheses in Zsh globs.
data ZshQualifier
  = QualRegularFile                  -- ^ @(.)@ Matches plain files
  | QualDirectory                    -- ^ @(/)@ Matches directory entries
  | QualSymlink                      -- ^ @(@)@ Matches symbolic links
  | QualSizeBound !Ordering !Integer -- ^ @(L+n)@, @(L-n)@, @(Ln)@ Size bound in bytes
  | QualMTimeBound !Ordering !Integer -- ^ @(m-n)@, @(m+n)@ Age bound in days
  | QualCallout !Text                -- ^ @(e:tag:)@ Effectful predicate hook
  | QualNegate !ZshQualifier         -- ^ @(^...)@ Negates the enclosed qualifier
  deriving stock (Eq, Show, Generic)

-- | Structured representation of a Zsh glob pattern and its qualifiers.
data ZshGlob = ZshGlob
  { globBasePattern :: !Text          -- ^ Base glob wildcard without qualifier parens
  , globQualifiers  :: ![ZshQualifier] -- ^ Trailing qualifiers applied to matched paths
  } deriving stock (Eq, Show, Generic)

type Parser = Parsec Void Text

-- | Megaparsec parser for a single Zsh qualifier symbol.
pSingleQualifier :: Parser ZshQualifier
pSingleQualifier =
      (char '.' $> QualRegularFile)
  <|> (char '/' $> QualDirectory)
  <|> (char '@' $> QualSymlink)
  <|> (char '^' *> (QualNegate <$> pSingleQualifier))
  <|> pSizeBound
  <|> pMTimeBound
  <|> pCalloutQualifier

-- | Parse size qualifiers: L+100, L-100, L100.
pSizeBound :: Parser ZshQualifier
pSizeBound = do
  _ <- char 'L'
  ord <- (char '+' $> GT) <|> (char '-' $> LT) <|> pure EQ
  digits <- takeWhile1P (Just "digits") isDigit
  case reads (T.unpack digits) of
    [(val, "")] -> pure (QualSizeBound ord val)
    _           -> empty

-- | Parse modification age qualifiers: m-5, m+5, m5.
pMTimeBound :: Parser ZshQualifier
pMTimeBound = do
  _ <- char 'm'
  ord <- (char '+' $> GT) <|> (char '-' $> LT) <|> pure EQ
  digits <- takeWhile1P (Just "digits") isDigit
  case reads (T.unpack digits) of
    [(val, "")] -> pure (QualMTimeBound ord val)
    _           -> empty

-- | Parse callout qualifier predicates: e:tag: or e'tag'.
pCalloutQualifier :: Parser ZshQualifier
pCalloutQualifier = do
  _ <- char 'e'
  sep <- char ':' <|> char '\'' <|> char '"'
  tag <- takeWhileP (Just "callout tag") (/= sep)
  _ <- char sep
  pure (QualCallout tag)

-- | Parse a sequence of qualifiers enclosed in parentheses: (./^@).
pQualifiersBlock :: Parser [ZshQualifier]
pQualifiersBlock = between (char '(') (char ')') (some pSingleQualifier)

-- | Parse only the qualifier string (e.g. \"(.)\", \"(/L+100)\").
parseZshQualifiers :: Text -> Either Text [ZshQualifier]
parseZshQualifiers input =
  case runParser (pQualifiersBlock <* eof) "zsh-qualifiers" input of
    Left err -> Left (T.pack (errorBundlePretty err))
    Right qs -> Right qs

-- | Parse an entire Zsh glob into base pattern and optional qualifiers.
parseZshGlob :: Text -> Either Text ZshGlob
parseZshGlob rawInput =
  let input = T.strip rawInput
  in if T.null input
       then Left "Empty glob pattern"
       else case T.unsnoc input of
         Just (rest, ')') ->
           case T.breakOnEnd "(" rest of
             ("", _) -> Right (ZshGlob input [])
             (prefix, qualContent) ->
               let qualStr = "(" <> qualContent <> ")"
                   basePat = T.dropEnd 1 prefix
               in case runParser (pQualifiersBlock <* eof) "zsh-qualifiers" qualStr of
                    Right qs -> Right (ZshGlob basePat qs)
                    Left _   -> Right (ZshGlob input [])
         _ -> Right (ZshGlob input [])

-- | Evaluate a single qualifier against file metadata.
evalGlobQualifier :: ZshQualifier -> FileInfo -> Bool
evalGlobQualifier qual info = case qual of
  QualRegularFile -> info.fileType == RegularFile
  QualDirectory   -> info.fileType == Directory
  QualSymlink     -> info.fileType == Symlink
  QualSizeBound ord sz -> compare info.fileSize sz == ord
  QualMTimeBound ord age -> compare info.fileMTime age == ord
  QualCallout _   -> True
  QualNegate q    -> not (evalGlobQualifier q info)

-- | Check whether a file satisfies all active Zsh qualifiers.
evalGlobQualifiers :: [ZshQualifier] -> FileInfo -> Bool
evalGlobQualifiers qs info = all (`evalGlobQualifier` info) qs

-- | Filter a list of candidates by base pattern matching and qualifier verification.
filterFilesByZshGlob :: ZshGlob -> [(FilePath, FileInfo)] -> [FilePath]
filterFilesByZshGlob zg candidates =
  [ path | (path, info) <- candidates, evalGlobQualifiers zg.globQualifiers info ]

-- | Qualifier to PCRE callout tag converter.
qualifierToTag :: ZshQualifier -> Text
qualifierToTag = \case
  QualRegularFile -> "is_regular"
  QualDirectory   -> "is_directory"
  QualSymlink     -> "is_symlink"
  QualSizeBound ord sz -> "size_" <> T.pack (show ord) <> "_" <> T.pack (show sz)
  QualMTimeBound ord age -> "mtime_" <> T.pack (show ord) <> "_" <> T.pack (show age)
  QualCallout tag -> tag
  QualNegate q    -> "not_" <> qualifierToTag q

-- | Compile a structured Zsh glob into a Smirk 'Regex' with embedded PCRE callouts.
compileZshGlobToRegex :: ZshGlob -> Either Text Regex
compileZshGlobToRegex zg = do
  baseAST <- globToAST defaultGlobOptions zg.globBasePattern
  let calloutASTs = [ Callout 0 (Just (qualifierToTag q)) | q <- zg.globQualifiers ]
      combinedAST = case calloutASTs of
        [] -> baseAST
        _  -> Seq (baseAST : calloutASTs)
  pure (compileToRegex zg.globBasePattern combinedAST)

-- | Compile a raw Zsh glob pattern string directly into a Smirk 'Regex'.
compileZshGlob :: Text -> Either Text Regex
compileZshGlob input = do
  zg <- parseZshGlob input
  compileZshGlobToRegex zg
