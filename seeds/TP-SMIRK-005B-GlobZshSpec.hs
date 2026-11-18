{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GlobZshSpec
Description : Test suite for Zsh-style filename generation and glob qualifiers
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
-}
module GlobZshSpec (spec) where

import Test.Hspec

import Smirk (testMatch)
import Smirk.Glob.Zsh
  ( FileInfo (..)
  , FileType (..)
  , ZshGlob (..)
  , ZshQualifier (..)
  , compileZshGlob
  , evalGlobQualifiers
  , filterFilesByZshGlob
  , parseZshGlob
  , parseZshQualifiers
  )

spec :: Spec
spec = describe "Smirk.Glob.Zsh (Zsh Glob Qualifiers)" $ do
  describe "Qualifier Parsing" $ do
    it "parses regular file qualifier (.)" $ do
      parseZshQualifiers "(.)" `shouldBe` Right [QualRegularFile]

    it "parses directory qualifier (/)" $ do
      parseZshQualifiers "(/)" `shouldBe` Right [QualDirectory]

    it "parses symlink qualifier (@)" $ do
      parseZshQualifiers "(@)" `shouldBe` Right [QualSymlink]

    it "parses negation (^/)" $ do
      parseZshQualifiers "(^/)" `shouldBe` Right [QualNegate QualDirectory]

    it "parses size bound (L+100)" $ do
      parseZshQualifiers "(L+100)" `shouldBe` Right [QualSizeBound GT 100]

    it "parses mtime bound (m-5)" $ do
      parseZshQualifiers "(m-5)" `shouldBe` Right [QualMTimeBound LT 5]

    it "parses callout predicate (e:tag:)" $ do
      parseZshQualifiers "(e:tag:)" `shouldBe` Right [QualCallout "tag"]

  describe "Full Glob Parsing" $ do
    it "parses base pattern and qualifiers from *.hs(.)" $ do
      case parseZshGlob "*.hs(.)" of
        Right zg -> do
          zg.globBasePattern `shouldBe` "*.hs"
          zg.globQualifiers `shouldBe` [QualRegularFile]
        Left err -> expectationFailure (show err)

    it "handles patterns without qualifiers" $ do
      case parseZshGlob "src/**/*.hs" of
        Right zg -> do
          zg.globBasePattern `shouldBe` "src/**/*.hs"
          zg.globQualifiers `shouldBe` []
        Left err -> expectationFailure (show err)

  describe "Qualifier Metadata Evaluation" $ do
    let regFile = FileInfo RegularFile 200 2
        dirFile = FileInfo Directory 4096 10
        symFile = FileInfo Symlink 12 1

    it "matches regular files and rejects directories under (.)" $ do
      evalGlobQualifiers [QualRegularFile] regFile `shouldBe` True
      evalGlobQualifiers [QualRegularFile] dirFile `shouldBe` False

    it "matches non-directories under (^/)" $ do
      evalGlobQualifiers [QualNegate QualDirectory] regFile `shouldBe` True
      evalGlobQualifiers [QualNegate QualDirectory] dirFile `shouldBe` False
      evalGlobQualifiers [QualNegate QualDirectory] symFile `shouldBe` True

    it "filters candidate file lists according to metadata" $ do
      let candidates =
            [ ("foo.hs", regFile)
            , ("bar_dir", dirFile)
            , ("baz_link", symFile)
            ]
          zg = ZshGlob "*.hs" [QualRegularFile]
      filterFilesByZshGlob zg candidates `shouldBe` ["foo.hs"]

  describe "Bytecode Compilation with Callouts" $ do
    it "compiles Zsh glob to executable regex" $ do
      case compileZshGlob "*.hs(.)" of
        Right reg -> testMatch reg ("Main.hs" :: Text) `shouldBe` True
        Left err  -> expectationFailure (show err)
