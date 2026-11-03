{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GlobSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Glob" $ do
  describe "Filesystem Globs (compileGlob)" $ do
    it "matches single level wildcards with *" $ do
      case compileGlob "src/*.hs" of
        Right g -> do
          matchGlob g ("src/Main.hs" :: Text) `shouldBe` True
          matchGlob g ("src/Smirk/Core.hs" :: Text) `shouldBe` False
        Left err -> expectationFailure (show err)

    it "matches recursive directories with **" $ do
      case compileGlob "src/**/*.hs" of
        Right g -> do
          matchGlob g ("src/Main.hs" :: Text) `shouldBe` True
          matchGlob g ("src/Smirk/Core/AST.hs" :: Text) `shouldBe` True
          matchGlob g ("test/Spec.hs" :: Text) `shouldBe` False
        Left err -> expectationFailure (show err)

    it "matches single character with ?" $ do
      case compileGlob "file?.txt" of
        Right g -> do
          matchGlob g ("file1.txt" :: Text) `shouldBe` True
          matchGlob g ("fileA.txt" :: Text) `shouldBe` True
          matchGlob g ("file10.txt" :: Text) `shouldBe` False
        Left err -> expectationFailure (show err)

    it "matches brace alternatives" $ do
      case compileGlob "{src,test}/**/*.hs" of
        Right g -> do
          matchGlob g ("src/Smirk.hs" :: Text) `shouldBe` True
          matchGlob g ("src/Smirk/Core/AST.hs" :: Text) `shouldBe` True
          matchGlob g ("test/Spec.hs" :: Text) `shouldBe` True
          matchGlob g ("app/Main.hs" :: Text) `shouldBe` False
        Left err -> expectationFailure (show err)

    it "matches character ranges and negation" $ do
      case compileGlob "doc-[0-9].[!a-z]" of
        Right g -> do
          matchGlob g ("doc-1.1" :: Text) `shouldBe` True
          matchGlob g ("doc-1.a" :: Text) `shouldBe` False
        Left err -> expectationFailure (show err)

  describe "Unconstrained Wildcards (compileWildcard)" $ do
    it "matches general tool names and namespaces across any delimiter" $ do
      case (compileWildcard "mcp__*", compileWildcard "git_*", compileWildcard "fs:*") of
        (Right wMcp, Right wGit, Right wColon) -> do
          matchGlob wMcp ("mcp__github__search_repos" :: Text) `shouldBe` True
          matchGlob wMcp ("local_tool" :: Text) `shouldBe` False
          matchGlob wGit ("git_commit" :: Text) `shouldBe` True
          matchGlob wColon ("fs:read_file" :: Text) `shouldBe` True
        _ -> expectationFailure "Failed to compile wildcards"

  describe "[glob|...|] Compile-Time QuasiQuoter" $ do
    it "validates and compiles glob pattern at compile time" $ do
      let g = [glob|src/**/*.hs|]
      matchGlob g ("src/Smirk/Glob.hs" :: Text) `shouldBe` True
      matchGlob g ("src/Smirk/Glob.hs" :: String) `shouldBe` True
      matchGlob g ("src/Smirk/Glob.hs" :: BS.ByteString) `shouldBe` True
      matchGlob g ("doc/README.md" :: Text) `shouldBe` False
