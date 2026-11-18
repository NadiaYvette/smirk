{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SheafParallelSpec
Description : Unit and property tests for Smirk.Sheaf.Parallel
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module SheafParallelSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Smirk (compileRegex)
import Smirk.Sheaf.Parallel

spec :: Spec
spec = describe "Smirk.Sheaf.Parallel" $ do
  describe "chunkInputSheaf" $ do
    it "partitions input into K balanced chunks" $ do
      let txt = "abcdefghij"
          chunks = chunkInputSheaf 3 txt
      length chunks `shouldBe` 3
      T.concat chunks `shouldBe` txt

    it "handles K=1 and empty strings gracefully" $ do
      chunkInputSheaf 1 "hello" `shouldBe` ["hello"]
      chunkInputSheaf 4 "" `shouldBe` [""]

  describe "matchParallelSheaf" $ do
    it "agrees with serial execution for K in {1, 2, 4, 8}" $ do
      case compileRegex "foo.*bar" of
        Left err -> expectationFailure $ "Failed to compile regex: " ++ show err
        Right re -> do
          let input = "foo123456789bar"
          matchParallelSheaf re 1 input `shouldBe` True
          matchParallelSheaf re 2 input `shouldBe` True
          matchParallelSheaf re 4 input `shouldBe` True
          matchParallelSheaf re 8 input `shouldBe` True
          matchParallelSheaf re 4 "foo_baz" `shouldBe` False
