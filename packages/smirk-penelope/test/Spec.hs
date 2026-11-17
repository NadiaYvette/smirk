{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Test.Hspec

import Smirk
import Smirk.Effect.Penelope

main :: IO ()
main = hspec $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms max=2000ms" :: Text)
      bsTarget = ("config: timeout=500ms max=2000ms" :: BSC.ByteString)

  describe "Smirk.Effect.Penelope (Penelope RegexEngine Interpreter)" $ do
    it "runs pure regex matching in Eff" $ do
      case mReg of
        Right reg -> do
          let mRes = runRegexEnginePure @Text $ do
                matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match in Penelope Eff"
        Left err -> expectationFailure (show err)

    it "runs fuel-bounded matching in Eff" $ do
      case mReg of
        Right reg -> do
          let status = runRegexEnginePure @Text $ do
                matchRegexFuel reg target (StepBudget 1000)
          case status of
            Matched res -> res.matchedSlice `shouldBe` "timeout=500"
            _           -> expectationFailure "Expected Matched status"
        Left err -> expectationFailure (show err)

    it "finds all matches in Eff" $ do
      case mReg of
        Right reg -> do
          let matches = runRegexEnginePure @Text $ do
                findAllRegex reg target
          map (\m -> m.matchedSlice) matches `shouldBe` ["timeout=500", "max=2000"]
        Left err -> expectationFailure (show err)

    it "runs with default fuel runner" $ do
      case mReg of
        Right reg -> do
          let mRes = runRegexEngineWithFuel @Text (StepBudget 5000) $ do
                matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match under fuel budget"
        Left err -> expectationFailure (show err)

    it "enforces fuel clamping against default fuel ceiling" $ do
      case mReg of
        Right reg -> do
          let status = runRegexEngineWithFuel @Text (StepBudget 5) $ do
                matchRegexFuel reg target (StepBudget 100000)
          case status of
            OutOfFuel _ -> pure ()
            _           -> expectationFailure "Expected OutOfFuel status due to clamped fuel"
        Left err -> expectationFailure (show err)

    it "supports sequence polymorphism over ByteString" $ do
      case mReg of
        Right reg -> do
          let mRes = runRegexEnginePure @BSC.ByteString $ do
                matchRegex reg bsTarget
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match with ByteString target"
        Left err -> expectationFailure (show err)

    it "verifies runRegexEnginePenelope equivalence with runRegexEnginePure" $ do
      case mReg of
        Right reg -> do
          let pureRes = runRegexEnginePure @Text (matchRegex reg target)
              facadeRes = runRegexEnginePenelope (matchRegex reg target)
          facadeRes `shouldBe` pureRes
        Left err -> expectationFailure (show err)

    it "runs under IO monad via runRegexEngineM" $ do
      case mReg of
        Right reg -> do
          mRes <- runRegexEngineM @Text $ do
            matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match in IO Eff"
        Left err -> expectationFailure (show err)

    it "runs under IO monad with fuel via runRegexEngineWithFuelM" $ do
      case mReg of
        Right reg -> do
          mRes <- runRegexEngineWithFuelM @Text (StepBudget 2000) $ do
            matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match in IO Eff with fuel"
        Left err -> expectationFailure (show err)
