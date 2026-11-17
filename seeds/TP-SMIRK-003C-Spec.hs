{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Effectful as Eff
import Test.Hspec

import Smirk
import Smirk.Core.Budget (ResourceBudget (..), budgetEmpty)
import qualified Smirk.Effect.Effectful as SmirkEff

main :: IO ()
main = hspec $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms max=2000ms" :: Text)
      bsTarget = ("config: timeout=500ms max=2000ms" :: BSC.ByteString)

  describe "Smirk.Effect.Effectful (Effectful RegexEngine Interpreter)" $ do
    it "runs pure regex matching in Eff" $ do
      case mReg of
        Right reg -> do
          mRes <- Eff.runEff $ SmirkEff.runRegexEnginePure @Text $ do
            SmirkEff.matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match in Eff"
        Left err -> expectationFailure (show err)

    it "runs fuel-bounded matching in Eff" $ do
      case mReg of
        Right reg -> do
          status <- Eff.runEff $ SmirkEff.runRegexEnginePure @Text $ do
            SmirkEff.matchRegexFuel reg target (StepBudget 1000)
          case status of
            Matched res -> res.matchedSlice `shouldBe` "timeout=500"
            _           -> expectationFailure "Expected Matched status"
        Left err -> expectationFailure (show err)

    it "finds all matches in Eff" $ do
      case mReg of
        Right reg -> do
          matches <- Eff.runEff $ SmirkEff.runRegexEnginePure @Text $ do
            SmirkEff.findAllRegex reg target
          map (\m -> m.matchedSlice) matches `shouldBe` ["timeout=500", "max=2000"]
        Left err -> expectationFailure (show err)

    it "enforces fuel clamping against default fuel ceiling" $ do
      case mReg of
        Right reg -> do
          status <- Eff.runEff $ SmirkEff.runRegexEngineWithFuel @Text (StepBudget 5) $ do
            SmirkEff.matchRegexFuel reg target (StepBudget 100000)
          case status of
            OutOfFuel _ -> pure ()
            _           -> expectationFailure "Expected OutOfFuel status due to clamped fuel"
        Left err -> expectationFailure (show err)

    it "supports sequence polymorphism over ByteString" $ do
      case mReg of
        Right reg -> do
          mRes <- Eff.runEff $ SmirkEff.runRegexEnginePure @BSC.ByteString $ do
            SmirkEff.matchRegex reg bsTarget
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match with ByteString target"
        Left err -> expectationFailure (show err)

    it "coordinates concurrent matching under shared TVar ResourceBudget" $ do
      case mReg of
        Right reg -> do
          let initialBudget = budgetEmpty { maxVmSteps = 50000 }
          budgetVar <- newTVarIO initialBudget
          _ <- replicateConcurrently 4 $ do
            Eff.runEff $ SmirkEff.runRegexEngineSharedBudget @Text budgetVar $ do
              SmirkEff.matchRegex reg target
          finalBudget <- readTVarIO budgetVar
          -- Verify steps were deducted from shared budget
          (finalBudget.maxVmSteps < 50000) `shouldBe` True
        Left err -> expectationFailure (show err)
