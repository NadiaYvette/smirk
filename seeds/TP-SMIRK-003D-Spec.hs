{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Polysemy as Poly
import Test.Hspec

import Smirk
import Smirk.Core.Budget (ResourceBudget (..), budgetEmpty)
import qualified Smirk.Effect.Polysemy as SmirkPoly

main :: IO ()
main = hspec $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms max=2000ms" :: Text)
      bsTarget = ("config: timeout=500ms max=2000ms" :: BSC.ByteString)

  describe "Smirk.Effect.Polysemy (Polysemy RegexEngine Interpreter)" $ do
    it "runs pure regex matching in Sem" $ do
      case mReg of
        Right reg -> do
          let mRes = Poly.run $ SmirkPoly.runRegexEnginePure @Text $ do
                SmirkPoly.matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match in Sem"
        Left err -> expectationFailure (show err)

    it "runs fuel-bounded matching in Sem" $ do
      case mReg of
        Right reg -> do
          let status = Poly.run $ SmirkPoly.runRegexEnginePure @Text $ do
                SmirkPoly.matchRegexFuel reg target (StepBudget 1000)
          case status of
            Matched res -> res.matchedSlice `shouldBe` "timeout=500"
            _           -> expectationFailure "Expected Matched status"
        Left err -> expectationFailure (show err)

    it "finds all matches in Sem" $ do
      case mReg of
        Right reg -> do
          let matches = Poly.run $ SmirkPoly.runRegexEnginePure @Text $ do
                SmirkPoly.findAllRegex reg target
          map (\m -> m.matchedSlice) matches `shouldBe` ["timeout=500", "max=2000"]
        Left err -> expectationFailure (show err)

    it "enforces fuel clamping against default fuel ceiling" $ do
      case mReg of
        Right reg -> do
          let status = Poly.run $ SmirkPoly.runRegexEngineWithFuel @Text (StepBudget 5) $ do
                SmirkPoly.matchRegexFuel reg target (StepBudget 100000)
          case status of
            OutOfFuel _ -> pure ()
            _           -> expectationFailure "Expected OutOfFuel status due to clamped fuel"
        Left err -> expectationFailure (show err)

    it "supports sequence polymorphism over ByteString" $ do
      case mReg of
        Right reg -> do
          let mRes = Poly.run $ SmirkPoly.runRegexEnginePure @BSC.ByteString $ do
                SmirkPoly.matchRegex reg bsTarget
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match with ByteString target"
        Left err -> expectationFailure (show err)

    it "carves child budget and restores remainder on scope exit" $ do
      case mReg of
        Right reg -> do
          let initialBudget = budgetEmpty { maxVmSteps = 50000 }
          budgetVar <- newTVarIO initialBudget
          Poly.runM $ SmirkPoly.runRegexEngineScopedBudget @Text budgetVar $ do
            let childBudget = budgetEmpty { maxVmSteps = 5000 }
            SmirkPoly.withResourceBudget @Text childBudget $ do
              _ <- SmirkPoly.matchRegex reg target
              pure ()
          finalBudget <- readTVarIO budgetVar
          -- Verify initial budget was debited by actual match steps, and remaining child budget was refunded
          (finalBudget.maxVmSteps < 50000) `shouldBe` True
          (finalBudget.maxVmSteps > 40000) `shouldBe` True
        Left err -> expectationFailure (show err)
