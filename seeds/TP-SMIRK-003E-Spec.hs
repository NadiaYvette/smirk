{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Test.Hspec

import qualified Data.Sketch.TDigest.Telemetry as TDT
import Data.Sketch.TDigest.Telemetry (TailQuantiles (..))
import Smirk
import Smirk.Effect.Penelope
import Smirk.Effect.Telemetry

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

  describe "Smirk.Effect.Telemetry (Streaming dunning-t-digest Quantile Profiler)" $ do
    it "records streaming observations and calculates accurate tail quantiles" $ do
      state <- newTelemetryState Nothing
      -- Feed synthetic step metrics across a range 10..1000
      forM_ [1 .. 100 :: Int] $ \i -> do
        let steps = i * 10
            bytes = 50
            met = (emptyStepMetrics Unlimited)
              { stepsConsumed = steps
              , utf8BytesMaterialized = bytes
              }
        _ <- recordStepMetrics state met
        pure ()
      (digests, _) <- snapshotTelemetry state
      case TDT.channelTailQuantiles TDT.stepsConsumed digests of
        Just tq -> do
          -- p50 of [10..1000] should be ~505
          (tq.p50 >= 450 && tq.p50 <= 560) `shouldBe` True
          -- p90 should be ~900
          (tq.p90 >= 850 && tq.p90 <= 950) `shouldBe` True
          -- p99 should be ~990
          (tq.p99 >= 940 && tq.p99 <= 1010) `shouldBe` True
        Nothing -> expectationFailure "Expected quantiles for stepsConsumed"

    it "triggers telemetry warnings on outlier ratio breach" $ do
      -- Set static threshold ceiling to 5.0 steps/byte
      state <- newTelemetryState (Just 5.0)
      let normalMet = (emptyStepMetrics Unlimited)
            { stepsConsumed = 20
            , utf8BytesMaterialized = 10
            }
          outlierMet = (emptyStepMetrics Unlimited)
            { stepsConsumed = 100
            , utf8BytesMaterialized = 10
            }
      mWarn1 <- recordStepMetrics state normalMet
      mWarn1 `shouldBe` Nothing

      mWarn2 <- recordStepMetrics state outlierMet
      case mWarn2 of
        Just warn -> do
          warn.warningChannel `shouldBe` "stepsPerByte"
          warn.observedRatio `shouldBe` 10.0
          warn.thresholdP999 `shouldBe` 5.0
        Nothing -> expectationFailure "Expected warning on outlier breach"

      (_, allWarns) <- snapshotTelemetry state
      length allWarns `shouldBe` 1

    it "runs Penelope computations with streaming telemetry via runRegexEngineTelemetry" $ do
      case mReg of
        Right reg -> do
          state <- newTelemetryState Nothing
          mRes <- runRegexEngineTelemetry @Text state (StepBudget 10000) $ do
            matchRegex reg target
          case mRes of
            Just res -> res.matchedSlice `shouldBe` "timeout=500"
            Nothing  -> expectationFailure "Expected match under telemetry runner"
          (digests, _) <- snapshotTelemetry state
          case TDT.channelTailQuantiles TDT.stepsConsumed digests of
            Just tq -> (tq.p50 > 0) `shouldBe` True
            Nothing -> expectationFailure "Expected non-empty telemetry digest"
        Left err -> expectationFailure (show err)
