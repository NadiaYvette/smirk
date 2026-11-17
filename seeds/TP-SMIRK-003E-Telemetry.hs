{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Smirk.Effect.Telemetry
Description : Streaming dunning-t-digest telemetry middleware for Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Lightweight telemetry middleware integrating @dunning-t-digest@ online quantile
sketches into Smirk's effect interpreters. Tracks streaming execution histograms
(p50, p90, p95, p99, p99.9) for steps consumed, steps per byte, materialized byte
widths, and backtrack depths without unbounded sample retention.
-}
module Smirk.Effect.Telemetry
  ( -- * Telemetry Types
    TelemetryState (..)
  , TelemetryWarning (..)
  , newTelemetryState
  , snapshotTelemetry
  , resetTelemetry

    -- * Metrics Recording
  , recordStepMetrics

    -- * Composable Middleware
  , withRegexTelemetry
  , withRegexTelemetryOp
  , interpretRegexTelemetry
  , runRegexEngineTelemetry
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import Penelope.Effects (Eff, EffHandler, UnaryEffect, mkEffHandler, runEffM)
import Polytropos (Field (..), Pair (..), emptyRecord, insertField)

import qualified Data.Sketch.TDigest.Telemetry as TDT
import Data.Sketch.TDigest.Telemetry
  ( DigestRecord
  , SmirkTelemetryRow
  , TailQuantiles (..)
  , emptySmirkTelemetry
  )
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types (Fuel (..), MatchOutcome (..), MatchStatus (..), StepMetrics (..))
import Smirk.Effect.Penelope (RegexEngine (..))
import Smirk.Engine.Sequence (RegexTarget)
import Smirk.Engine.VM (findAllMatches, runMatchWithMetrics)

-- | Warning emitted when a match operation's metrics breach statistical outlier thresholds.
data TelemetryWarning =
    ThresholdBreachWarning
      { warningChannel      :: !String
      , observedRatio       :: !Double
      , thresholdP999       :: !Double
      , observedSteps       :: !Int
      , observedBytes       :: !Int
      }
  deriving stock (Eq, Show, Generic)

-- | Thread-safe streaming telemetry state holding the t-digest multi-channel sketch and warnings.
data TelemetryState = TelemetryState
  { telemetryDigests     :: !(IORef (DigestRecord SmirkTelemetryRow))
  , telemetryWarnings    :: !(IORef [TelemetryWarning])
  , p999ThresholdCeiling :: !(Maybe Double)
  }

-- | Clamp execution fuel against a maximum ceiling.
clampFuel :: Fuel -> Fuel -> Fuel
clampFuel Unlimited f = f
clampFuel f Unlimited = f
clampFuel (StepBudget a) (StepBudget b) = StepBudget (min a b)

-- | Initialize a fresh streaming telemetry state with an optional static outlier ceiling threshold.
newTelemetryState :: Maybe Double -> IO TelemetryState
newTelemetryState mCeiling = do
  digestsRef <- newIORef emptySmirkTelemetry
  warnsRef <- newIORef []
  pure TelemetryState
    { telemetryDigests     = digestsRef
    , telemetryWarnings    = warnsRef
    , p999ThresholdCeiling = mCeiling
    }

-- | Record 'StepMetrics' into the streaming t-digest sketch and check for threshold breaches.
recordStepMetrics :: TelemetryState -> StepMetrics -> IO (Maybe TelemetryWarning)
recordStepMetrics state met = do
  let !cSteps = fromIntegral met.stepsConsumed :: Double
      !cBytes = fromIntegral met.utf8BytesMaterialized :: Double
      !cDepth = fromIntegral met.peakBacktrackDepth :: Double
      !cRatio = if cBytes > 0 then cSteps / cBytes else cSteps

  -- Check existing p99.9 threshold from sketch before adding this observation
  curDigest <- readIORef state.telemetryDigests
  let mBreach = case state.p999ThresholdCeiling of
        Just ceilingVal | cRatio > ceilingVal ->
          Just (ThresholdBreachWarning "stepsPerByte" cRatio ceilingVal met.stepsConsumed met.utf8BytesMaterialized)
        _ -> case TDT.channelTailQuantiles TDT.stepsPerByte curDigest of
          Just tq | tq.p999 > 0 && cRatio > tq.p999 ->
            Just (ThresholdBreachWarning "stepsPerByte" cRatio tq.p999 met.stepsConsumed met.utf8BytesMaterialized)
          _ -> Nothing

  case mBreach of
    Just warn -> atomicModifyIORef' state.telemetryWarnings (\ws -> (warn : ws, ()))
    Nothing   -> pure ()

  -- Stream observation into the t-digest sketch
  let !newDigest =
        TDT.observe TDT.stepsConsumed cSteps .
        TDT.observe TDT.stepsPerByte cRatio .
        TDT.observe TDT.utf8ByteWidth cBytes .
        TDT.observe TDT.peakBacktrackDepth cDepth $
        curDigest
  atomicModifyIORef' state.telemetryDigests (const (newDigest, ()))
  pure mBreach

-- | Read the current digest record snapshot and warnings list.
snapshotTelemetry :: TelemetryState -> IO (DigestRecord SmirkTelemetryRow, [TelemetryWarning])
snapshotTelemetry state = do
  d <- readIORef state.telemetryDigests
  w <- readIORef state.telemetryWarnings
  pure (d, reverse w)

-- | Reset the telemetry state to initial empty sketches.
resetTelemetry :: TelemetryState -> IO ()
resetTelemetry state = do
  writeIORef state.telemetryDigests emptySmirkTelemetry
  writeIORef state.telemetryWarnings []

-- | Composable telemetry middleware wrapping regex execution with streaming t-digest tracking.
withRegexTelemetry ::
  TelemetryState
  -> (Regex -> s -> Fuel -> MatchOutcome s)
  -> Regex
  -> s
  -> Fuel
  -> IO (MatchOutcome s)
withRegexTelemetry state runner reg tgt fuel = do
  let outcome@(MatchOutcome _ met) = runner reg tgt fuel
  _ <- recordStepMetrics state met
  pure outcome

-- | Dispatch a single 'RegexEngine' operation with streaming telemetry tracking.
withRegexTelemetryOp ::
  RegexTarget s =>
  TelemetryState
  -> Fuel
  -> RegexEngine s a
  -> IO a
withRegexTelemetryOp state defaultFuel = \case
  MatchRegex reg tgt -> do
    let MatchOutcome st met = runMatchWithMetrics reg tgt defaultFuel
    _ <- recordStepMetrics state met
    pure $ case st of
      Matched res -> Just res
      _           -> Nothing
  MatchRegexFuel reg tgt fuel -> do
    let effFuel = clampFuel fuel defaultFuel
        MatchOutcome st met = runMatchWithMetrics reg tgt effFuel
    _ <- recordStepMetrics state met
    pure st
  FindAllRegex reg tgt -> do
    let MatchOutcome _ met = runMatchWithMetrics reg tgt defaultFuel
    _ <- recordStepMetrics state met
    pure (findAllMatches reg tgt)

-- | Construct a Penelope 'EffHandler' that records streaming telemetry sketches for every operation.
interpretRegexTelemetry ::
  RegexTarget s =>
  TelemetryState
  -> Fuel
  -> EffHandler IO (RegexEngine s)
interpretRegexTelemetry state defaultFuel = mkEffHandler (withRegexTelemetryOp state defaultFuel)

-- | Run a Penelope computation with streaming telemetry instrumentation under the @"regex"@ label.
runRegexEngineTelemetry ::
  RegexTarget s =>
  TelemetryState
  -> Fuel
  -> Eff '[] '["regex" ':= UnaryEffect (RegexEngine s)] a
  -> IO a
runRegexEngineTelemetry state defaultFuel prog = do
  let handlers = insertField (Field @"regex") (interpretRegexTelemetry state defaultFuel) emptyRecord
  runEffM handlers prog
