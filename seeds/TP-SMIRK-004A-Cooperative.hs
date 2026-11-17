{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Continuation.Cooperative
Description : Time-sliced quantum auto-resumption and coroutine interleaving driver
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental

Provides cooperative, time-sliced regex matching that divides a total fuel
budget into small quantum slices (e.g. 500 VM steps), automatically invoking
'resumeMatchWithMetrics' across 'OutOfFuel' boundaries while yielding the GHC
thread ('yield') and checking cancellation tokens.
-}
module Smirk.Continuation.Cooperative
  ( -- * Cancellation Tokens
    CancellationToken
  , newCancellationToken
  , cancelToken
  , isTokenCancelled

    -- * Configuration
  , CooperativeConfig (..)
  , defaultCooperativeConfig

    -- * Outcomes
  , CooperativeOutcome (..)

    -- * Cooperative Match Drivers
  , runCooperativeMatch
  , runCooperativeMatchWithMetrics
  , runCooperativeFindAll
  ) where

import Control.Concurrent (yield)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Monad (when)
import GHC.Generics (Generic)

import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types
  ( Fuel (..)
  , MatchResult (..)
  , StepMetrics (..)
  , emptyStepMetrics
  )
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM
  ( CalloutDecision (..)
  , MatchContinuation (..)
  , MatchOutcome (..)
  , MatchStatus (..)
  , resumeMatchWithMetrics
  , runMatchWithMetrics
  )

-- | Atomic cancellation token for interruptible regex execution.
newtype CancellationToken = CancellationToken (TVar Bool)
  deriving (Eq)

-- | Create a fresh uncancelled token.
newCancellationToken :: IO CancellationToken
newCancellationToken = CancellationToken <$> newTVarIO False

-- | Request cancellation through the token.
cancelToken :: CancellationToken -> IO ()
cancelToken (CancellationToken var) = atomically (writeTVar var True)

-- | Check whether cancellation has been requested.
isTokenCancelled :: CancellationToken -> IO Bool
isTokenCancelled (CancellationToken var) = readTVarIO var

-- | Configuration parameters for cooperative time-sliced matching.
data CooperativeConfig = CooperativeConfig
  { ccQuantum           :: !Int                   -- ^ Number of VM steps per quantum slice (default: 500)
  , ccTotalBudget       :: !Fuel                  -- ^ Total step budget across all slices (default: Unlimited)
  , ccCancellationToken :: !(Maybe CancellationToken) -- ^ Optional cancellation token
  , ccYieldOnSlice      :: !Bool                  -- ^ Whether to invoke 'yield' between slices
  } deriving stock (Generic)

-- | Default cooperative configuration with 500-step quantum and cooperative yielding enabled.
defaultCooperativeConfig :: CooperativeConfig
defaultCooperativeConfig = CooperativeConfig
  { ccQuantum           = 500
  , ccTotalBudget       = Unlimited
  , ccCancellationToken = Nothing
  , ccYieldOnSlice      = True
  }

-- | Update the stepsConsumed field of StepMetrics unambiguously.
updateStepsConsumed :: StepMetrics -> Int -> StepMetrics
updateStepsConsumed m s = StepMetrics
  { stepsConsumed         = s
  , fuelRemaining         = m.fuelRemaining
  , unitsScanned          = m.unitsScanned
  , utf8BytesMaterialized = m.utf8BytesMaterialized
  , peakBacktrackDepth    = m.peakBacktrackDepth
  , activeThreadCount     = m.activeThreadCount
  }

-- | Outcome of cooperative matching.
data CooperativeOutcome s
  = CoopMatched !(MatchResult s)
  | CoopNoMatch
  | CoopCancelled !(MatchContinuation s)
  | CoopOutOfFuel !(MatchContinuation s)
  deriving stock (Show, Eq, Generic)

-- | Execute a regex match cooperatively with automatic quantum resumption.
runCooperativeMatch
  :: RegexTarget s
  => CooperativeConfig
  -> Regex
  -> s
  -> IO (CooperativeOutcome s)
runCooperativeMatch cfg regex target = fst <$> runCooperativeMatchWithMetrics cfg regex target

-- | Execute a regex match cooperatively returning cumulative 'StepMetrics'.
runCooperativeMatchWithMetrics
  :: RegexTarget s
  => CooperativeConfig
  -> Regex
  -> s
  -> IO (CooperativeOutcome s, StepMetrics)
runCooperativeMatchWithMetrics cfg regex target = do
  let initialFuel = computeBudget 0
  let outcome0 = runMatchWithMetrics regex target initialFuel
  loop outcome0
  where
    computeBudget currentSteps =
      case cfg.ccTotalBudget of
        Unlimited -> StepBudget (currentSteps + cfg.ccQuantum)
        StepBudget b -> StepBudget (min (currentSteps + cfg.ccQuantum) b)

    loop outcome = do
      let steps = outcome.outcomeMetrics.stepsConsumed
      case outcome.outcomeStatus of
        Matched res -> pure (CoopMatched res, outcome.outcomeMetrics)
        NoMatch        -> pure (CoopNoMatch, outcome.outcomeMetrics)
        OutOfFuel cont -> do
          cancelled <- case cfg.ccCancellationToken of
            Just tok -> isTokenCancelled tok
            Nothing  -> pure False
          if cancelled
            then pure (CoopCancelled cont, outcome.outcomeMetrics)
            else case cfg.ccTotalBudget of
              StepBudget b | steps >= b ->
                pure (CoopOutOfFuel cont, outcome.outcomeMetrics)
              _ -> do
                let nextFuel = computeBudget steps
                case nextFuel of
                  StepBudget b | b <= steps ->
                    pure (CoopOutOfFuel cont, outcome.outcomeMetrics)
                  _ -> do
                    when cfg.ccYieldOnSlice yield
                    let nextOutcome = resumeMatchWithMetrics cont nextFuel
                    loop nextOutcome
        SuspendedCallout _ resume -> do
          let nextCont = resume ProceedMatch
          let nextFuel = computeBudget steps
          let nextOutcome = resumeMatchWithMetrics nextCont nextFuel
          loop nextOutcome

-- | Find all occurrences cooperatively across the target sequence.
runCooperativeFindAll
  :: forall s. RegexTarget s
  => CooperativeConfig
  -> Regex
  -> s
  -> IO ([MatchResult s], StepMetrics)
runCooperativeFindAll cfg regex target = go 0 [] 0 (emptyStepMetrics cfg.ccTotalBudget)
  where
    targetLen = seqLength target
    go !startIdx !acc !totalSteps !lastMetrics
      | startIdx > targetLen = pure (reverse acc, updateStepsConsumed lastMetrics totalSteps)
      | otherwise = do
          (outcome, metrics) <- runCooperativeMatchWithMetrics cfg regex (seqSlice startIdx (targetLen - startIdx) target)
          let newTotal = totalSteps + metrics.stepsConsumed
              combMetrics = updateStepsConsumed metrics newTotal
          case outcome of
            CoopMatched res -> do
              let adjRes = res
                    { matchOffset = res.matchOffset + startIdx
                    }
                  nextStart = if res.matchLength > 0
                                then startIdx + res.matchOffset + res.matchLength
                                else startIdx + 1
              go nextStart (adjRes : acc) newTotal combMetrics
            _ -> pure (reverse acc, combMetrics)
