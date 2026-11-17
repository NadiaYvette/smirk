{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Smirk.Effect.Effectful
Description : Effectful algebraic effect interface for the Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Exposes Smirk's step-bounded, pure PCRE engine as an idiomatic @Effectful@
effect ('RegexEngine'), providing dynamic dispatch, unlifted pure interpretation,
fuel-bounded step execution, and concurrent shared-budget coordination.
-}
module Smirk.Effect.Effectful
  ( -- * Effect Definition
    RegexEngine (..)
  , matchRegex
  , matchRegexFuel
  , findAllRegex

    -- * Interpreters
  , runRegexEnginePure
  , runRegexEngineWithFuel
  , runRegexEngineSharedBudget
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
import Data.Kind (Type)
import Effectful hiding (Unlimited)
import Effectful.Dispatch.Dynamic

import Smirk.Core.Budget (ResourceBudget (..), debitBudget)
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types (Fuel (..), MatchOutcome (..), MatchResult, MatchStatus (..))
import Smirk.Engine.Sequence (RegexTarget)
import Smirk.Engine.VM (findAllMatches, runMatch, runMatchWithMetrics)

-- | Clamp execution fuel against a maximum ceiling.
clampFuel :: Fuel -> Fuel -> Fuel
clampFuel Unlimited f = f
clampFuel f Unlimited = f
clampFuel (StepBudget a) (StepBudget b) = StepBudget (min a b)

-- | First-class algebraic effect representing regular expression operations over target sequence type @s@.
data RegexEngine (s :: Type) :: Effect where
  -- | Perform an unconstrained search returning the first match if found.
  MatchRegex     :: Regex -> s -> RegexEngine s m (Maybe (MatchResult s))
  -- | Perform a step-budgeted search tracking consumed fuel and yielding continuations on exhaustion.
  MatchRegexFuel :: Regex -> s -> Fuel -> RegexEngine s m (MatchStatus s)
  -- | Find all non-overlapping matches across the input target.
  FindAllRegex   :: Regex -> s -> RegexEngine s m [MatchResult s]

type instance DispatchOf (RegexEngine s) = 'Dynamic

-- | Perform an unconstrained search returning the first match if found.
matchRegex ::
  (RegexEngine s :> es) =>
  Regex
  -> s
  -> Eff es (Maybe (MatchResult s))
matchRegex reg tgt = send (MatchRegex reg tgt)

-- | Perform a step-budgeted search.
matchRegexFuel ::
  (RegexEngine s :> es) =>
  Regex
  -> s
  -> Fuel
  -> Eff es (MatchStatus s)
matchRegexFuel reg tgt fuel = send (MatchRegexFuel reg tgt fuel)

-- | Find all non-overlapping matches across the input target.
findAllRegex ::
  (RegexEngine s :> es) =>
  Regex
  -> s
  -> Eff es [MatchResult s]
findAllRegex reg tgt = send (FindAllRegex reg tgt)

-- | Interpret 'RegexEngine' purely in memory with unlimited execution fuel.
runRegexEnginePure ::
  RegexTarget s =>
  Eff (RegexEngine s : es) a
  -> Eff es a
runRegexEnginePure = interpret $ \_ -> \case
  MatchRegex reg tgt -> case runMatch reg tgt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg tgt fuel -> pure (runMatch reg tgt fuel)
  FindAllRegex reg tgt -> pure (findAllMatches reg tgt)

-- | Interpret 'RegexEngine' enforcing a specific default step budget limit.
runRegexEngineWithFuel ::
  RegexTarget s =>
  Fuel
  -> Eff (RegexEngine s : es) a
  -> Eff es a
runRegexEngineWithFuel defaultFuel = interpret $ \_ -> \case
  MatchRegex reg tgt -> case runMatch reg tgt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg tgt fuel ->
    let effFuel = clampFuel fuel defaultFuel
    in pure (runMatch reg tgt effFuel)
  FindAllRegex reg tgt -> pure (findAllMatches reg tgt)

-- | Interpret 'RegexEngine' coordinating multi-fiber matching under a shared concurrent thread-group token bucket.
runRegexEngineSharedBudget ::
  (RegexTarget s, IOE :> es) =>
  TVar ResourceBudget
  -> Eff (RegexEngine s : es) a
  -> Eff es a
runRegexEngineSharedBudget budgetVar = interpret $ \_ -> \case
  MatchRegex reg tgt -> do
    availSteps <- liftIO $ atomically $ do
      b <- readTVar budgetVar
      pure b.maxVmSteps
    let MatchOutcome st met = runMatchWithMetrics reg tgt (StepBudget (max 1 availSteps))
    liftIO $ atomically $ do
      modifyTVar' budgetVar (`debitBudget` met)
    case st of
      Matched res -> pure (Just res)
      _           -> pure Nothing
  MatchRegexFuel reg tgt fuel -> do
    effFuel <- liftIO $ atomically $ do
      b <- readTVar budgetVar
      let maxSteps = b.maxVmSteps
          req = case fuel of
            Unlimited    -> maxSteps
            StepBudget n -> min n maxSteps
      pure (StepBudget (max 1 req))
    let MatchOutcome st met = runMatchWithMetrics reg tgt effFuel
    liftIO $ atomically $ do
      modifyTVar' budgetVar (`debitBudget` met)
    pure st
  FindAllRegex reg tgt -> pure (findAllMatches reg tgt)
