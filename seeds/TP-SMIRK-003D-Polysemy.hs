{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Smirk.Effect.Polysemy
Description : Polysemy extensible effect interface for the Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Exposes Smirk's step-bounded, pure PCRE engine as an idiomatic @Polysemy@
effect ('RegexEngine'), integrating seamlessly into 'Sem r' effect rows
with sequence polymorphism and scoped resource budgets.
-}
module Smirk.Effect.Polysemy
  ( -- * Effect Definition
    RegexEngine (..)
  , matchRegex
  , matchRegexFuel
  , findAllRegex
  , withResourceBudget

    -- * Interpreters
  , runRegexEnginePure
  , runRegexEngineWithFuel
  , runRegexEngineScopedBudget
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, writeTVar)
import Data.Kind (Type)
import Polysemy

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

-- | First-class Polysemy effect representing regular expression operations over target sequence type @s@.
data RegexEngine (s :: Type) (m :: Type -> Type) a where
  MatchRegex         :: Regex -> s -> RegexEngine s m (Maybe (MatchResult s))
  MatchRegexFuel     :: Regex -> s -> Fuel -> RegexEngine s m (MatchStatus s)
  FindAllRegex       :: Regex -> s -> RegexEngine s m [MatchResult s]
  WithResourceBudget :: ResourceBudget -> m a -> RegexEngine s m a

-- | Perform an unconstrained search returning the first match if found.
matchRegex ::
  (Member (RegexEngine s) r) =>
  Regex
  -> s
  -> Sem r (Maybe (MatchResult s))
matchRegex reg tgt = send (MatchRegex reg tgt)

-- | Perform a step-budgeted search.
matchRegexFuel ::
  (Member (RegexEngine s) r) =>
  Regex
  -> s
  -> Fuel
  -> Sem r (MatchStatus s)
matchRegexFuel reg tgt fuel = send (MatchRegexFuel reg tgt fuel)

-- | Find all non-overlapping matches across the input target.
findAllRegex ::
  (Member (RegexEngine s) r) =>
  Regex
  -> s
  -> Sem r [MatchResult s]
findAllRegex reg tgt = send (FindAllRegex reg tgt)

-- | Execute a scoped action under a carved child resource budget.
withResourceBudget ::
  forall s r a.
  (Member (RegexEngine s) r) =>
  ResourceBudget
  -> Sem r a
  -> Sem r a
withResourceBudget budget action = send @(RegexEngine s) (WithResourceBudget budget action)

-- | Interpret 'RegexEngine' purely in memory with unlimited execution fuel.
runRegexEnginePure ::
  RegexTarget s =>
  Sem (RegexEngine s : r) a
  -> Sem r a
runRegexEnginePure = interpretH $ \case
  MatchRegex reg tgt -> case runMatch reg tgt Unlimited of
    Matched res -> pureT (Just res)
    _           -> pureT Nothing
  MatchRegexFuel reg tgt fuel -> pureT (runMatch reg tgt fuel)
  FindAllRegex reg tgt -> pureT (findAllMatches reg tgt)
  WithResourceBudget _ m -> runTSimple m

-- | Interpret 'RegexEngine' enforcing a specific default step budget limit.
runRegexEngineWithFuel ::
  RegexTarget s =>
  Fuel
  -> Sem (RegexEngine s : r) a
  -> Sem r a
runRegexEngineWithFuel defaultFuel = interpretH $ \case
  MatchRegex reg tgt -> case runMatch reg tgt defaultFuel of
    Matched res -> pureT (Just res)
    _           -> pureT Nothing
  MatchRegexFuel reg tgt fuel ->
    let effFuel = clampFuel fuel defaultFuel
    in pureT (runMatch reg tgt effFuel)
  FindAllRegex reg tgt -> pureT (findAllMatches reg tgt)
  WithResourceBudget _ m -> runTSimple m

-- | Interpret 'RegexEngine' with scoped budget carving and automatic remainder refund on exit.
runRegexEngineScopedBudget ::
  (RegexTarget s, Member (Embed IO) r) =>
  TVar ResourceBudget
  -> Sem (RegexEngine s : r) a
  -> Sem r a
runRegexEngineScopedBudget budgetVar = interpretH $ \case
  MatchRegex reg tgt -> do
    availSteps <- embed $ atomically $ do
      b <- readTVar budgetVar
      pure b.maxVmSteps
    let MatchOutcome st met = runMatchWithMetrics reg tgt (StepBudget (max 1 availSteps))
    embed $ atomically $ do
      modifyTVar' budgetVar (`debitBudget` met)
    case st of
      Matched res -> pureT (Just res)
      _           -> pureT Nothing
  MatchRegexFuel reg tgt fuel -> do
    effFuel <- embed $ atomically $ do
      b <- readTVar budgetVar
      let maxSteps = b.maxVmSteps
          req = case fuel of
            Unlimited    -> maxSteps
            StepBudget n -> min n maxSteps
      pure (StepBudget (max 1 req))
    let MatchOutcome st met = runMatchWithMetrics reg tgt effFuel
    embed $ atomically $ do
      modifyTVar' budgetVar (`debitBudget` met)
    pureT st
  FindAllRegex reg tgt -> pureT (findAllMatches reg tgt)
  WithResourceBudget childBudget m -> do
    (parentOrig, carved) <- embed $ atomically $ do
      b <- readTVar budgetVar
      let c = min b.maxVmSteps childBudget.maxVmSteps
      writeTVar budgetVar (b { maxVmSteps = c })
      pure (b, c)
    res <- runTSimple m
    embed $ atomically $ do
      bAfter <- readTVar budgetVar
      let used = max 0 (carved - bAfter.maxVmSteps)
          restored = max 0 (parentOrig.maxVmSteps - used)
      writeTVar budgetVar (parentOrig { maxVmSteps = restored })
    pure res
