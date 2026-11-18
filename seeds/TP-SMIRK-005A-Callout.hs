{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Smirk.Effect.Callout
Description : Host-effectful PCRE callout dispatchers and lexical disambiguation
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides host-effectful PCRE callout interception, continuation resumption,
and semantic lexical-disambiguation combinators across effect libraries.

When the Smirk Pike VM encounters a @(?C)@ or @(?C"tag")@ callout instruction,
it yields a 'SuspendedCallout' containing a 'CalloutPayload' snapshot and a
continuation function waiting for a 'CalloutDecision'. This module provides
first-class effectful bridges for resolving callouts inside host monads and
the Penelope algebraic effect stack.
-}
module Smirk.Effect.Callout
  ( -- * Core Callout Types & Decisions
    CalloutDecision (..)
  , CalloutPayload (..)

    -- * Dispatchers & Resumption
  , dispatchCallout
  , resumeFromCalloutDecision
  , runMatchWithCallouts

    -- * Lexical Disambiguation & Symbol-Table Combinators
  , lookupSymbolCallout
  , withSymbolTableCallout

    -- * Penelope Effect Integration
  , runMatchWithCalloutPenelope
  ) where

import Data.Text (Text)
import Penelope.Effects (Eff)
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types
  ( CalloutDecision (..)
  , CalloutPayload (..)
  , Fuel (..)
  , MatchContinuation
  , MatchOutcome (..)
  , MatchStatus (..)
  )
import Smirk.Engine.Sequence (RegexTarget)
import Smirk.Engine.VM (resumeMatchWithMetrics, runMatchWithMetrics)

-- | Dispatch an inspection payload to an arbitrary host effect handler.
{-# INLINE dispatchCallout #-}
dispatchCallout ::
  Monad m =>
  (CalloutPayload s -> m CalloutDecision)
  -- ^ Host effectful handler callback
  -> CalloutPayload s
  -- ^ Inspection snapshot from the Pike VM
  -> m CalloutDecision
dispatchCallout handler = handler

-- | Resume Pike VM execution from a callout decision using the suspended continuation.
{-# INLINE resumeFromCalloutDecision #-}
resumeFromCalloutDecision ::
  CalloutDecision
  -- ^ Inspection verdict from the host environment
  -> (CalloutDecision -> MatchContinuation s)
  -- ^ Suspended continuation awaiting the verdict
  -> Fuel
  -- ^ Execution fuel budget for the resumed execution
  -> MatchOutcome s
resumeFromCalloutDecision decision resumeCont =
  resumeMatchWithMetrics (resumeCont decision)

-- | Repeatedly resolve 'SuspendedCallout' hooks in a host monad until the regex engine terminates.
runMatchWithCallouts ::
  (Monad m, RegexTarget s) =>
  (CalloutPayload s -> m CalloutDecision)
  -- ^ Effectful callout evaluation action
  -> Regex
  -- ^ Compiled Smirk regex bytecode
  -> s
  -- ^ Target sequence being searched
  -> Fuel
  -- ^ Execution fuel budget
  -> m (MatchOutcome s)
runMatchWithCallouts handler reg input fuel =
  loop (runMatchWithMetrics reg input fuel)
  where
    loop outcome = case outcome.outcomeStatus of
      SuspendedCallout payload resumeCont -> do
        decision <- handler payload
        let nextOutcome = resumeMatchWithMetrics (resumeCont decision) fuel
        loop nextOutcome
      _ -> pure outcome

-- | Reference callout inspector verifying string tags against a scoped symbol-table predicate.
-- Rejects mismatching lexical categories by forcing Pike VM backtracking.
lookupSymbolCallout ::
  Monad m =>
  (Text -> m Bool)
  -- ^ Scoped symbol predicate (e.g. checking if a tag is recognized)
  -> CalloutPayload s
  -- ^ Callout inspection payload
  -> m CalloutDecision
lookupSymbolCallout predicate payload = case payload.calloutTag of
  Just tag -> do
    ok <- predicate tag
    pure (if ok then ProceedMatch else ForceBacktrack)
  Nothing -> pure ProceedMatch

-- | Disambiguate multi-branch lexical patterns using host symbol-table predicates.
withSymbolTableCallout ::
  (Monad m, RegexTarget s) =>
  (Text -> m Bool)
  -- ^ Symbol validation predicate
  -> Regex
  -- ^ Compiled PCRE pattern with @(?C\"tag\")@ annotations
  -> s
  -- ^ Target text sequence
  -> Fuel
  -- ^ Execution fuel budget
  -> m (MatchOutcome s)
withSymbolTableCallout checkSymbol =
  runMatchWithCallouts (lookupSymbolCallout checkSymbol)

-- | Execute regex matching with callouts inside the Penelope algebraic effect stack.
runMatchWithCalloutPenelope ::
  RegexTarget s =>
  (CalloutPayload s -> Eff eh ef CalloutDecision)
  -- ^ Penelope effectful handler callback
  -> Regex
  -- ^ Compiled Smirk regex bytecode
  -> s
  -- ^ Target sequence
  -> Fuel
  -- ^ Fuel budget
  -> Eff eh ef (MatchOutcome s)
runMatchWithCalloutPenelope = runMatchWithCallouts
