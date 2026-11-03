{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Smirk.Effect.Effectful
Description : Effectful algebraic effect interface for the Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Exposes Smirk's step-bounded, pure PCRE engine as an idiomatic @Effectful@
effect ('RegexEngine'), providing dynamic dispatch, unlifted pure interpretation,
and fuel-bounded step execution.

=== Example Usage

@
import Effectful
import Smirk (re)
import Smirk.Effect.Effectful

searchUser :: (RegexEngine :> es) => Text -> Eff es (Maybe Text)
searchUser input = do
  mMatch <- matchRegex [re|user:(\\w+)|] input
  pure (fmap matchedSlice mMatch)
@
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
  ) where

import Data.Text (Text)
import Effectful hiding (Unlimited)
import Effectful.Dispatch.Dynamic
import Effectful.TH

import Smirk.Core.Bytecode (Regex)
import Smirk.Engine.VM (Fuel (..), MatchResult, MatchStatus (..), findAllMatches, runMatch)

-- | First-class algebraic effect representing regular expression operations.
data RegexEngine :: Effect where
  -- | Perform an unconstrained search returning the first match if found.
  MatchRegex     :: Regex -> Text -> RegexEngine m (Maybe (MatchResult Text))
  -- | Perform a step-budgeted search tracking consumed fuel and yielding continuations on exhaustion.
  MatchRegexFuel :: Regex -> Text -> Fuel -> RegexEngine m (MatchStatus Text)
  -- | Find all non-overlapping matches across the input text.
  FindAllRegex   :: Regex -> Text -> RegexEngine m [MatchResult Text]

makeEffect ''RegexEngine

-- | Interpret 'RegexEngine' purely in memory with unlimited execution fuel.
-- Suitable for trusted regular expressions and inputs where ReDoS attacks are not a concern.
runRegexEnginePure :: Eff (RegexEngine : es) a -> Eff es a
runRegexEnginePure = interpret $ \_ -> \case
  MatchRegex reg txt -> case runMatch reg txt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Interpret 'RegexEngine' enforcing a specific default step budget limit.
-- Guarantees ReDoS immunity by bounding execution iterations across untrusted inputs.
runRegexEngineWithFuel :: Fuel -> Eff (RegexEngine : es) a -> Eff es a
runRegexEngineWithFuel defaultFuel = interpret $ \_ -> \case
  MatchRegex reg txt -> case runMatch reg txt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)
