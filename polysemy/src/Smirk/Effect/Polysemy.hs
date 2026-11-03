{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Smirk.Effect.Polysemy
Description : Polysemy extensible effect interface for the Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Exposes Smirk's step-bounded, pure PCRE engine as an idiomatic 'Polysemy'
effect ('RegexEngine'), integrating seamlessly into 'Sem r' effect rows.

=== Example Usage

@
import Polysemy
import Smirk (re)
import Smirk.Effect.Polysemy

searchUser :: (Member RegexEngine r) => Text -> Sem r (Maybe Text)
searchUser input = do
  mMatch <- matchRegex [re|user:(\\w+)|] input
  pure (fmap matchedSlice mMatch)
@
-}
module Smirk.Effect.Polysemy
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
import Polysemy

import Smirk.Core.Bytecode (Regex)
import Smirk.Engine.VM (Fuel (..), MatchResult, MatchStatus (..), findAllMatches, runMatch)

-- | First-class Polysemy effect representing regular expression operations.
data RegexEngine m a where
  -- | Perform an unconstrained search returning the first match if found.
  MatchRegex     :: Regex -> Text -> RegexEngine m (Maybe (MatchResult Text))
  -- | Perform a step-budgeted search tracking consumed fuel and yielding continuations on exhaustion.
  MatchRegexFuel :: Regex -> Text -> Fuel -> RegexEngine m (MatchStatus Text)
  -- | Find all non-overlapping matches across the input text.
  FindAllRegex   :: Regex -> Text -> RegexEngine m [MatchResult Text]

makeSem ''RegexEngine

-- | Interpret 'RegexEngine' purely in memory with unlimited execution fuel.
-- Suitable for trusted regular expressions and inputs where ReDoS attacks are not a concern.
runRegexEnginePure :: Sem (RegexEngine : r) a -> Sem r a
runRegexEnginePure = interpret $ \case
  MatchRegex reg txt -> case runMatch reg txt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Interpret 'RegexEngine' enforcing a specific default step budget limit.
-- Guarantees ReDoS immunity by bounding execution iterations across untrusted inputs.
runRegexEngineWithFuel :: Fuel -> Sem (RegexEngine : r) a -> Sem r a
runRegexEngineWithFuel defaultFuel = interpret $ \case
  MatchRegex reg txt -> case runMatch reg txt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)
