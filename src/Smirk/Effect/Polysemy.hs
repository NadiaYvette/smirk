{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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

-- | Polysemy effect for regular expression matching.
data RegexEngine m a where
  MatchRegex     :: Regex -> Text -> RegexEngine m (Maybe (MatchResult Text))
  MatchRegexFuel :: Regex -> Text -> Fuel -> RegexEngine m (MatchStatus Text)
  FindAllRegex   :: Regex -> Text -> RegexEngine m [MatchResult Text]

makeSem ''RegexEngine

-- | Pure Polysemy interpreter with unlimited fuel.
runRegexEnginePure :: Sem (RegexEngine : r) a -> Sem r a
runRegexEnginePure = interpret $ \case
  MatchRegex reg txt -> case runMatch reg txt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Polysemy interpreter with default fuel budget.
runRegexEngineWithFuel :: Fuel -> Sem (RegexEngine : r) a -> Sem r a
runRegexEngineWithFuel defaultFuel = interpret $ \case
  MatchRegex reg txt -> case runMatch reg txt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)
