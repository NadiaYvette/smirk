{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

-- | First-class algebraic effect for regular expression matching.
data RegexEngine :: Effect where
  MatchRegex     :: Regex -> Text -> RegexEngine m (Maybe (MatchResult Text))
  MatchRegexFuel :: Regex -> Text -> Fuel -> RegexEngine m (MatchStatus Text)
  FindAllRegex   :: Regex -> Text -> RegexEngine m [MatchResult Text]

makeEffect ''RegexEngine

-- | Interpret 'RegexEngine' purely with unlimited execution fuel.
runRegexEnginePure :: Eff (RegexEngine : es) a -> Eff es a
runRegexEnginePure = interpret $ \_ -> \case
  MatchRegex reg txt -> case runMatch reg txt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Interpret 'RegexEngine' enforcing a default step budget limit.
runRegexEngineWithFuel :: Fuel -> Eff (RegexEngine : es) a -> Eff es a
runRegexEngineWithFuel defaultFuel = interpret $ \_ -> \case
  MatchRegex reg txt -> case runMatch reg txt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)
