{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Smirk.Effect.Penelope
Description : Penelope effect system interface for the Smirk PCRE engine
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Exposes Smirk's step-bounded, pure PCRE engine as an algebraic effect ('RegexEngine')
for the @Penelope@ effect system, providing zero-cost polycategorical dispatch over
flat Polytropos row vectors, unlifted pure interpretation, and fuel-bounded execution.

=== Example Usage

@
import Data.Text (Text)
import Penelope.Effects ('t:Eff')
import Smirk (re)
import Smirk.Effect.Penelope

searchUser :: (ConstructorIndex "regex" ef) => Text -> 't:Eff' eh ef (Maybe Text)
searchUser input = do
  mMatch <- matchRegex [re|user:(\\w+)|] input
  pure (fmap matchedSlice mMatch)
@
-}
module Smirk.Effect.Penelope
  ( -- * Effect Definition
    RegexEngine (..)

    -- * Effect Injection Operations
  , matchRegex
  , matchRegexAt
  , matchRegexFuel
  , matchRegexFuelAt
  , findAllRegex
  , findAllRegexAt

    -- * Handlers
  , interpretRegexPure
  , interpretRegexWithFuel

    -- * Pure and Monadic Runners
  , runRegexEnginePure
  , runRegexEngineWithFuel
  , runRegexEngineM
  , runRegexEngineWithFuelM
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Penelope.Effects
  ( Eff
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), Pair (..), emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex)
import Smirk.Core.Bytecode (Regex)
import Smirk.Engine.VM
  ( Fuel (..)
  , MatchResult
  , MatchStatus (..)
  , findAllMatches
  , runMatch
  )

-- | First-class algebraic effect representing regular expression operations.
data RegexEngine (a :: Type) where
  -- | Perform an unconstrained search returning the first match if found.
  MatchRegex     :: Regex -> Text -> RegexEngine (Maybe (MatchResult Text))
  -- | Perform a step-budgeted search tracking consumed fuel and yielding continuations on exhaustion.
  MatchRegexFuel :: Regex -> Text -> Fuel -> RegexEngine (MatchStatus Text)
  -- | Find all non-overlapping matches across the input text.
  FindAllRegex   :: Regex -> Text -> RegexEngine [MatchResult Text]

-- | Perform an unconstrained search at effect label @"regex"@ returning the first match if found.
{-# INLINE matchRegex #-}
matchRegex ::
  forall eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> Text
  -> Eff eh ef (Maybe (MatchResult Text))
matchRegex = matchRegexAt @"regex"

-- | Perform an unconstrained search at an explicit effect label returning the first match if found.
{-# INLINE matchRegexAt #-}
matchRegexAt ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  Regex
  -> Text
  -> Eff eh ef (Maybe (MatchResult Text))
matchRegexAt reg txt = send @l (MatchRegex reg txt)

-- | Perform a step-budgeted search at effect label @"regex"@.
{-# INLINE matchRegexFuel #-}
matchRegexFuel ::
  forall eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> Text
  -> Fuel
  -> Eff eh ef (MatchStatus Text)
matchRegexFuel = matchRegexFuelAt @"regex"

-- | Perform a step-budgeted search at an explicit effect label.
{-# INLINE matchRegexFuelAt #-}
matchRegexFuelAt ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  Regex
  -> Text
  -> Fuel
  -> Eff eh ef (MatchStatus Text)
matchRegexFuelAt reg txt fuel = send @l (MatchRegexFuel reg txt fuel)

-- | Find all non-overlapping matches at effect label @"regex"@ across the input text.
{-# INLINE findAllRegex #-}
findAllRegex ::
  forall eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> Text
  -> Eff eh ef [MatchResult Text]
findAllRegex = findAllRegexAt @"regex"

-- | Find all non-overlapping matches at an explicit effect label across the input text.
{-# INLINE findAllRegexAt #-}
findAllRegexAt ::
  forall (l :: Symbol) eh ef.
  ConstructorIndex l ef =>
  Regex
  -> Text
  -> Eff eh ef [MatchResult Text]
findAllRegexAt reg txt = send @l (FindAllRegex reg txt)

-- | Construct an 'EffHandler' interpreting 'RegexEngine' with unlimited execution fuel.
-- Suitable for trusted regular expressions and inputs where ReDoS attacks are not a concern.
{-# INLINE interpretRegexPure #-}
interpretRegexPure ::
  Monad m =>
  EffHandler m RegexEngine
interpretRegexPure = mkEffHandler $ \case
  MatchRegex reg txt -> case runMatch reg txt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Construct an 'EffHandler' interpreting 'RegexEngine' enforcing a specific default step budget limit.
-- Guarantees ReDoS immunity by bounding execution iterations across untrusted inputs.
{-# INLINE interpretRegexWithFuel #-}
interpretRegexWithFuel ::
  Monad m =>
  Fuel
  -> EffHandler m RegexEngine
interpretRegexWithFuel defaultFuel = mkEffHandler $ \case
  MatchRegex reg txt -> case runMatch reg txt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg txt fuel -> pure (runMatch reg txt fuel)
  FindAllRegex reg txt -> pure (findAllMatches reg txt)

-- | Execute a pure 't:Eff' computation with unlimited fuel using the @"regex"@ label.
{-# INLINE runRegexEnginePure #-}
runRegexEnginePure ::
  Eff '[] '["regex" ':= UnaryEffect RegexEngine] a
  -> a
runRegexEnginePure prog =
  let handlers = insertField (Field @"regex") interpretRegexPure emptyRecord
      Identity res = runEffM handlers prog
  in res

-- | Execute a pure 't:Eff' computation enforcing a step budget using the @"regex"@ label.
{-# INLINE runRegexEngineWithFuel #-}
runRegexEngineWithFuel ::
  Fuel
  -> Eff '[] '["regex" ':= UnaryEffect RegexEngine] a
  -> a
runRegexEngineWithFuel fuel prog =
  let handlers = insertField (Field @"regex") (interpretRegexWithFuel fuel) emptyRecord
      Identity res = runEffM handlers prog
  in res

-- | Execute an effectful 't:Eff' computation under a target 'Monad' using pure unlimited regex matching.
{-# INLINE runRegexEngineM #-}
runRegexEngineM ::
  Monad m =>
  Eff '[] '["regex" ':= UnaryEffect RegexEngine] a
  -> m a
runRegexEngineM prog =
  let handlers = insertField (Field @"regex") interpretRegexPure emptyRecord
  in runEffM handlers prog

-- | Execute an effectful 't:Eff' computation under a target 'Monad' enforcing a step budget.
{-# INLINE runRegexEngineWithFuelM #-}
runRegexEngineWithFuelM ::
  Monad m =>
  Fuel
  -> Eff '[] '["regex" ':= UnaryEffect RegexEngine] a
  -> m a
runRegexEngineWithFuelM fuel prog =
  let handlers = insertField (Field @"regex") (interpretRegexWithFuel fuel) emptyRecord
  in runEffM handlers prog
