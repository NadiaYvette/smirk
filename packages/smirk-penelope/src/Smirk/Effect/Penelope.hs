{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
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
-}
module Smirk.Effect.Penelope
  ( -- * Effect Definition
    RegexEngine (..)
  , RegexPolyOp (..)
  , RegexScopedH (..)

    -- * Effect Injection Operations
  , matchRegex
  , matchRegexAt
  , matchRegexFuel
  , matchRegexFuelAt
  , findAllRegex
  , findAllRegexAt

    -- * Native Polycategorical Branching Operations
  , branchMatchPoly
  , branchMatchPolyAt

    -- * Scoped Operations
  , withResourceBudgetPoly
  , withResourceBudgetEff

    -- * Handlers
  , interpretRegexPure
  , interpretRegexWithFuel
  , interpretRegexPoly

    -- * Pure and Monadic Runners
  , runRegexEnginePure
  , runRegexEngineWithFuel
  , runRegexEngineM
  , runRegexEngineWithFuelM
  , runRegexEnginePenelope
  ) where

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Primitive.SmallArray (indexSmallArray)
import Data.Text (Text)
import GHC.Exts (Word (W#), unsafeCoerce#)
import GHC.TypeLits (Symbol)
import Penelope.Core.Engine.Interpret
  ( FrameKleisli (..)
  , Handler (..)
  , runFrameKleisli
  )
import Penelope.Core.Syntax
  ( BranchCont (..)
  , Penelope (..)
  , PortSig (..)
  )
import Penelope.Effects
  ( Eff
  , EffHandler
  , UnaryEffect
  , mkEffHandler
  , runEffM
  , send
  )
import Polytropos (Field (..), Pair (..), Record, RowOf, emptyRecord, insertField)
import Polytropos.Core (ConstructorIndex (..), Rep (..), RowRep (..))
import Polytropos.RowMorph (unsafeMkRowMorph)
import Smirk.Core.Budget
  ( BudgetHierarchy
  , ResourceBudget
  , withResourceBudget
  )
import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types
  ( CalloutDecision (..)
  , CalloutPayload (..)
  , Fuel (..)
  , MatchContinuation
  , MatchResult
  , MatchStatus (..)
  )
import Smirk.Engine.Sequence (RegexTarget)
import Smirk.Engine.VM
  ( findAllMatches
  , runMatch
  )

-- | Clamp execution fuel against a maximum ceiling.
clampFuel :: Fuel -> Fuel -> Fuel
clampFuel Unlimited f = f
clampFuel f Unlimited = f
clampFuel (StepBudget a) (StepBudget b) = StepBudget (min a b)

-- | First-class algebraic effect representing regular expression operations over target sequence type @s@.
data RegexEngine s (a :: Type) where
  -- | Perform an unconstrained search returning the first match if found.
  MatchRegex     :: Regex -> s -> RegexEngine s (Maybe (MatchResult s))
  -- | Perform a step-budgeted search tracking consumed fuel and yielding continuations on exhaustion.
  MatchRegexFuel :: Regex -> s -> Fuel -> RegexEngine s (MatchStatus s)
  -- | Find all non-overlapping matches across the input target.
  FindAllRegex   :: Regex -> s -> RegexEngine s [MatchResult s]

-- | Perform an unconstrained search at effect label @"regex"@ returning the first match if found.
{-# INLINE matchRegex #-}
matchRegex ::
  forall s eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> s
  -> Eff eh ef (Maybe (MatchResult s))
matchRegex = matchRegexAt @"regex"

-- | Perform an unconstrained search at an explicit effect label returning the first match if found.
{-# INLINE matchRegexAt #-}
matchRegexAt ::
  forall (l :: Symbol) s eh ef.
  ConstructorIndex l ef =>
  Regex
  -> s
  -> Eff eh ef (Maybe (MatchResult s))
matchRegexAt reg tgt = send @l (MatchRegex reg tgt)

-- | Perform a step-budgeted search at effect label @"regex"@.
{-# INLINE matchRegexFuel #-}
matchRegexFuel ::
  forall s eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> s
  -> Fuel
  -> Eff eh ef (MatchStatus s)
matchRegexFuel = matchRegexFuelAt @"regex"

-- | Perform a step-budgeted search at an explicit effect label.
{-# INLINE matchRegexFuelAt #-}
matchRegexFuelAt ::
  forall (l :: Symbol) s eh ef.
  ConstructorIndex l ef =>
  Regex
  -> s
  -> Fuel
  -> Eff eh ef (MatchStatus s)
matchRegexFuelAt reg tgt fuel = send @l (MatchRegexFuel reg tgt fuel)

-- | Find all non-overlapping matches at effect label @"regex"@ across the input target.
{-# INLINE findAllRegex #-}
findAllRegex ::
  forall s eh ef.
  ConstructorIndex "regex" ef =>
  Regex
  -> s
  -> Eff eh ef [MatchResult s]
findAllRegex = findAllRegexAt @"regex"

-- | Find all non-overlapping matches at an explicit effect label across the input target.
{-# INLINE findAllRegexAt #-}
findAllRegexAt ::
  forall (l :: Symbol) s eh ef.
  ConstructorIndex l ef =>
  Regex
  -> s
  -> Eff eh ef [MatchResult s]
findAllRegexAt reg tgt = send @l (FindAllRegex reg tgt)

-- | Native (2,n,4,0)-ary first-order polycategorical regex effect for 4-way direct branch dispatch.
data RegexPolyOp (s :: Type) (ins :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  BranchMatchPoly :: Regex -> s -> Fuel -> RegexPolyOp s '[]
    '[ "matched"   ':= '["result" ':= MatchResult s]
     , "noMatch"   ':= '[]
     , "outOfFuel" ':= '["continuation" ':= MatchContinuation s]
     , "callout"   ':= '["payload" ':= CalloutPayload s, "resume" ':= (CalloutDecision -> MatchContinuation s)]
     ]

-- | Native polycategorical branching match operation dispatched into a 4-field continuation record.
{-# INLINE branchMatchPolyAt #-}
branchMatchPolyAt ::
  forall (l :: Symbol) s exits eh ef outs.
  ConstructorIndex l ef =>
  Regex
  -> s
  -> Fuel
  -> Record exits
  -> Penelope eh ef '[] outs
branchMatchPolyAt reg tgt fuel conts =
  let !tag# = case constructorIndex @_ @l @ef of W# w# -> w#
      op = BranchMatchPoly reg tgt fuel
  in OpF tag# (unsafeMkRowMorph []) (unsafeCoerce# op) (unsafeCoerce# conts)

-- | Native polycategorical branching match operation dispatched under default @"regexPoly"@ label.
{-# INLINE branchMatchPoly #-}
branchMatchPoly ::
  forall s exits eh ef outs.
  ConstructorIndex "regexPoly" ef =>
  Regex
  -> s
  -> Fuel
  -> Record exits
  -> Penelope eh ef '[] outs
branchMatchPoly = branchMatchPolyAt @"regexPoly"

-- | Scoped higher-order operations for resource budgets and callout environments.
data RegexScopedH (r :: PortSig -> Type) (regs :: RowOf PortSig) (opIns :: RowOf Type) (exits :: RowOf (RowOf Type)) where
  WithResourceBudgetOp :: ResourceBudget -> RegexScopedH r
    '[ "body" ':= 'PortSig '[] '["val" ':= a] ]
    '[]
    '[ "ret"  ':= '["val" ':= a] ]
  WithCalloutEnvOp :: (forall s. CalloutPayload s -> IO CalloutDecision) -> RegexScopedH r
    '[ "body" ':= 'PortSig '[] '["val" ':= a] ]
    '[]
    '[ "ret"  ':= '["val" ':= a] ]

-- | Execute an action under a carved child budget, automatically refunding remaining budget.
{-# INLINE withResourceBudgetEff #-}
withResourceBudgetEff ::
  BudgetHierarchy
  -> ResourceBudget
  -> (BudgetHierarchy -> IO a)
  -> IO a
withResourceBudgetEff = withResourceBudget

-- | Execute an action under a carved child budget within Penelope polycategorical scopes.
{-# INLINE withResourceBudgetPoly #-}
withResourceBudgetPoly ::
  BudgetHierarchy
  -> ResourceBudget
  -> (BudgetHierarchy -> IO a)
  -> IO a
withResourceBudgetPoly = withResourceBudget

-- | Construct an 'EffHandler' interpreting 'RegexEngine' with unlimited execution fuel.
{-# INLINE interpretRegexPure #-}
interpretRegexPure ::
  (RegexTarget s, Monad m) =>
  EffHandler m (RegexEngine s)
interpretRegexPure = mkEffHandler $ \case
  MatchRegex reg tgt -> case runMatch reg tgt Unlimited of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg tgt fuel -> pure (runMatch reg tgt fuel)
  FindAllRegex reg tgt -> pure (findAllMatches reg tgt)

-- | Construct an 'EffHandler' interpreting 'RegexEngine' enforcing a specific default step budget limit.
-- Guarantees ReDoS immunity by bounding execution iterations across untrusted inputs.
{-# INLINE interpretRegexWithFuel #-}
interpretRegexWithFuel ::
  (RegexTarget s, Monad m) =>
  Fuel
  -> EffHandler m (RegexEngine s)
interpretRegexWithFuel defaultFuel = mkEffHandler $ \case
  MatchRegex reg tgt -> case runMatch reg tgt defaultFuel of
    Matched res -> pure (Just res)
    _           -> pure Nothing
  MatchRegexFuel reg tgt fuel ->
    let effFuel = clampFuel fuel defaultFuel
    in pure (runMatch reg tgt effFuel)
  FindAllRegex reg tgt -> pure (findAllMatches reg tgt)

-- | Native polycategorical interpreter dispatching 4-way exit branches directly into continuation records.
{-# INLINE interpretRegexPoly #-}
interpretRegexPoly ::
  forall s m ef.
  RegexTarget s =>
  Handler ef (FrameKleisli m) (RegexPolyOp s)
interpretRegexPoly = Handler $ \_inMap opAny exits -> FrameKleisli $ \inFrame -> do
  let !(BranchMatchPoly reg tgt fuel) = unsafeCoerce# opAny :: RegexPolyOp s '[] exits
      !(RowRep (Rep _ contArr)) = exits
  case runMatch reg tgt fuel of
    Matched res -> do
      let !(Identity contAny) = indexSmallArray contArr 0
          !(BranchCont k) = unsafeCoerce# contAny
          outRec = insertField (Field @"result") res emptyRecord
      runFrameKleisli (k (unsafeCoerce# outRec)) inFrame
    NoMatch -> do
      let !(Identity contAny) = indexSmallArray contArr 1
          !(BranchCont k) = unsafeCoerce# contAny
      runFrameKleisli (k (unsafeCoerce# emptyRecord)) inFrame
    OutOfFuel cont -> do
      let !(Identity contAny) = indexSmallArray contArr 2
          !(BranchCont k) = unsafeCoerce# contAny
          outRec = insertField (Field @"continuation") cont emptyRecord
      runFrameKleisli (k (unsafeCoerce# outRec)) inFrame
    SuspendedCallout payload resume -> do
      let !(Identity contAny) = indexSmallArray contArr 3
          !(BranchCont k) = unsafeCoerce# contAny
          outRec = insertField (Field @"payload") payload
                 $ insertField (Field @"resume") resume emptyRecord
      runFrameKleisli (k (unsafeCoerce# outRec)) inFrame

-- | Execute a pure 't:Eff' computation with unlimited fuel using the @"regex"@ label.
{-# INLINE runRegexEnginePure #-}
runRegexEnginePure ::
  RegexTarget s =>
  Eff '[] '["regex" ':= UnaryEffect (RegexEngine s)] a
  -> a
runRegexEnginePure prog =
  let handlers = insertField (Field @"regex") interpretRegexPure emptyRecord
      Identity res = runEffM handlers prog
  in res

-- | Execute a pure 't:Eff' computation enforcing a step budget using the @"regex"@ label.
{-# INLINE runRegexEngineWithFuel #-}
runRegexEngineWithFuel ::
  RegexTarget s =>
  Fuel
  -> Eff '[] '["regex" ':= UnaryEffect (RegexEngine s)] a
  -> a
runRegexEngineWithFuel fuel prog =
  let handlers = insertField (Field @"regex") (interpretRegexWithFuel fuel) emptyRecord
      Identity res = runEffM handlers prog
  in res

-- | Execute an effectful 't:Eff' computation under a target 'Monad' using pure unlimited regex matching.
{-# INLINE runRegexEngineM #-}
runRegexEngineM ::
  (RegexTarget s, Monad m) =>
  Eff '[] '["regex" ':= UnaryEffect (RegexEngine s)] a
  -> m a
runRegexEngineM prog =
  let handlers = insertField (Field @"regex") interpretRegexPure emptyRecord
  in runEffM handlers prog

-- | Execute an effectful 't:Eff' computation under a target 'Monad' enforcing a step budget.
{-# INLINE runRegexEngineWithFuelM #-}
runRegexEngineWithFuelM ::
  (RegexTarget s, Monad m) =>
  Fuel
  -> Eff '[] '["regex" ':= UnaryEffect (RegexEngine s)] a
  -> m a
runRegexEngineWithFuelM fuel prog =
  let handlers = insertField (Field @"regex") (interpretRegexWithFuel fuel) emptyRecord
  in runEffM handlers prog

-- | Unary backward-compatible wrapper delegating to the poly-effect pipeline over strict Text.
{-# INLINE runRegexEnginePenelope #-}
runRegexEnginePenelope ::
  Eff '[] '["regex" ':= UnaryEffect (RegexEngine Text)] a
  -> a
runRegexEnginePenelope = runRegexEnginePure
