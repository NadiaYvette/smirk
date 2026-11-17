{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Core.Budget
Description : Multi-dimensional, proportional, and hierarchical resource budget semilattice
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Provides a Polytropos-compatible multi-dimensional 'ResourceBudget' meet-semilattice
for hygienic fuel and resource accounting across the Smirk PCRE virtual machine.

=== 1. Multi-Dimensional Resource Bounds
A 'ResourceBudget' tracks three orthogonal resource dimensions simultaneously:
1. *Cumulative Step Count*: Maximum virtual machine execution steps ('maxVmSteps').
2. *Backtrack Frame Depth*: Maximum stack frames allowed in backtrack search ('maxBacktrackFrames').
3. *Materialized UTF-8 Bytes*: Physical memory footprint bound ('maxMaterializedBytes').

=== 2. Proportional / Specific-Heat Rate Bounds
To distinguish algorithmic ReDoS catastrophic backtracking explosions from benign
long matches over large documents, 'ResourceBudget' enforces a proportional rate limit:
@
stepsConsumed > startupGraceSteps + maxStepsPerUnit * max 1 unitsScanned
@
where 'startupGraceSteps' permits initial NFA compilation and small-prefix overhead,
while 'maxStepsPerUnit' caps the differential work rate per input character.

=== 3. Hierarchical Lease Tree
Hierarchical accounting links parent session quotas to local execution scopes:
@
System-Wide Counter >= Session Quota >= Thread Pool >= Call-site Scope
@
When entering a child scope via 'withResourceBudget', a sub-budget is carved out from the parent;
upon exit (normal completion or unwinding), unconsumed fuel is automatically refunded.
-}
module Smirk.Core.Budget
  ( -- * Multi-Dimensional Resource Budget
    ResourceBudget (..)
  , budgetEmpty
  , budgetUnlimited
  , defaultResourceBudget

    -- * Meet-Semilattice Algebra
  , meetBudget
  , isBudgetExceeded
  , isStepMetricsExceeded
  , debitBudget
  , checkProportionalRate

    -- * Hierarchical Lease Tree
  , BudgetHierarchy (..)
  , newBudgetHierarchy
  , withResourceBudget
  , carveChildBudget
  , refundRemainder
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )
import GHC.Generics (Generic)
import Smirk.Core.Types (Fuel (..), StepMetrics (..))

-- | Strict multi-dimensional resource budget covering cumulative, rate, and memory bounds.
data ResourceBudget = ResourceBudget
  { maxVmSteps           :: {-# UNPACK #-} !Int -- ^ Cumulative VM execution steps (0 = unlimited)
  , maxBacktrackFrames   :: {-# UNPACK #-} !Int -- ^ Maximum backtrack frames (0 = unlimited)
  , maxMaterializedBytes :: {-# UNPACK #-} !Int -- ^ UTF-8 byte materialization ceiling (0 = unlimited)
  , maxStepsPerUnit      :: {-# UNPACK #-} !Int -- ^ Proportional steps allowed per input unit (0 = unlimited)
  , startupGraceSteps    :: {-# UNPACK #-} !Int -- ^ Startup grace steps before rate limiting applies
  } deriving stock (Eq, Show, Generic)

-- | Zero/empty budget that immediately halts execution.
budgetEmpty :: ResourceBudget
budgetEmpty = ResourceBudget
  { maxVmSteps           = 0
  , maxBacktrackFrames   = 0
  , maxMaterializedBytes = 0
  , maxStepsPerUnit      = 0
  , startupGraceSteps    = 0
  }

-- | Unconstrained budget allowing unbounded execution.
budgetUnlimited :: ResourceBudget
budgetUnlimited = ResourceBudget
  { maxVmSteps           = maxBound
  , maxBacktrackFrames   = maxBound
  , maxMaterializedBytes = maxBound
  , maxStepsPerUnit      = maxBound
  , startupGraceSteps    = maxBound
  }

-- | Sensible default resource budget for general-purpose PCRE matching.
defaultResourceBudget :: ResourceBudget
defaultResourceBudget = ResourceBudget
  { maxVmSteps           = 100000
  , maxBacktrackFrames   = 5000
  , maxMaterializedBytes = 10485760 -- 10 MiB
  , maxStepsPerUnit      = 50
  , startupGraceSteps    = 500
  }

-- | Meet-semilattice operator: component-wise minimum bounding two budgets.
-- Associative, commutative, and idempotent:
-- @
-- meetBudget a (meetBudget b c) == meetBudget (meetBudget a b) c
-- meetBudget a b == meetBudget b a
-- meetBudget a a == a
-- @
meetBudget :: ResourceBudget -> ResourceBudget -> ResourceBudget
meetBudget b1 b2 = ResourceBudget
  { maxVmSteps           = min b1.maxVmSteps b2.maxVmSteps
  , maxBacktrackFrames   = min b1.maxBacktrackFrames b2.maxBacktrackFrames
  , maxMaterializedBytes = min b1.maxMaterializedBytes b2.maxMaterializedBytes
  , maxStepsPerUnit      = min b1.maxStepsPerUnit b2.maxStepsPerUnit
  , startupGraceSteps    = min b1.startupGraceSteps b2.startupGraceSteps
  }

-- | Check whether candidate budget @b1@ exceeds ceiling budget @b2@.
isBudgetExceeded :: ResourceBudget -> ResourceBudget -> Bool
isBudgetExceeded candidate ceiling =
  (ceiling.maxVmSteps > 0 && candidate.maxVmSteps > ceiling.maxVmSteps)
    || (ceiling.maxBacktrackFrames > 0 && candidate.maxBacktrackFrames > ceiling.maxBacktrackFrames)
    || (ceiling.maxMaterializedBytes > 0 && candidate.maxMaterializedBytes > ceiling.maxMaterializedBytes)
    || (ceiling.maxStepsPerUnit > 0 && candidate.maxStepsPerUnit > ceiling.maxStepsPerUnit)

-- | Check whether observed runtime execution metrics have breached budget ceilings.
isStepMetricsExceeded :: ResourceBudget -> StepMetrics -> Bool
isStepMetricsExceeded b m =
  let cumulativeStepsExceeded = b.maxVmSteps > 0 && m.stepsConsumed > b.maxVmSteps
      backtrackExceeded = b.maxBacktrackFrames > 0 && m.peakBacktrackDepth > b.maxBacktrackFrames
      materializedExceeded = b.maxMaterializedBytes > 0 && m.utf8BytesMaterialized > b.maxMaterializedBytes
      rateLimitExceeded = checkProportionalRate b m.stepsConsumed m.unitsScanned
  in cumulativeStepsExceeded || backtrackExceeded || materializedExceeded || rateLimitExceeded

-- | Test whether consumed steps exceed the proportional rate allowed for the scanned input length.
checkProportionalRate :: ResourceBudget -> Int -> Int -> Bool
checkProportionalRate b consumed units =
  b.maxStepsPerUnit > 0
    && consumed > b.startupGraceSteps + b.maxStepsPerUnit * max 1 units

-- | Debit consumed execution metrics from a resource budget, returning the remaining budget.
debitBudget :: ResourceBudget -> StepMetrics -> ResourceBudget
debitBudget b m = ResourceBudget
  { maxVmSteps           = max 0 (b.maxVmSteps - m.stepsConsumed)
  , maxBacktrackFrames   = b.maxBacktrackFrames
  , maxMaterializedBytes = max 0 (b.maxMaterializedBytes - m.utf8BytesMaterialized)
  , maxStepsPerUnit      = b.maxStepsPerUnit
  , startupGraceSteps    = max 0 (b.startupGraceSteps - m.stepsConsumed)
  }

-- | Hierarchical budget lease tree coordinating parent and child quotas.
data BudgetHierarchy = BudgetHierarchy
  { hierarchySystemTotal  :: !(IORef Int)         -- ^ System-wide atomic counter
  , hierarchySessionQuota :: !(IORef Int)         -- ^ Session / thread-group quota
  , hierarchyLocalBudget  :: {-# UNPACK #-} !ResourceBudget -- ^ Active thread-local budget
  } deriving stock (Generic)

-- | Initialize a new root 'BudgetHierarchy' lease tree.
newBudgetHierarchy :: ResourceBudget -> IO BudgetHierarchy
newBudgetHierarchy budget = do
  sysRef <- newIORef budget.maxVmSteps
  sesRef <- newIORef budget.maxVmSteps
  pure BudgetHierarchy
    { hierarchySystemTotal  = sysRef
    , hierarchySessionQuota = sesRef
    , hierarchyLocalBudget  = budget
    }

-- | Carve out a child budget from a parent hierarchy.
carveChildBudget :: BudgetHierarchy -> ResourceBudget -> IO BudgetHierarchy
carveChildBudget parent childReq = do
  let childBudget = meetBudget parent.hierarchyLocalBudget childReq
      carvedSteps = childBudget.maxVmSteps
  atomicModifyIORef' parent.hierarchySessionQuota (\curr -> (max 0 (curr - carvedSteps), ()))
  atomicModifyIORef' parent.hierarchySystemTotal (\curr -> (max 0 (curr - carvedSteps), ()))
  childSesRef <- newIORef carvedSteps
  pure BudgetHierarchy
    { hierarchySystemTotal  = parent.hierarchySystemTotal
    , hierarchySessionQuota = childSesRef
    , hierarchyLocalBudget  = childBudget
    }

-- | Refund unconsumed steps from a child lease back to parent session counters.
refundRemainder :: BudgetHierarchy -> ResourceBudget -> IO ()
refundRemainder parent remaining = do
  let refundSteps = remaining.maxVmSteps
  when (refundSteps > 0) $ do
    atomicModifyIORef' parent.hierarchySessionQuota (\curr -> (curr + refundSteps, ()))
    atomicModifyIORef' parent.hierarchySystemTotal (\curr -> (curr + refundSteps, ()))

-- | Hierarchical scope bracket: carve a sub-budget for a child action and automatically
-- refund unconsumed quota back to the parent hierarchy upon exit.
withResourceBudget ::
  BudgetHierarchy
  -> ResourceBudget
  -> (BudgetHierarchy -> IO a)
  -> IO a
withResourceBudget parent requestedBudget =
  bracket
    (carveChildBudget parent requestedBudget)
    (\child -> refundRemainder parent child.hierarchyLocalBudget)
