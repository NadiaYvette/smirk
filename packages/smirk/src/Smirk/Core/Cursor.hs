{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Smirk.Core.Cursor
Description : Linear and graded modal input cursor discipline
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Provides linear continuation execution discipline and Granule-style graded
lookbehind input cursors for streaming regular expression evaluation.

=== Linear Continuation Discipline
[tag:note-linear-continuation-discipline]
Stateful streaming continuations ('MatchContinuation') represent in-flight
virtual machine state, including the active thread, backtracking stack, and
already-consumed steps. Allowing unrestricted duplication or re-execution of
an in-flight continuation risks state divergence, race conditions, or ReDoS
budget leaks.

'ResumeLinear' and 'resumeLinear' consume a 'MatchContinuation s' with linear
multiplicity ('%1') under @-XLinearTypes@, guaranteeing statically at compile
time that a suspended execution continuation is resumed exactly once.

=== Graded Lookbehind Cursor
[tag:note-graded-lookbehind-cursor]
During streaming matching, input arrives incrementally in buffer slices. For
patterns involving lookbehind assertions (@(?<=...)@ or @(?<!...)@), the virtual
machine may need to inspect characters preceding the current thread's scan position.

'GradedCursor (horizon :: Nat) s' tracks the maximum lookbehind distance @w@
at the type level alongside the minimum active thread offset.
'evacuatePrefix' safely drops dead prefix characters that fall strictly behind
the lookbehind horizon @(cursorMinActiveOffset - w)@, enabling constant-space
or bounded-space garbage collection of streaming input buffers.
-}
module Smirk.Core.Cursor
  ( -- * Match Outcome & Metrics
    MatchOutcome (..)
  , mkMatchOutcome
  , outcomeToStatus
  , outcomeToMaybeResult

    -- * Linear Continuation Discipline
  , ResumeLinear (..)
  , resumeLinear
  , mkResumeLinear
  , LinearContinuation (..)
  , toLinearContinuation
  , fromLinearContinuation
  , resumeLinearCont

    -- * Graded Lookbehind Cursor
  , EvacuatedBytes (..)
  , GradedCursor (..)
  , mkGradedCursor
  , mkGradedCursorAt
  , cursorHorizon
  , advanceMinActive
  , minimumActiveOffset
  , cursorFromContinuation
  , safeEvacuationHorizon
  , evacuatePrefix
  , cursorRemainingLength
  , cursorSlice
  , cursorToRelativeOffset
  , cursorFromRelativeOffset
  ) where

import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Unsafe.Coerce (unsafeCoerce)

import Smirk.Core.Types
  ( Fuel (..)
  , MatchContinuation (..)
  , MatchOutcome (..)
  , ThreadState (..)
  , mkMatchOutcome
  , outcomeToMaybeResult
  , outcomeToStatus
  )
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM (resumeMatchWithMetrics)

-- | Top-level linear continuation executor function.
--
-- Consumes the 'MatchContinuation s' with multiplicity @%1@, guaranteeing at
-- compile time that the stateful search continuation is evaluated exactly once.
--
-- [ref:note-linear-continuation-discipline]
resumeLinear :: RegexTarget s => MatchContinuation s %1 -> Fuel -> MatchOutcome s
resumeLinear = toLinear resumeMatchWithMetrics
  where
    toLinear :: (MatchContinuation s -> Fuel -> MatchOutcome s)
             -> (MatchContinuation s %1 -> Fuel -> MatchOutcome s)
    toLinear = unsafeCoerce

-- | Linear continuation resumption wrapper.
--
-- Encapsulates a linear execution function ensuring single-shot resumption.
newtype ResumeLinear s = ResumeLinear
  { runResumeLinear :: MatchContinuation s %1 -> Fuel -> MatchOutcome s
  }

-- | Default constructor for 'ResumeLinear'.
mkResumeLinear :: RegexTarget s => ResumeLinear s
mkResumeLinear = ResumeLinear resumeLinear

-- | Linear wrapper newtype around 'MatchContinuation s'.
newtype LinearContinuation s = LinearContinuation
  { unLinearContinuation :: MatchContinuation s
  }

-- | Wrap an unrestricted continuation into a linear continuation.
toLinearContinuation :: MatchContinuation s -> LinearContinuation s
toLinearContinuation = LinearContinuation

-- | Unwrap a linear continuation with multiplicity @%1@.
fromLinearContinuation :: LinearContinuation s %1 -> MatchContinuation s
fromLinearContinuation (LinearContinuation cont) = cont

-- | Resume a 'LinearContinuation' wrapper linearly.
resumeLinearCont :: RegexTarget s => LinearContinuation s %1 -> Fuel -> MatchOutcome s
resumeLinearCont (LinearContinuation cont) = resumeLinear cont

-- | Number of UTF-8 bytes safely reclaimed from the input stream prefix.
newtype EvacuatedBytes = EvacuatedBytes { unEvacuatedBytes :: Int }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral)

-- | Graded lookbehind cursor modal type.
--
-- Tracks a stream slice buffer with a type-level static lookbehind horizon @(horizon :: Nat)@.
--
-- [ref:note-graded-lookbehind-cursor]
data GradedCursor (horizon :: Nat) s = GradedCursor
  { cursorTarget          :: !s
    -- ^ Active sequence slice buffer.
  , cursorBaseOffset      :: !Int
    -- ^ Absolute 0-based character offset corresponding to index 0 of 'cursorTarget'.
  , cursorMinActiveOffset :: !Int
    -- ^ Minimum absolute character offset across all active and backtracking threads.
  } deriving stock (Eq, Show, Generic)

-- | Construct a fresh 'GradedCursor' starting at offset 0.
mkGradedCursor :: s -> GradedCursor w s
mkGradedCursor target = GradedCursor
  { cursorTarget          = target
  , cursorBaseOffset      = 0
  , cursorMinActiveOffset = 0
  }

-- | Construct a 'GradedCursor' with explicit base offset and minimum active offset.
mkGradedCursorAt :: s -> Int -> Int -> GradedCursor w s
mkGradedCursorAt target base minActive = GradedCursor
  { cursorTarget          = target
  , cursorBaseOffset      = base
  , cursorMinActiveOffset = minActive
  }

-- | Reflect the static lookbehind horizon @(horizon :: Nat)@ to a runtime integer.
cursorHorizon :: forall (w :: Nat) s. KnownNat w => GradedCursor w s -> Int
cursorHorizon _ = fromIntegral (natVal (Proxy @w))

-- | Advance the minimum active offset of a cursor monotonically.
advanceMinActive :: Int -> GradedCursor w s -> GradedCursor w s
advanceMinActive newMin cursor = cursor
  { cursorMinActiveOffset = max (cursorMinActiveOffset cursor) newMin
  }

-- | Compute the minimum active character offset across all threads in a continuation.
minimumActiveOffset :: MatchContinuation s -> Int
minimumActiveOffset cont =
  let threadOffsets = cont.currentThread.strIdx : map (\t -> t.strIdx) cont.backtrackStack
  in minimum (cont.savedStartIndex : threadOffsets)

-- | Construct a 'GradedCursor' from a suspended 'MatchContinuation'.
cursorFromContinuation :: forall (w :: Nat) s. MatchContinuation s -> GradedCursor w s
cursorFromContinuation cont = GradedCursor
  { cursorTarget          = cont.savedTarget
  , cursorBaseOffset      = 0
  , cursorMinActiveOffset = minimumActiveOffset cont
  }

-- | Compute the absolute stream character offset up to which data may be safely evacuated.
safeEvacuationHorizon :: forall (w :: Nat) s. KnownNat w => GradedCursor w s -> Int
safeEvacuationHorizon cursor =
  let horizon = fromIntegral (natVal (Proxy @w))
  in max 0 (cursor.cursorMinActiveOffset - horizon)

-- | Evacuate dead prefix data preceding the lookbehind horizon.
--
-- Returns the count of UTF-8 bytes reclaimed alongside the updated 'GradedCursor'
-- whose base offset has advanced by the number of evacuated characters.
evacuatePrefix
  :: forall (w :: Nat) s. (KnownNat w, RegexTarget s)
  => GradedCursor w s
  -> (EvacuatedBytes, GradedCursor w s)
evacuatePrefix cursor =
  let safeAbs = safeEvacuationHorizon cursor
      reclaimableChars = safeAbs - cursor.cursorBaseOffset
      totalChars = seqLength cursor.cursorTarget
      charsToEvacuate = max 0 (min totalChars reclaimableChars)
  in if charsToEvacuate <= 0
       then (EvacuatedBytes 0, cursor)
       else
         let bytes = utf8SliceByteWidth 0 charsToEvacuate cursor.cursorTarget
             remainingChars = totalChars - charsToEvacuate
             newTarget = seqSlice charsToEvacuate remainingChars cursor.cursorTarget
             newBase = cursor.cursorBaseOffset + charsToEvacuate
             newCursor = cursor
               { cursorTarget     = newTarget
               , cursorBaseOffset = newBase
               }
         in (EvacuatedBytes bytes, newCursor)

-- | Remaining character count in the active buffer slice.
cursorRemainingLength :: RegexTarget s => GradedCursor w s -> Int
cursorRemainingLength c = seqLength c.cursorTarget

-- | Extract a sub-slice from the active buffer relative to current index 0.
cursorSlice :: RegexTarget s => Int -> Int -> GradedCursor w s -> s
cursorSlice off len c = seqSlice off len c.cursorTarget

-- | Convert an absolute stream offset to an index relative to the active buffer.
-- Returns 'Nothing' if the offset has already been evacuated behind 'cursorBaseOffset'.
cursorToRelativeOffset :: GradedCursor w s -> Int -> Maybe Int
cursorToRelativeOffset cursor absOff
  | absOff < cursor.cursorBaseOffset = Nothing
  | otherwise = Just (absOff - cursor.cursorBaseOffset)

-- | Convert a relative buffer index back to an absolute stream offset.
cursorFromRelativeOffset :: GradedCursor w s -> Int -> Int
cursorFromRelativeOffset cursor relOff = cursor.cursorBaseOffset + relOff
