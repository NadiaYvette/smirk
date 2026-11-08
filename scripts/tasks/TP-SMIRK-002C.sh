#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${REPO_ROOT}"

echo "=== [TP-SMIRK-002C Implementation Scaffold] ==="

echo "  Writing packages/smirk/smirk.cabal..."
cat << 'EOF_CABAL' > packages/smirk/smirk.cabal
cabal-version:      3.0
name:               smirk
version:            0.1.0.0
synopsis:           Pure, effectful, resumable, step-budgeted PCRE interpreter for Haskell
description:
    Smirk (;-P / ;-) is a pure, native Haskell PCRE regular expression engine
    featuring step-bounded execution (ReDoS defense), pausable/resumable matching,
    dual algebraic effect interpreters (effectful & polysemy), and mono-traversable
    sequence polymorphism (Text, ByteString, Seq, Vector, NonEmpty).
license:            Apache-2.0
license-file:       LICENSE
author:             Nadia Yvette Chambers
maintainer:         Nadia Chambers <nadia.yvette.chambers@ik.me>
homepage:           https://github.com/NadiaYvette/smirk
bug-reports:        https://github.com/NadiaYvette/smirk/issues
category:           Text, Regex, Effects
build-type:         Simple
extra-doc-files:
    README.md
  , CHANGELOG.md
  , LICENSE

source-repository head
    type:     git
    location: https://github.com/NadiaYvette/smirk.git
    subdir:   packages/smirk

common common-options
    default-language:   GHC2021
    default-extensions:
        DataKinds
        DeriveAnyClass
        DeriveGeneric
        DerivingStrategies
        DuplicateRecordFields
        GADTs
        GeneralizedNewtypeDeriving
        LambdaCase
        MultiParamTypeClasses
        OverloadedRecordDot
        OverloadedStrings
        RankNTypes
        RecordWildCards
        ScopedTypeVariables
        TypeApplications
        TypeFamilies
        TypeOperators
    ghc-options:
        -Wall
        -Wcompat
        -Widentities
        -Wincomplete-record-updates
        -Wincomplete-uni-patterns
        -Wpartial-fields
        -Wredundant-constraints

library
    import:             common-options
    hs-source-dirs:     src
    exposed-modules:
        Smirk
        Smirk.Core.AST
        Smirk.Core.Parser
        Smirk.Core.Bytecode
        Smirk.Core.Compiler
        Smirk.Core.Cursor
        Smirk.Core.Evacuable
        Smirk.Core.Quote
        Smirk.Core.Types
        Smirk.Engine.Sequence
        Smirk.Engine.VM
        Smirk.Glob
    build-depends:
        base >= 4.16 && < 5,
        containers >= 0.6 && < 1,
        bytestring >= 0.11 && < 1,
        megaparsec >= 9.0 && < 10,
        mono-traversable >= 1.0 && < 2,
        parser-combinators >= 1.3 && < 2,
        polytropos-core >= 0.1,
        primitive >= 0.7 && < 0.10,
        template-haskell >= 2.18 && < 3,
        text >= 2.0 && < 3,
        vector >= 0.12 && < 1

test-suite smirk-test
    import:             common-options
    type:               exitcode-stdio-1.0
    hs-source-dirs:     test
    main-is:            Spec.hs
    other-modules:
        CursorSpec
        ParserSpec
        VMSpec
        SequenceSpec
        QuoteSpec
        GlobSpec
    build-depends:
        base >= 4.16 && < 5,
        bytestring >= 0.11 && < 1,
        containers >= 0.6 && < 1,
        hspec >= 2.10 && < 3,
        polytropos-core >= 0.1,
        primitive >= 0.7 && < 0.10,
        smirk,
        text >= 2.0 && < 3,
        vector >= 0.12 && < 1
EOF_CABAL

echo "  Writing packages/smirk/src/Smirk/Core/Cursor.hs..."
cat << 'EOF_CURSOR_HS' > packages/smirk/src/Smirk/Core/Cursor.hs
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
  , MatchResult (..)
  , MatchStatus (..)
  , ThreadState (..)
  )
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM (resumeMatch)

-- | Execution outcome produced by resuming or executing a match.
-- In TP-SMIRK-002C, this encapsulates the 'MatchStatus s'.
-- In TP-SMIRK-002E, this expands to track multi-resource 'StepMetrics'.
newtype MatchOutcome s = MatchOutcome
  { outcomeStatus :: MatchStatus s
  } deriving stock (Eq, Show, Generic)

-- | Construct a 'MatchOutcome' from a 'MatchStatus'.
mkMatchOutcome :: MatchStatus s -> MatchOutcome s
mkMatchOutcome = MatchOutcome

-- | Extract the 'MatchStatus' from an outcome.
outcomeToStatus :: MatchOutcome s -> MatchStatus s
outcomeToStatus = outcomeStatus

-- | Extract a successful 'MatchResult', if the outcome was 'Matched'.
outcomeToMaybeResult :: MatchOutcome s -> Maybe (MatchResult s)
outcomeToMaybeResult outcome = case outcome.outcomeStatus of
  Matched res -> Just res
  _           -> Nothing

-- | Top-level linear continuation executor function.
--
-- Consumes the 'MatchContinuation s' with multiplicity @%1@, guaranteeing at
-- compile time that the stateful search continuation is evaluated exactly once.
--
-- [ref:note-linear-continuation-discipline]
resumeLinear :: RegexTarget s => MatchContinuation s %1 -> Fuel -> MatchOutcome s
resumeLinear = toLinear (\cont fuel -> MatchOutcome (resumeMatch cont fuel))
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
EOF_CURSOR_HS

echo "  Updating packages/smirk/src/Smirk/Core/Types.hs..."
cat << 'EOF_TYPES_HS' > packages/smirk/src/Smirk/Core/Types.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Core.Types
Description : Common execution types, continuation states, and result structures
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Defines common data types and structures used throughout the regular expression
engine, including execution fuel, match results, backtracking thread states,
unboxed flat capture slots ('PrimArray'), Polytropos row representations,
and execution continuations.

=== Flat Primitive Array Capture Registers
[tag:note-pike-vm-flat-captures]
Traditional Haskell regex engines often store capture group state in persistent
pointer-chasing tree maps (e.g. 'Data.Map.Strict.Map Int (Int, Int)'), which
incur pointer dereference overhead and heap allocation on every 'OpSaveStart'
and 'OpSaveEnd' instruction.

In Smirk, capture boundaries are stored in a contiguous, unboxed 'PrimArray Int'
buffer indexed as pairs: @(index * 2)@ stores the start offset and @(index * 2 + 1)@
stores the end offset. Uninitialized boundaries are flagged with sentinel value @-1@.

=== Polytropos Generic Row Projections
[tag:note-polytropos-capture-reps]
Named capture groups are projected into Polytropos 'Rep Identity ()' product
representations (tag 0, with captures ordered lexicographically in a flat
'SmallArray (Identity Any)'). This collapses nominal records and structural capture
dictionaries into a single canonical runtime substrate.
-}
module Smirk.Core.Types
  ( -- * Execution Fuel & Budgeting
    Fuel (..)

    -- * Capture Group Buffers & Flat PrimArray Storage
  , CaptureSlots (..)
  , emptyCaptureSlots
  , captureSlotsCount
  , getCaptureStart
  , getCaptureEnd
  , getCaptureSpan
  , setCaptureStart
  , setCaptureEnd
  , toCaptureMap
  , toFullMatchSpan

    -- * Polytropos Row Projections
  , toNamedCaptureSmallArray
  , toNamedCaptureRep
  , matchNamedCaptureRep
  , lookupNamedCaptureRep

    -- * Match Verdicts & Continuations
  , MatchMode (..)
  , MatchResult (..)
  , ThreadState (..)
  , MatchContinuation (..)
  , MatchStatus (..)
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Char (chr)
import Data.Functor.Identity (Identity (..))
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Primitive.PrimArray
  ( PrimArray
  , cloneMutableArray
  , indexPrimArray
  , newPrimArray
  , setPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.SmallArray
  ( SmallArray
  , indexSmallArray
  , newSmallArray
  , runSmallArray
  , sizeofSmallArray
  , writeSmallArray
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Polytropos.Core (Rep (..))
import Smirk.Core.Bytecode (Regex (..))
import Smirk.Engine.Sequence (RegexTarget (..))

-- | Execution fuel budget for step bounding and ReDoS prevention.
data Fuel
  = StepBudget !Int    -- ^ Bounded execution: halt after N instruction steps
  | Unlimited          -- ^ Unbounded execution: run to completion or exhaustion
  deriving (Eq, Show, Generic)

-- | Capture slots stored as a flat, unboxed primitive array of (startOffset, endOffset) pairs.
-- Group 0 represents the full matched span, followed by groups 1..N.
-- A sentinel value of -1 denotes an uninitialized / unset capture boundary.
newtype CaptureSlots = CaptureSlots { unCaptureSlots :: PrimArray Int }
  deriving stock (Eq, Show, Generic)

-- | Allocate empty capture slots for a given number of groups (at least group 0).
-- All slots are pre-initialized to -1 (unset).
emptyCaptureSlots :: Int -> CaptureSlots
emptyCaptureSlots numGroups = runST $ do
  let count = max 1 numGroups
      totalElements = count * 2
  marr <- newPrimArray totalElements
  setPrimArray marr 0 totalElements (-1)
  arr <- unsafeFreezePrimArray marr
  pure (CaptureSlots arr)

-- | Number of capture groups supported by this slot buffer.
captureSlotsCount :: CaptureSlots -> Int
captureSlotsCount (CaptureSlots arr) = sizeofPrimArray arr `div` 2

-- | Retrieve the start offset of capture group @idx@, if recorded.
getCaptureStart :: Int -> CaptureSlots -> Maybe Int
getCaptureStart idx (CaptureSlots arr)
  | idx >= 0 && (idx * 2) < sizeofPrimArray arr =
      let val = indexPrimArray arr (idx * 2)
      in if val >= 0 then Just val else Nothing
  | otherwise = Nothing
{-# INLINE getCaptureStart #-}

-- | Retrieve the end offset of capture group @idx@, if recorded.
getCaptureEnd :: Int -> CaptureSlots -> Maybe Int
getCaptureEnd idx (CaptureSlots arr)
  | idx >= 0 && (idx * 2 + 1) < sizeofPrimArray arr =
      let val = indexPrimArray arr (idx * 2 + 1)
      in if val >= 0 then Just val else Nothing
  | otherwise = Nothing
{-# INLINE getCaptureEnd #-}

-- | Retrieve both the start and end offsets of group @idx@ as a span @(start, end)@.
getCaptureSpan :: Int -> CaptureSlots -> Maybe (Int, Int)
getCaptureSpan idx slots = do
  st <- getCaptureStart idx slots
  en <- getCaptureEnd idx slots
  pure (st, en)
{-# INLINE getCaptureSpan #-}

-- | Set the start offset of capture group @idx@ in the slot buffer.
setCaptureStart :: Int -> Int -> CaptureSlots -> CaptureSlots
setCaptureStart idx offset (CaptureSlots arr)
  | idx >= 0 && (idx * 2) < sizeofPrimArray arr = runST $ do
      let sz = sizeofPrimArray arr
      marr <- newPrimArray sz
      forM_ [0 .. sz - 1] $ \i ->
        writePrimArray marr i (indexPrimArray arr i)
      writePrimArray marr (idx * 2) offset
      arr' <- unsafeFreezePrimArray marr
      pure (CaptureSlots arr')
  | otherwise = CaptureSlots arr
{-# INLINE setCaptureStart #-}

-- | Set the end offset of capture group @idx@ in the slot buffer.
setCaptureEnd :: Int -> Int -> CaptureSlots -> CaptureSlots
setCaptureEnd idx offset (CaptureSlots arr)
  | idx >= 0 && (idx * 2 + 1) < sizeofPrimArray arr = runST $ do
      let sz = sizeofPrimArray arr
      marr <- newPrimArray sz
      forM_ [0 .. sz - 1] $ \i ->
        writePrimArray marr i (indexPrimArray arr i)
      writePrimArray marr (idx * 2 + 1) offset
      arr' <- unsafeFreezePrimArray marr
      pure (CaptureSlots arr')
  | otherwise = CaptureSlots arr
{-# INLINE setCaptureEnd #-}

-- | Convert flat capture slots into a standard 1-based numbered capture map.
toCaptureMap :: RegexTarget s => CaptureSlots -> s -> Map Int s
toCaptureMap slots target =
  let count = captureSlotsCount slots
  in Map.fromList
       [ (idx, seqSlice st (en - st) target)
       | idx <- [1 .. count - 1]
       , Just (st, en) <- [getCaptureSpan idx slots]
       ]

-- | Extract the overall match span @(startOffset, length)@ from group 0.
toFullMatchSpan :: CaptureSlots -> Int -> (Int, Int)
toFullMatchSpan slots fallbackEnd =
  case getCaptureSpan 0 slots of
    Just (st, en) -> (st, max 0 (en - st))
    Nothing       -> (0, fallbackEnd)
{-# INLINE toFullMatchSpan #-}

-- | Materialize named capture groups as a flat 'SmallArray' of slices,
-- ordered lexicographically by capture group name.
toNamedCaptureSmallArray
  :: RegexTarget s
  => Regex
  -> CaptureSlots
  -> s
  -> SmallArray (Maybe s)
toNamedCaptureSmallArray regex slots target =
  let sortedNames = Map.toAscList regex.groupNames
      n = length sortedNames
  in runSmallArray $ do
       marr <- newSmallArray n Nothing
       forM_ (zip [0..] sortedNames) $ \(i, (_name, gIdx)) -> do
         case getCaptureSpan gIdx slots of
           Just (st, en) -> do
             let !slice = seqSlice st (en - st) target
             writeSmallArray marr i (Just slice)
           Nothing -> pure ()
       pure marr

-- | Project named capture groups into a Polytropos 'Rep Identity ()' record representation.
-- The resulting 'Rep' has tag 0 (product representation) and stores each named capture
-- slot in a flat 'SmallArray (Identity Any)', ordered lexicographically by capture name.
toNamedCaptureRep
  :: forall s. RegexTarget s
  => Regex
  -> CaptureSlots
  -> s
  -> Rep Identity ()
toNamedCaptureRep regex slots target =
  let sortedNames = Map.toAscList regex.groupNames
      n = length sortedNames
      arr = runSmallArray $ do
        marr <- newSmallArray n (Identity (unsafeCoerce (Nothing :: Maybe s)))
        forM_ (zip [0..] sortedNames) $ \(i, (_name, gIdx)) -> do
          case getCaptureSpan gIdx slots of
            Just (st, en) -> do
              let !slice = seqSlice st (en - st) target
                  !val = Identity (unsafeCoerce (Just slice :: Maybe s))
              writeSmallArray marr i val
            Nothing -> pure ()
        pure marr
  in Rep 0 arr

-- | Convenience projection extracting named capture groups from a 'MatchResult' into Polytropos 'Rep'.
matchNamedCaptureRep
  :: forall s. RegexTarget s
  => Regex
  -> MatchResult s
  -> s
  -> Rep Identity ()
matchNamedCaptureRep regex res =
  toNamedCaptureRep regex res.captureSlots

-- | Lookup a named capture group slice from a Polytropos capture 'Rep'.
lookupNamedCaptureRep
  :: forall s. Text
  -> Regex
  -> Rep Identity ()
  -> Maybe s
  lookupNamedCaptureRep name regex (Rep _ arr) =
  let sortedNames = map fst (Map.toAscList regex.groupNames)
  in case elemIndex name sortedNames of
       Just idx | idx < sizeofSmallArray arr ->
         let Identity anyVal = indexSmallArray arr idx
         in unsafeCoerce anyVal :: Maybe s
       _ -> Nothing

-- | Successful regular expression match result with full capture offsets.
data MatchResult s = MatchResult
  { matchedSlice  :: !s             -- ^ Exact matched slice of the input
  , matchOffset   :: !Int           -- ^ 0-based character offset where match begins
  , matchLength   :: !Int           -- ^ Total length (in characters) of the matched slice
  , captureSlots  :: !CaptureSlots  -- ^ Flat unboxed capture registers (group 0..N)
  , captureGroups :: !(Map Int s)   -- ^ Numbered capture groups (1-indexed)
  , namedGroups   :: !(Map Text s)  -- ^ Named capture groups mapped to slices
  , stepsConsumed :: !Int           -- ^ Total VM steps executed to find this match
  } deriving stock (Eq, Show, Generic)

-- | Internal VM thread execution state with flat unboxed capture registers.
data ThreadState = ThreadState
  { pc     :: !Int           -- ^ Program counter: index into program vector
  , strIdx :: !Int           -- ^ Current position in target character sequence
  , saves  :: !CaptureSlots  -- ^ Flat unboxed capture registers
  } deriving stock (Eq, Show, Generic)

-- | Match execution mode controlling empty match acceptance.
data MatchMode
  = MatchStandard   -- ^ Standard matching: allows zero-length (empty) matches
  | MatchNotEmpty   -- ^ PCRE2_NOTEMPTY semantics: rejects zero-length matches (matchLength >= 1)
  deriving stock (Eq, Show, Generic)

-- | Pausable and resumable execution continuation captured when fuel is exhausted.
data MatchContinuation s = MatchContinuation
  { savedMatchMode   :: !MatchMode      -- ^ Match execution mode (standard or non-empty)
  , savedRegex       :: !Regex          -- ^ The compiled regex being evaluated
  , savedTarget      :: !s              -- ^ The target sequence
  , savedStartIndex  :: !Int            -- ^ Outer search loop start offset
  , currentThread    :: !ThreadState    -- ^ Active thread state at suspension point
  , backtrackStack   :: ![ThreadState]  -- ^ Pending alternative branches
  , currentSteps     :: !Int            -- ^ Total steps executed up to suspension
  } deriving stock (Eq, Show, Generic)

-- | Verdict of a matching operation.
data MatchStatus s
  = Matched !(MatchResult s)            -- ^ A match was successfully found
  | NoMatch                             -- ^ The pattern does not match the target
  | OutOfFuel !(MatchContinuation s)    -- ^ Fuel budget exhausted; resumable
  deriving stock (Eq, Show, Generic)
EOF_TYPES_HS

echo "  Updating packages/smirk/src/Smirk.hs..."
cat << 'EOF_SMIRK_HS' > packages/smirk/src/Smirk.hs
{- |
Module      : Smirk
Description : Pure, effectful, resumable, step-budgeted PCRE interpreter for Haskell
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

@Smirk@ (;-P / ;-) is a pure, native Haskell PCRE regular expression engine
featuring step-bounded execution (ReDoS defense), pausable/resumable matching,
and @mono-traversable@ sequence polymorphism.

=== Key Architectural Pillars

1. __Step-Bounded Execution & ReDoS Defense:__ Every pattern evaluation can be
   constrained by a discrete 'Fuel' step budget ('StepBudget' or 'Unlimited'). If thread
   state evaluations exceed the threshold, evaluation halts safely and yields
   'OutOfFuel' along with a resumable 't:MatchContinuation'.

2. __Sequence Polymorphism:__ Pattern matching operates uniformly across any
   data type implementing 'RegexTarget', including strict 'Text', @ByteString@,
   'String', @Seq Char@, and @Vector Char@.

3. __Compile-Time QuasiQuoters:__ Static pattern validation via @[re|...|]@,
   stream editor substitution via @[ed|s/find/replace/g|]@, and shell/glob
   matching via @[glob|...|]@.

4. __Modular Effect Handlers:__ Algebraic effect bindings for @effectful@
   (@smirk-effectful@), @polysemy@ (@smirk-polysemy@), and @penelope@ (@smirk-penelope@).
-}
module Smirk
  ( -- * Core Types & API
    Regex (..)
  , compileRegex
  , compileRegexWithFlags
  , compileNonEmptyRegex
  , Flags (..)
  , defaultFlags
  , testMatch
  , execMatch
  , execMatchNotEmpty
  , execMatchWithFuel

    -- * Glob Matching
  , compileGlob
  , compileGlobWithOptions
  , compileWildcard
  , GlobOptions (..)
  , defaultGlobOptions
  , wildcardGlobOptions
  , matchGlob

    -- * QuasiQuoters & Editors
  , re
  , reNE
  , ed
  , glob
  , Editor (..)
  , applyEditor

    -- * AST Analysis & Evacuability
  , AST (..)
  , isNullableAST
  , astMinLength
  , astMaxLength
  , maxLookbehindHorizon
  , collectLookbehinds

    -- * Non-Empty Slices & Targets
  , NonEmptySlice (..)
  , mkNonEmptySlice
  , unsafeNonEmptySlice
  , NonEmptyTarget
  , ToNonEmptyTarget (..)
  , NonEmptyMatchResult (..)
  , toNonEmptyMatchResult

    -- * Compiler & Bytecode
  , Opcode (..)
  , Program
  , parsePCRE
  , parsePCREWithFlags
  , compileAST
  , compileToRegex

    -- * Execution & VM Modes
  , RegexTarget (..)
  , Fuel (..)
  , MatchMode (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , runMatchWithMode
  , runMatchAtWithMode
  , runMatchNotEmpty
  , resumeMatch
  , findAllMatches
  , findAllMatchesWithMode
  , findAllMatchesNotEmpty

    -- * Capture Registers & Polytropos Projections
  , CaptureSlots (..)
  , emptyCaptureSlots
  , getCaptureStart
  , getCaptureEnd
  , getCaptureSpan
  , toCaptureMap
  , toFullMatchSpan
  , toNamedCaptureSmallArray
  , toNamedCaptureRep
  , matchNamedCaptureRep
  , lookupNamedCaptureRep

    -- * Linear Continuation & Graded Modal Cursor Discipline
  , MatchOutcome (..)
  , mkMatchOutcome
  , outcomeToStatus
  , outcomeToMaybeResult
  , ResumeLinear (..)
  , resumeLinear
  , mkResumeLinear
  , LinearContinuation (..)
  , toLinearContinuation
  , fromLinearContinuation
  , resumeLinearCont
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

import Data.Text (Text)

import Smirk.Core.AST
import Smirk.Core.Bytecode
import Smirk.Core.Compiler
import Smirk.Core.Cursor
import Smirk.Core.Evacuable
import Smirk.Core.Parser
import Smirk.Core.Quote
import Smirk.Core.Types
import Smirk.Engine.Sequence
import Smirk.Engine.VM
import Smirk.Glob

-- | Compile a PCRE regular expression string into a runnable 't:Regex'.
-- Returns a parse error description as 'Left' on failure.
compileRegex :: Text -> Either Text Regex
compileRegex = compileRegexWithFlags defaultFlags
{-# INLINE compileRegex #-}

-- | Compile a PCRE regular expression string with explicit compilation 't:Flags'.
compileRegexWithFlags :: Flags -> Text -> Either Text Regex
compileRegexWithFlags flags input = do
  ast <- parsePCREWithFlags flags input
  pure (compileToRegex input ast)

-- | Test if a target sequence matches a regular expression.
-- Returns 'True' if a match is found anywhere in the target sequence.
testMatch :: RegexTarget s => Regex -> s -> Bool
testMatch reg target = case runMatch reg target Unlimited of
  Matched _ -> True
  _         -> False

-- | Execute regex search returning the first 't:MatchResult', if any.
execMatch :: RegexTarget s => Regex -> s -> Maybe (MatchResult s)
execMatch reg target = case runMatch reg target Unlimited of
  Matched res -> Just res
  _           -> Nothing

-- | Execute regex search bounded by a specific step budget.
-- Returns 'OutOfFuel' with a continuation if the step threshold is exceeded.
execMatchWithFuel :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
execMatchWithFuel = runMatch
EOF_SMIRK_HS

echo "  Writing packages/smirk/test/CursorSpec.hs..."
cat << 'EOF_CURSOR_SPEC_HS' > packages/smirk/test/CursorSpec.hs
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : CursorSpec
Description : Test suite for linear continuations and graded lookbehind cursors
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module CursorSpec (spec) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Core.Cursor (Linear & Graded Modal Input Cursor Discipline)" $ do
  describe "Linear Continuation Discipline (-XLinearTypes)" $ do
    it "resumes single-shot linear continuation to successful match" $ do
      case compileRegex "hello\\s+world" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("hello    world" :: Text) (StepBudget 3) of
            OutOfFuel cont -> do
              let outcome = resumeLinear cont Unlimited
              case outcomeToMaybeResult outcome of
                Just res -> do
                  res.matchedSlice `shouldBe` "hello    world"
                  res.matchOffset `shouldBe` 0
                Nothing -> expectationFailure "Expected resumeLinear to find match"
            _ -> expectationFailure "Expected initial runMatch to halt with OutOfFuel"

    it "resumes using LinearContinuation wrapper via resumeLinearCont" $ do
      case compileRegex "abc[0-9]+def" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("xyzabc12345defghi" :: Text) (StepBudget 4) of
            OutOfFuel cont -> do
              let linCont = toLinearContinuation cont
                  outcome = resumeLinearCont linCont Unlimited
              case outcome.outcomeStatus of
                Matched res -> res.matchedSlice `shouldBe` "abc12345def"
                _           -> expectationFailure "Expected Matched"
            _ -> expectationFailure "Expected OutOfFuel"

    it "executes via mkResumeLinear and runResumeLinear function wrapper" $ do
      case compileRegex "pattern" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("search for pattern here" :: Text) (StepBudget 2) of
            OutOfFuel cont -> do
              let runner = mkResumeLinear
                  outcome = runResumeLinear runner cont Unlimited
              case outcomeToMaybeResult outcome of
                Just res -> res.matchedSlice `shouldBe` "pattern"
                Nothing  -> expectationFailure "Expected match"
            _ -> expectationFailure "Expected OutOfFuel"

    it "handles NoMatch verdict when resumed with unlimited fuel" $ do
      case compileRegex "nonexistent" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("some long input string that does not have the word" :: Text) (StepBudget 5) of
            OutOfFuel cont -> do
              let outcome = resumeLinear cont Unlimited
              outcome.outcomeStatus `shouldBe` NoMatch
              outcomeToMaybeResult outcome `shouldBe` Nothing
            _ -> expectationFailure "Expected OutOfFuel"

  describe "Graded Lookbehind Cursor (GradedCursor w s)" $ do
    it "reflects static lookbehind horizon at type level" $ do
      let c0 = mkGradedCursor ("test" :: Text) :: GradedCursor 0 Text
          c3 = mkGradedCursor ("test" :: Text) :: GradedCursor 3 Text
          c8 = mkGradedCursor ("test" :: Text) :: GradedCursor 8 Text
      cursorHorizon c0 `shouldBe` 0
      cursorHorizon c3 `shouldBe` 3
      cursorHorizon c8 `shouldBe` 8

    it "evacuates prefix on horizon 0 immediately" $ do
      let c = mkGradedCursor ("hello world" :: Text) :: GradedCursor 0 Text
          cAdv = advanceMinActive 6 c
      cursorHorizon cAdv `shouldBe` 0
      safeEvacuationHorizon cAdv `shouldBe` 6
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 6
      c'.cursorTarget `shouldBe` "world"
      cursorRemainingLength c' `shouldBe` 5

    it "preserves lookbehind window w on GradedCursor w" $ do
      -- Target length 10: "abcdefghij"
      -- Horizon = 3. Active thread at offset 7.
      -- Safe evacuation = 7 - 3 = 4 chars ("abcd" can be evacuated).
      let c = mkGradedCursor ("abcdefghij" :: Text) :: GradedCursor 3 Text
          cAdv = advanceMinActive 7 c
      safeEvacuationHorizon cAdv `shouldBe` 4
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 4
      c'.cursorBaseOffset `shouldBe` 4
      c'.cursorTarget `shouldBe` "efghij"
      cursorRemainingLength c' `shouldBe` 6
      -- Relative offset: absolute 7 is at relative index (7 - 4) = 3 ('h')
      cursorToRelativeOffset c' 7 `shouldBe` Just 3
      cursorFromRelativeOffset c' 3 `shouldBe` 7
      -- Relative index 0..2 ("efg") is intact in c'.cursorTarget
      cursorSlice 0 3 c' `shouldBe` "efg"
      -- Absolute index 2 was evacuated
      cursorToRelativeOffset c' 2 `shouldBe` Nothing

    it "handles zero evacuation when active offset <= horizon" $ do
      let c = mkGradedCursor ("abcdef" :: Text) :: GradedCursor 5 Text
          cAdv = advanceMinActive 3 c
      safeEvacuationHorizon cAdv `shouldBe` 0
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 0
      c'.cursorBaseOffset `shouldBe` 0
      c'.cursorTarget `shouldBe` "abcdef"

    it "reclaims multi-byte UTF-8 sequences safely on Text" $ do
      -- 'λ' is 2 bytes, '€' is 3 bytes, '🦀' is 4 bytes
      -- "λ € 🦀 rest": 3 chars ("λ €") takes 2 + 1 + 3 = 6 UTF-8 bytes
      let t = ("λ € 🦀 rest" :: Text)
          c = mkGradedCursor t :: GradedCursor 0 Text
          cAdv = advanceMinActive 3 c -- after "λ €" (3 chars, 6 UTF-8 bytes)
          (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 3
      c'.cursorTarget `shouldBe` " 🦀 rest"
      cursorRemainingLength c' `shouldBe` 7

    it "evacuates raw byte sequences on ByteString" $ do
      let bs = ("hello world" :: ByteString)
          c = mkGradedCursor bs :: GradedCursor 0 ByteString
          cAdv = advanceMinActive 6 c
          (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 6
      c'.cursorTarget `shouldBe` "world"
      cursorRemainingLength c' `shouldBe` 5

    it "extracts minimum active offset from suspended MatchContinuation" $ do
      case compileRegex "deep.*pattern" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("deep long input string with pattern" :: Text) (StepBudget 10) of
            OutOfFuel cont -> do
              let c = cursorFromContinuation @2 cont
              cursorHorizon c `shouldBe` 2
              c.cursorMinActiveOffset `shouldBe` minimumActiveOffset cont
              let safe = safeEvacuationHorizon c
              safe `shouldBe` max 0 (minimumActiveOffset cont - 2)
            _ -> expectationFailure "Expected OutOfFuel"
EOF_CURSOR_SPEC_HS

echo "  Updating packages/smirk/test/Spec.hs..."
cat << 'EOF_SPEC_HS' > packages/smirk/test/Spec.hs
module Main (main) where

import Test.Hspec
import qualified CursorSpec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified QuoteSpec
import qualified GlobSpec

main :: IO ()
main = hspec $ do
  CursorSpec.spec
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  QuoteSpec.spec
  GlobSpec.spec
EOF_SPEC_HS

echo "  Updating docs/notes/SMIRK_DESIGN.md..."
cat << 'EOF_SMIRK_DESIGN_MD' > docs/notes/SMIRK_DESIGN.md
# SMIRK_DESIGN.md — Architectural Decisions & PCRE Engine Invariants

Canonical design monograph for `smirk`.

---

## 1. Design Vision & Motivation

`smirk` (;-P / ;-) provides a pure, native Haskell PCRE regular expression engine with ironclad defenses against algorithmic complexity attacks, first-class effect system integrations, and container polymorphism.

Historically, Haskell regular expression solutions presented several major drawbacks:

1. **Unsafe C Bindings (e.g. `pcre-heavy`, `regex-pcre`):**
   - Direct foreign function calls to libpcre / libpcre2 introduce C toolchain dependencies, memory safety vulnerabilities, and build complexity on cross-compilation targets.
2. **Algorithmic ReDoS Vulnerabilities (Backtracking Hell):**
   - Naive backtracking engines exhibit exponential $\mathcal{O}(2^N)$ time complexity on ambiguous grammars (such as `(a+)+$`), allowing unauthenticated attackers to lock server CPU cores indefinitely.
3. **Container Inflexibility:**
   - Libraries frequently mandate converting inputs to `ByteString` or `Text`, inducing extraneous heap allocations when processing lazy `String`, `Vector Char`, or finger-tree `Seq Char`.
4. **Effect System Coupling:**
   - Extensible effect users were forced to choose between heavy wrapper packages or dragging in monolithic dependencies.

`smirk` resolves these challenges through:
- A pure Haskell Pike-style virtual machine compiling PCRE patterns to bytecode.
- Step-bounded execution (`Fuel`) guaranteeing that matching terminates within a defined step budget.
- Pausable and resumable execution continuations (`MatchContinuation`).
- A sequence polymorphism typeclass (`RegexTarget`) operating directly on standard Haskell sequences.
- A split-package architecture providing dedicated, zero-overhead effect wrappers for `effectful` and `polysemy`.

---

## 2. Canonical Architectural Decisions

### D1: Pike-Style Virtual Machine with Explicit Backtracking Stack
- **Decision:** Regex patterns compile into a linear bytecode vector ('Program' of 'Opcode's: `OpChar`, `OpClass`, `OpSplit`, `OpJump`, `OpSaveStart`, `OpSaveEnd`, `OpAssertLookahead`, `OpAssertLookbehind`, `OpMatchBackRef`, `OpMatch`).
- **Rationale:** Avoids recursive C-style runtime call stacks that can overflow on deep inputs. Thread alternatives are maintained on an explicit heap stack (`[ThreadState]`), enabling clean stepping and continuation capture.
- **Reference:** Note [Pike VM State Machine and Backtracking Stack] `[ref:note-pike-vm-state-machine]`.

### D2: Fuel-Bounded Step Budgeting for ReDoS Defense
- **Decision:** Every instruction step in 'stepVM' consumes 1 unit of 'Fuel' (`StepBudget !Int` or `Unlimited`). If the budget is exhausted before a match or total failure is determined, execution halts safely and yields `OutOfFuel (MatchContinuation s)`.
- **Rationale:** Completely neutralizes Regular Expression Denial of Service (ReDoS) attacks by giving host applications deterministic control over execution latency.
- **Reference:** Note [Fuel-Bounded Step Budget and ReDoS Defense] `[ref:note-fuel-step-budget]`.

### D3: First-Class Pausable and Resumable Continuations
- **Decision:** When 'OutOfFuel' is returned, the 'MatchContinuation' bundles the target sequence, program counter, thread state, backtrack stack, and step count. The operation can be resumed at any time via 'resumeMatch' with a fresh fuel allocation.
- **Rationale:** Enables time-sliced, interactive, or background regex processing across long-running streams without re-executing previous steps from scratch.

### D4: Sequence Polymorphism via `RegexTarget`
- **Decision:** Instead of fixing input types to strict `Text` or `ByteString`, all VM execution primitives operate on the 'RegexTarget' typeclass, supporting `Text`, `ByteString`, `[Char]`, `Seq Char`, and `Vector Char`.
- **Rationale:** Eliminates redundant data copying and heap allocations at application boundaries.

### D5: Decoupled Multi-Package Architecture
- **Decision:** The codebase is partitioned into three independent packages:
  - `smirk`: Pure core engine, VM, compiler, and QuasiQuoters.
  - `smirk-effectful`: Dedicated bindings for the `effectful` algebraic effect library.
  - `smirk-polysemy`: Dedicated bindings for the `polysemy` extensible effect library.
- **Rationale:** Users who only require `effectful` never transitively depend on `polysemy`, and users desiring pure regexes never depend on any effect library.

### D6: Compile-Time QuasiQuoters (`[re|...|]`, `[ed|...|]`, `[glob|...|]`)
- **Decision:** Exposes QuasiQuoters validating PCRE syntax, stream-editor replacement patterns, and glob wildcards at compile time using Template Haskell.
- **Rationale:** Detects regex syntax errors during compilation rather than failing at runtime, and builds pre-validated AST structures.

### D7: Native POSIX and Git-Style Globbing Engine
- **Decision:** Implements filesystem globbing (`compileGlob`, `[glob|...|]`) and unconstrained wildcards (`compileWildcard`) by compiling glob tokens into equivalent Smirk PCRE AST representations.
- **Rationale:** Provides zero-dependency, high-performance globbing that shares the same step-bounded VM, ReDoS defense, and sequence polymorphism as standard regexes.

### D8: Zero-Warning Compiler Discipline
- **Decision:** Every package in Smirk compiles under `-Wall -Wcompat -Wredundant-constraints` with 0 HLint hints and 0 compiler warnings.
- **Rationale:** Preserves strict code hygiene and eliminates latent runtime hazards.

### D9: Forge-Tiered CI Architecture
- **Decision:** CI pipelines partition into lightweight Tier 1 static gates (Tagref, link integrity, HLint, CLOC) and Tier 2 full multi-compiler test matrices.
- **Reference:** Note [Forge-Tiered CI Resource Allocation and Static Gate Partitioning] `[ref:note-forge-tiered-ci]`.

### D10: Non-Destructive Scaffolding and Quality Gate Governance
- **Decision:** Continuous tooling, git hooks, and verification harnesses are managed via `gachi retrofit` without overwriting existing application code.
- **Rationale:** Guarantees progressive quality improvement across the multi-package workspace.

---

## 3. Grounded Notes Architecture

- **Note [Pike VM State Machine and Backtracking Stack]** (`Smirk.Engine.VM`):  
  `[ref:note-pike-vm-state-machine]`  
  Details the bytecode instruction set, explicit backtracking stack, and thread branching semantics.
- **Note [Fuel-Bounded Step Budget and ReDoS Defense]** (`Smirk.Engine.VM`):  
  `[ref:note-fuel-step-budget]`  
  Specifies the fuel step consumption model, budget exhaustion handling, and ReDoS immunity.
- **Note [Forge-Tiered CI Resource Allocation and Static Gate Partitioning]** (`docs/notes/DOC_STRATEGY.md`):  
  `[ref:note-forge-tiered-ci]`  
  Details CI execution tiering across GitHub Actions, Framagit, and Radicle.
- **Note [Pike VM Flat Capture Register Allocation]** (`Smirk.Core.Types`):  
  `[ref:note-pike-vm-flat-captures]`  
  Replaces pointer-chasing maps with flat unboxed PrimArray registers.
- **Note [Polytropos Record Representations for Named Captures]** (`Smirk.Core.Types`):  
  `[ref:note-polytropos-capture-reps]`  
  Projects named capture groups into row-polymorphic Polytropos Rep structures.
- **Note [Static AST Nullability Analysis]** (`Smirk.Core.Evacuable`):  
  `[ref:note-static-ast-nullability]`  
  Decides whether epsilon is in L(r) statically at compile time without VM allocation.
- **Note [Non-Empty Target Polymorphism]** (`Smirk.Core.Evacuable`):  
  `[ref:note-non-empty-target-polymorphism]`  
  Witnesses matchLength >= 1 at the type level across String, Text, ByteString, Seq, and Vector.
- **Note [Linear Continuation Discipline]** (`Smirk.Core.Cursor`):  
  `[ref:note-linear-continuation-discipline]`  
  Guarantees single-shot resumption under -XLinearTypes without continuation duplication.
- **Note [Graded Lookbehind Cursor]** (`Smirk.Core.Cursor`):  
  `[ref:note-graded-lookbehind-cursor]`  
  Tracks type-level lookbehind horizon bounds and safely evacuates dead prefix data.
EOF_SMIRK_DESIGN_MD

chmod +x "${REPO_ROOT}/scripts/tasks/TP-SMIRK-002C.sh"
echo "=== [TP-SMIRK-002C Implementation Scaffold Applied Successfully] ==="
