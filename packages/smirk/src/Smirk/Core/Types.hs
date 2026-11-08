{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Core.Types
Description : Flat primitive capture slots, Polytropos record projections, and VM state types
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Defines the core runtime representation types for the Smirk virtual machine,
centered around unboxed flat primitive arrays for capture group register tracking
and Polytropos generic 'Rep' projections for zero-copy named group extraction.

=== Pike VM Flat Capture Invariant
[tag:note-pike-vm-flat-captures]
Traditional regex virtual machines (including previous versions of Smirk) model
sub-match capture registers using tree-based maps ('Data.Map.Strict.Map Int (Int, Maybe Int)').
In the inner Pike VM execution loop, every 'OpSaveStart' and 'OpSaveEnd' allocates new
map nodes and triggers tree re-balancing overhead.

Smirk eliminates this allocation bottleneck by replacing capture register maps with
'CaptureSlots', backed by a single unboxed 'Data.Primitive.PrimArray.PrimArray Int'.
For a regular expression with $M$ capture groups, the array stores $(M + 1) \times 2$
contiguous unboxed integers:

* Even slot $2 \times i$: 0-based character start offset of group $i$.
* Odd slot $2 \times i + 1$: 0-based character end offset of group $i$.
* An offset value of @-1@ designates an uninitialized or uncaptured boundary.

=== Polytropos Integration
[tag:note-polytropos-capture-reps]
Named capture groups are projected directly into Polytropos 'Rep' values
('Rep 0 (SmallArray (Identity Any))'), enabling compile-time constant slot indexing
and unboxed target lowering without heap boxing.
-}
module Smirk.Core.Types
  ( -- * Flat Unboxed Capture Registers
    CaptureSlots (..)
  , emptyCaptureSlots
  , captureSlotsCount
  , getCaptureStart
  , getCaptureEnd
  , getCaptureSpan
  , setCaptureStart
  , setCaptureEnd
  , toCaptureMap
  , toFullMatchSpan

    -- * Polytropos & SmallArray Projections
  , toNamedCaptureSmallArray
  , toNamedCaptureRep
  , matchNamedCaptureRep
  , lookupNamedCaptureRep

    -- * Fuel & Budgeting
  , Fuel (..)

    -- * Match Execution Modes & Outcomes
  , MatchMode (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)

    -- * VM Execution State
  , ThreadState (..)
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Functor.Identity (Identity (..))
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Primitive.PrimArray
  ( PrimArray
  , copyPrimArray
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

-- | Retrieve the inclusive start and exclusive end character offsets for group @idx@.
getCaptureSpan :: Int -> CaptureSlots -> Maybe (Int, Int)
getCaptureSpan idx slots = do
  st <- getCaptureStart idx slots
  en <- getCaptureEnd idx slots
  pure (st, en)
{-# INLINE getCaptureSpan #-}

-- | Record the start offset for capture group @idx@ and invalidate any previous end offset.
setCaptureStart :: Int -> Int -> CaptureSlots -> CaptureSlots
setCaptureStart idx !stOff (CaptureSlots arr) = runST $ do
  let currentLen = sizeofPrimArray arr
      requiredLen = max currentLen ((idx + 1) * 2)
  marr <- newPrimArray requiredLen
  if requiredLen == currentLen
    then copyPrimArray marr 0 arr 0 currentLen
    else do
      copyPrimArray marr 0 arr 0 currentLen
      setPrimArray marr currentLen (requiredLen - currentLen) (-1)
  writePrimArray marr (idx * 2) stOff
  writePrimArray marr (idx * 2 + 1) (-1)
  frozen <- unsafeFreezePrimArray marr
  pure (CaptureSlots frozen)

-- | Record the end offset for capture group @idx@.
setCaptureEnd :: Int -> Int -> CaptureSlots -> CaptureSlots
setCaptureEnd idx !enOff (CaptureSlots arr) = runST $ do
  let currentLen = sizeofPrimArray arr
      requiredLen = max currentLen ((idx + 1) * 2)
  marr <- newPrimArray requiredLen
  if requiredLen == currentLen
    then copyPrimArray marr 0 arr 0 currentLen
    else do
      copyPrimArray marr 0 arr 0 currentLen
      setPrimArray marr currentLen (requiredLen - currentLen) (-1)
  writePrimArray marr (idx * 2 + 1) enOff
  frozen <- unsafeFreezePrimArray marr
  pure (CaptureSlots frozen)

-- | Extract group 0 full match span @(startOffset, endOffset)@ if recorded.
toFullMatchSpan :: CaptureSlots -> Maybe (Int, Int)
toFullMatchSpan = getCaptureSpan 0

-- | Convert 'CaptureSlots' to a standard 'Map Int s' of numbered capture groups (1-indexed).
toCaptureMap :: RegexTarget s => CaptureSlots -> s -> Map Int s
toCaptureMap slots target =
  let totalGroups = captureSlotsCount slots
  in Map.fromList
       [ (idx, seqSlice st (en - st) target)
       | idx <- [1 .. totalGroups - 1]
       , Just (st, en) <- [getCaptureSpan idx slots]
       ]

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
  }

-- | Verdict of a matching operation.
data MatchStatus s
  = Matched !(MatchResult s)            -- ^ A match was successfully found
  | NoMatch                             -- ^ The pattern does not match the target
  | OutOfFuel !(MatchContinuation s)    -- ^ Fuel budget exhausted; resumable
