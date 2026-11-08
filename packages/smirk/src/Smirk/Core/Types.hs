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

    -- * Multi-Resource Step Metrics & Execution Outcomes
  , StepMetrics (..)
  , emptyStepMetrics
  , MatchOutcome (..)
  , mkMatchOutcome
  , outcomeToStatus
  , outcomeToMaybeResult

    -- * PCRE Callouts
  , CalloutDecision (..)
  , CalloutPayload (..)

    -- * Match Execution Modes & Continuations
  , MatchMode (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)

    -- * VM Execution State & Pre-Allocated Frames
  , ThreadState (..)
  , VMFrame (..)
  , threadToFrame
  , frameToThread
  , VMFramePool (..)
  , emptyVMFramePool
  , readFrame
  , threadsToFramePool
  , framePoolToThreads
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
  } deriving stock (Eq, Show, Generic)

-- | Decision passed by a callout hook to resume VM evaluation.
data CalloutDecision
  = ProceedMatch      -- ^ Accept callout and proceed matching (advancing pc)
  | ForceBacktrack    -- ^ Reject callout and trigger backtracking to next branch
  | AbortEvaluation   -- ^ Abort regex evaluation immediately with NoMatch
  deriving stock (Eq, Show, Generic)

-- | Inspection payload passed to PCRE callout handlers.
data CalloutPayload s = CalloutPayload
  { calloutNumber     :: !Int          -- ^ PCRE callout identifier (0 for (?C), n for (?Cn))
  , calloutTag        :: !(Maybe Text) -- ^ Optional string tag from (?C"tag") or (?C'tag')
  , calloutPosition   :: !Int          -- ^ Current 0-based character offset in target
  , calloutMatchStart :: !Int          -- ^ 0-based character offset where current match began
  , calloutCaptures   :: !CaptureSlots -- ^ Current snapshot of capture group registers
  , calloutSubject    :: !s            -- ^ Entire target sequence being searched
  } deriving stock (Eq, Show, Generic)

{- Note [PCRE Callout Resumption and Continuation Hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-pcre-callout-resumption]
PCRE callouts ((?C), (?Cn), (?C"tag")) provide deterministic hooks into the
regular expression search process. Smirk models callouts as first-class suspended
continuations: when the engine hits 'OpCallout', it yields 'SuspendedCallout' containing
the inspection payload and a continuation function '(CalloutDecision -> MatchContinuation s)'.

The host application can inspect target offsets, capture registers, and tags, then choose:
1. 'ProceedMatch': increment the program counter past the callout and continue.
2. 'ForceBacktrack': fail the current branch and pop the next backtrack thread.
3. 'AbortEvaluation': terminate the search immediately with 'NoMatch'.
-}

-- | Verdict of a matching operation.
data MatchStatus s
  = Matched !(MatchResult s)            -- ^ A match was successfully found
  | NoMatch                             -- ^ The pattern does not match the target
  | OutOfFuel !(MatchContinuation s)    -- ^ Fuel budget exhausted; resumable
  | SuspendedCallout !(CalloutPayload s) !(CalloutDecision -> MatchContinuation s)
  deriving stock Generic

instance Eq s => Eq (MatchStatus s) where
  Matched r1 == Matched r2 = r1 == r2
  NoMatch == NoMatch = True
  OutOfFuel c1 == OutOfFuel c2 = c1 == c2
  SuspendedCallout p1 _ == SuspendedCallout p2 _ = p1 == p2
  _ == _ = False

instance Show s => Show (MatchStatus s) where
  showsPrec p = \case
    Matched r -> showParen (p > 10) $ showString "Matched " . showsPrec 11 r
    NoMatch -> showString "NoMatch"
    OutOfFuel c -> showParen (p > 10) $ showString "OutOfFuel " . showsPrec 11 c
    SuspendedCallout payload _ ->
      showParen (p > 10) $ showString "SuspendedCallout " . showsPrec 11 payload . showString " <continuation>"

{- Note [Multi-Resource Step Metrics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-step-metrics-multi-resource]
Rather than tracking only a scalar step counter, Smirk monitors multiple physical
execution dimensions to ensure ReDoS protection, memory accounting, and streaming telemetry:
* 'stepsConsumed': Total bytecode instruction steps executed.
* 'fuelRemaining': Remaining fuel budget (StepBudget or Unlimited).
* 'unitsScanned': Highest character position accessed in target input sequence.
* 'utf8BytesMaterialized': UTF-8 byte representation width of the scanned prefix.
* 'peakBacktrackDepth': Maximum depth attained by the backtracking stack.
* 'activeThreadCount': Number of live/pending threads at outcome suspension or termination.
-}

-- | Multi-resource execution metrics tracked during VM evaluation.
data StepMetrics = StepMetrics
  { stepsConsumed         :: !Int   -- ^ Total VM instruction transitions executed
  , fuelRemaining         :: !Fuel  -- ^ Remaining fuel budget
  , unitsScanned          :: !Int   -- ^ Maximum target units (characters) scanned
  , utf8BytesMaterialized :: !Int   -- ^ UTF-8 bytes represented by the scanned units
  , peakBacktrackDepth    :: !Int   -- ^ Maximum depth of the backtrack stack observed
  , activeThreadCount     :: !Int   -- ^ Number of active or pending threads at termination/suspension
  } deriving stock (Eq, Show, Generic)

-- | Initial zeroed 'StepMetrics' for a fresh execution run.
emptyStepMetrics :: Fuel -> StepMetrics
emptyStepMetrics fuel = StepMetrics
  { stepsConsumed         = 0
  , fuelRemaining         = fuel
  , unitsScanned          = 0
  , utf8BytesMaterialized = 0
  , peakBacktrackDepth    = 0
  , activeThreadCount     = 1
  }

-- | Complete match outcome pairing verdict with multi-resource metrics.
data MatchOutcome s = MatchOutcome
  { outcomeStatus  :: !(MatchStatus s)
  , outcomeMetrics :: !StepMetrics
  } deriving stock (Eq, Show, Generic)

-- | Construct a 'MatchOutcome' from a 'MatchStatus' and 'StepMetrics'.
mkMatchOutcome :: MatchStatus s -> StepMetrics -> MatchOutcome s
mkMatchOutcome = MatchOutcome

-- | Extract the 'MatchStatus' from an outcome.
outcomeToStatus :: MatchOutcome s -> MatchStatus s
outcomeToStatus = outcomeStatus

-- | Extract a 'MatchResult' if the outcome was successful.
outcomeToMaybeResult :: MatchOutcome s -> Maybe (MatchResult s)
outcomeToMaybeResult outcome = case outcome.outcomeStatus of
  Matched res -> Just res
  _           -> Nothing

{- Note [Pre-Allocated VM Frames and Thread Pools]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-preallocated-vm-frames]
In the Pike VM, backtrack branches represent alternative execution paths.
'VMFrame' packages the unboxed program counter and string index with flat 'CaptureSlots',
avoiding intermediate tuple allocations.
'VMFramePool' provides a flat, pre-allocated array buffer backed by 'PrimArray Int'
and 'SmallArray CaptureSlots' for high-throughput thread pool operations.
-}

-- | Pre-allocated frame representation for thread pool states.
-- Stores program counter, string index, and capture registers.
data VMFrame = VMFrame
  { framePc       :: {-# UNPACK #-} !Int
  , frameStrIdx   :: {-# UNPACK #-} !Int
  , frameCaptures :: !CaptureSlots
  } deriving stock (Eq, Show, Generic)

-- | Convert a 'ThreadState' to a 'VMFrame'.
threadToFrame :: ThreadState -> VMFrame
threadToFrame th = VMFrame th.pc th.strIdx th.saves

-- | Convert a 'VMFrame' back to a 'ThreadState'.
frameToThread :: VMFrame -> ThreadState
frameToThread fr = ThreadState fr.framePc fr.frameStrIdx fr.frameCaptures

-- | Pre-allocated thread pool frame buffer for backtrack branches.
-- Uses unboxed 'PrimArray Int' for (pc, strIdx) pairs and 'SmallArray CaptureSlots'.
data VMFramePool = VMFramePool
  { poolPcs      :: !(PrimArray Int)
  , poolStrIdxs  :: !(PrimArray Int)
  , poolCaptures :: !(SmallArray CaptureSlots)
  , poolCapacity :: {-# UNPACK #-} !Int
  } deriving stock (Eq, Show, Generic)

-- | Initialize a pre-allocated 'VMFramePool' with the given capacity.
emptyVMFramePool :: Int -> VMFramePool
emptyVMFramePool cap =
  let safeCap = max 1 cap
      pcs = runST $ do
        m <- newPrimArray safeCap
        setPrimArray m 0 safeCap (-1)
        unsafeFreezePrimArray m
      strIdxs = runST $ do
        m <- newPrimArray safeCap
        setPrimArray m 0 safeCap 0
        unsafeFreezePrimArray m
      caps = runSmallArray (newSmallArray safeCap (emptyCaptureSlots 0))
  in VMFramePool pcs strIdxs caps safeCap

-- | Read a frame from the frame pool at the given index.
readFrame :: VMFramePool -> Int -> VMFrame
readFrame pool idx =
  VMFrame
    (indexPrimArray pool.poolPcs idx)
    (indexPrimArray pool.poolStrIdxs idx)
    (indexSmallArray pool.poolCaptures idx)

-- | Convert a list of 'ThreadState' frames into a pre-allocated 'VMFramePool'.
threadsToFramePool :: [ThreadState] -> VMFramePool
threadsToFramePool ths =
  let len = length ths
      cap = max 16 len
      pcs = runST $ do
        m <- newPrimArray cap
        setPrimArray m 0 cap (-1)
        forM_ (zip [0..] ths) $ \(i, th) -> writePrimArray m i th.pc
        unsafeFreezePrimArray m
      strIdxs = runST $ do
        m <- newPrimArray cap
        setPrimArray m 0 cap 0
        forM_ (zip [0..] ths) $ \(i, th) -> writePrimArray m i th.strIdx
        unsafeFreezePrimArray m
      caps = runSmallArray $ do
        m <- newSmallArray cap (emptyCaptureSlots 0)
        forM_ (zip [0..] ths) $ \(i, th) -> writeSmallArray m i th.saves
        pure m
  in VMFramePool pcs strIdxs caps cap

-- | Extract active 'ThreadState' list from a 'VMFramePool' up to depth.
framePoolToThreads :: VMFramePool -> Int -> [ThreadState]
framePoolToThreads pool depth =
  [ frameToThread (readFrame pool i)
  | i <- [0 .. depth - 1]
  ]
