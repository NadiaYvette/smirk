{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : Smirk.Engine.VM
Description : Resumable, fuel-budgeted Pike-style regex virtual machine with ReDoS defense
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Implements the bytecode virtual machine for Smirk. The engine executes a Pike-style
nondeterministic state machine with an explicit backtracking continuation stack.

=== Architectural Invariants

* __ReDoS Immunity via Step-Bounding:__ Every instruction transition increments
  the consumed step counter. When running under 'StepBudget', exceeding the limit
  immediately yields 'OutOfFuel' with an intact 't:MatchContinuation', completely
  preventing algorithmic complexity attacks.
* __Pausable & Resumable Continuation:__ Suspended computations retain the entire
  thread state, backtrack stack, and program counters, enabling interactive or
  time-sliced regex execution.
* __Sequence Polymorphism:__ Character lookups and slice extractions are dispatched
  via the 'RegexTarget' class without intermediate string conversions.
* __Flat Unboxed Capture Registers:__ Capture group boundaries are tracked in flat
  unboxed 'CaptureSlots' ('Data.Primitive.PrimArray.PrimArray Int') rather than
  pointer-chasing tree maps, per [ref:note-pike-vm-flat-captures].
-}
module Smirk.Engine.VM
  ( -- * Fuel & Budgeting
    Fuel (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)

    -- * Multi-Resource Step Metrics & Match Outcomes
  , StepMetrics (..)
  , emptyStepMetrics
  , MatchOutcome (..)
  , mkMatchOutcome
  , outcomeToStatus
  , outcomeToMaybeResult

    -- * PCRE Callouts
  , CalloutDecision (..)
  , CalloutPayload (..)

    -- * Execution Modes
  , MatchMode (..)

    -- * Execution Entry Points with Multi-Resource Metrics
  , runMatchWithMetrics
  , runMatchWithMetricsAt
  , runMatchWithMetricsWithMode
  , runMatchWithMetricsAtWithMode
  , resumeMatchWithMetrics

    -- * Backwards-Compatible Execution Entry Points
  , runMatch
  , runMatchAt
  , runMatchWithMode
  , runMatchAtWithMode
  , runMatchNotEmpty
  , execMatchNotEmpty
  , resumeMatch
  , findAllMatches
  , findAllMatchesWithMode
  , findAllMatchesNotEmpty
  , runAutoProceed

    -- * Internal VM Engine & Pre-Allocated Frames
  , ThreadState (..)
  , VMFrame (..)
  , threadToFrame
  , frameToThread
  , VMFramePool (..)
  , emptyVMFramePool
  , readFrame
  , threadsToFramePool
  , framePoolToThreads
  , CaptureSlots (..)
  , emptyCaptureSlots
  , getCaptureStart
  , getCaptureEnd
  , getCaptureSpan
  , setCaptureStart
  , setCaptureEnd
  , toCaptureMap
  , toFullMatchSpan
  , toNamedCaptureSmallArray
  , toNamedCaptureRep
  , matchNamedCaptureRep
  , stepVM
  , stepVMWithMode
  , stepVMWithMetrics
  , backtrackOrNextStart
  , backtrackOrNextStartWithMetrics
  , scanWord64
  , matchClass
  , matchAnchor
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word (Word64)

import Smirk.Core.AST (Anchor (..), CharClass (..), CharRange (..))
import Smirk.Core.Bytecode (Opcode (..), Regex (..))
import Smirk.Core.Types
  ( CalloutDecision (..)
  , CalloutPayload (..)
  , CaptureSlots (..)
  , Fuel (..)
  , MatchContinuation (..)
  , MatchMode (..)
  , MatchOutcome (..)
  , MatchResult (..)
  , MatchStatus (..)
  , StepMetrics (..)
  , ThreadState (..)
  , VMFrame (..)
  , VMFramePool (..)
  , emptyCaptureSlots
  , emptyStepMetrics
  , emptyVMFramePool
  , framePoolToThreads
  , frameToThread
  , getCaptureEnd
  , getCaptureSpan
  , getCaptureStart
  , lookupNamedCaptureRep
  , matchNamedCaptureRep
  , mkMatchOutcome
  , outcomeToMaybeResult
  , outcomeToStatus
  , readFrame
  , setCaptureEnd
  , setCaptureStart
  , threadToFrame
  , threadsToFramePool
  , toCaptureMap
  , toFullMatchSpan
  , toNamedCaptureRep
  , toNamedCaptureSmallArray
  )
import Smirk.Engine.Sequence (RegexTarget (..))

{- Note [Pike VM State Machine and Backtracking Stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-pike-vm-state-machine]
Smirk's execution engine operates as a bytecode virtual machine. The compiled
program comprises instructions like OpChar, OpSplit, OpJump, OpSaveStart, and
OpSaveEnd.

Nondeterminism from repetitions (.*, .+) and alternations (a|b) is modeled
via OpSplit d1 d2. Branch 1 is pursued immediately while Branch 2 is pushed
onto an explicit backtrack stack. If a thread encounters a character mismatch,
it pops the next alternate thread from the stack.

Unlike traditional recursive backtracking which consumes unbounded C call stack
frames and exposes the runtime to stack overflows, Smirk stores thread states in
an explicit heap-allocated list [ThreadState], bounded by the step fuel.
-}

{- Note [Fuel-Bounded Step Budget and ReDoS Defense]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[tag:note-fuel-step-budget]
Regular Expression Denial of Service (ReDoS) occurs when malicious or pathological
inputs trigger superlinear (e.g. O(2^N)) backtracking iterations over ambiguous
grammars like (a+)+$.

To neutralize this threat, Smirk introduces the 'Fuel' budget:
1. Every iteration of 'stepVM' consumes exactly 1 unit of fuel.
2. If steps >= budget, the machine halts immediately and packages the active thread
    and backtrack stack into 'OutOfFuel (MatchContinuation ...)'.
3. The host application can inspect the partial progress, abort, or allocate more
    fuel to resume via 'resumeMatch'.
-}

-- | Context tracking step counts and physical resource dimensions during matching.
data VMContext = VMContext
  { vmStepsConsumed :: {-# UNPACK #-} !Int
  , vmUnitsScanned  :: {-# UNPACK #-} !Int
  , vmPeakDepth     :: {-# UNPACK #-} !Int
  } deriving stock (Eq, Show)

-- | Construct 'StepMetrics' snapshot from 'VMContext' and fuel budget.
buildStepMetrics
  :: RegexTarget s
  => Fuel
  -> s
  -> VMContext
  -> Int        -- ^ Number of active/pending threads
  -> StepMetrics
buildStepMetrics fuel target ctx activeThreads =
  let remFuel = case fuel of
        Unlimited    -> Unlimited
        StepBudget b -> StepBudget (max 0 (b - ctx.vmStepsConsumed))
      scanned = ctx.vmUnitsScanned
      bytes = utf8SliceByteWidth 0 scanned target
  in StepMetrics
    { stepsConsumed         = ctx.vmStepsConsumed
    , fuelRemaining         = remFuel
    , unitsScanned          = scanned
    , utf8BytesMaterialized = bytes
    , peakBacktrackDepth    = ctx.vmPeakDepth
    , activeThreadCount     = activeThreads
    }

-- | Execute regex search across a sequence starting from index 0 returning multi-resource 'StepMetrics'.
runMatchWithMetrics :: RegexTarget s => Regex -> s -> Fuel -> MatchOutcome s
runMatchWithMetrics = runMatchWithMetricsWithMode MatchStandard

-- | Execute regex search starting from a specific character offset returning multi-resource 'StepMetrics'.
runMatchWithMetricsAt :: RegexTarget s => Regex -> s -> Int -> Fuel -> MatchOutcome s
runMatchWithMetricsAt = runMatchWithMetricsAtWithMode MatchStandard

-- | Execute regex search across a sequence under an explicit 'MatchMode' returning multi-resource 'StepMetrics'.
runMatchWithMetricsWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Fuel -> MatchOutcome s
runMatchWithMetricsWithMode mode regex target = runMatchWithMetricsAtWithMode mode regex target 0

-- | Execute regex search starting from a specific offset under an explicit 'MatchMode' returning 'StepMetrics'.
runMatchWithMetricsAtWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Int -> Fuel -> MatchOutcome s
runMatchWithMetricsAtWithMode mode regex target startIdx fuel =
  let len = seqLength target
  in if startIdx > len
       then MatchOutcome NoMatch (emptyStepMetrics fuel)
       else
         let initialThread = ThreadState 0 startIdx (emptyCaptureSlots (regex.maxGroupIndex + 1))
             ctx = VMContext 0 startIdx 0
         in stepVMWithMetrics mode regex target startIdx initialThread [] ctx fuel

-- | Resume a previously suspended match with additional fuel, returning multi-resource 'StepMetrics'.
resumeMatchWithMetrics :: RegexTarget s => MatchContinuation s -> Fuel -> MatchOutcome s
resumeMatchWithMetrics cont fuel =
  let ctx = VMContext
        cont.currentSteps
        (max cont.savedStartIndex cont.currentThread.strIdx)
        (length cont.backtrackStack)
  in stepVMWithMetrics
       cont.savedMatchMode
       cont.savedRegex
       cont.savedTarget
       cont.savedStartIndex
       cont.currentThread
       cont.backtrackStack
       ctx
       fuel

-- | Execute regex search across a sequence starting from index 0 using standard matching.
runMatch :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatch reg target fuel = (runMatchWithMetrics reg target fuel).outcomeStatus

-- | Execute regex search starting from a specific character offset using standard matching.
runMatchAt :: RegexTarget s => Regex -> s -> Int -> Fuel -> MatchStatus s
runMatchAt reg target startIdx fuel = (runMatchWithMetricsAt reg target startIdx fuel).outcomeStatus

-- | Execute regex search across a sequence under an explicit 'MatchMode'.
runMatchWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Fuel -> MatchStatus s
runMatchWithMode mode regex target fuel = (runMatchWithMetricsWithMode mode regex target fuel).outcomeStatus

-- | Execute regex search starting from a specific character offset under an explicit 'MatchMode'.
runMatchAtWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Int -> Fuel -> MatchStatus s
runMatchAtWithMode mode regex target startIdx fuel = (runMatchWithMetricsAtWithMode mode regex target startIdx fuel).outcomeStatus

-- | Repeatedly resolve 'SuspendedCallout' hooks with 'ProceedMatch' until
-- achieving a definitive match verdict ('Matched' or 'NoMatch' / 'OutOfFuel').
runAutoProceed :: RegexTarget s => MatchStatus s -> Maybe (MatchResult s)
runAutoProceed = \case
  Matched res -> Just res
  SuspendedCallout _ resume -> runAutoProceed (resumeMatch (resume ProceedMatch) Unlimited)
  _ -> Nothing

-- | Execute regex search enforcing 'MatchNotEmpty' (PCRE2_NOTEMPTY semantics: rejects empty matches).
runMatchNotEmpty :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatchNotEmpty = runMatchWithMode MatchNotEmpty

-- | Execute non-empty regex search returning the first non-empty match, if any.
execMatchNotEmpty :: RegexTarget s => Regex -> s -> Maybe (MatchResult s)
execMatchNotEmpty reg target = runAutoProceed (runMatchNotEmpty reg target Unlimited)

-- | Resume a previously suspended match with additional fuel, preserving its 'MatchMode'.
resumeMatch :: RegexTarget s => MatchContinuation s -> Fuel -> MatchStatus s
resumeMatch cont fuel = (resumeMatchWithMetrics cont fuel).outcomeStatus

-- | Find all non-overlapping matches across the target sequence in standard matching mode.
findAllMatches :: RegexTarget s => Regex -> s -> [MatchResult s]
findAllMatches = findAllMatchesWithMode MatchStandard

-- | Find all non-overlapping matches under an explicit 'MatchMode'.
findAllMatchesWithMode :: RegexTarget s => MatchMode -> Regex -> s -> [MatchResult s]
findAllMatchesWithMode mode regex target = go 0
  where
    go off
      | off > seqLength target = []
      | otherwise = case runAutoProceed (runMatchAtWithMode mode regex target off Unlimited) of
          Just res ->
            let nextOff = max (off + 1) (res.matchOffset + res.matchLength)
            in res : go nextOff
          Nothing -> []

-- | Find all non-overlapping matches enforcing non-empty match lengths (matchLength >= 1).
findAllMatchesNotEmpty :: RegexTarget s => Regex -> s -> [MatchResult s]
findAllMatchesNotEmpty = findAllMatchesWithMode MatchNotEmpty

-- | Core backtracking execution step loop under standard matching mode.
stepVM
  :: RegexTarget s
  => Regex
  -> s
  -> Int               -- ^ Outer loop start index
  -> ThreadState       -- ^ Current active thread
  -> [ThreadState]     -- ^ Backtrack stack
  -> Int               -- ^ Steps consumed so far
  -> Fuel              -- ^ Fuel limit
  -> MatchStatus s
stepVM = stepVMWithMode MatchStandard

-- | Core backtracking execution step loop under an explicit 'MatchMode'.
stepVMWithMode
  :: RegexTarget s
  => MatchMode         -- ^ Execution mode (standard or non-empty)
  -> Regex
  -> s
  -> Int               -- ^ Outer loop start index
  -> ThreadState       -- ^ Current active thread
  -> [ThreadState]     -- ^ Backtrack stack
  -> Int               -- ^ Steps consumed so far
  -> Fuel              -- ^ Fuel limit
  -> MatchStatus s
stepVMWithMode mode regex target startIdx th stack steps fuel =
  let ctx = VMContext steps (max startIdx th.strIdx) (length stack)
      outcome = stepVMWithMetrics mode regex target startIdx th stack ctx fuel
  in outcome.outcomeStatus

-- | Core multi-resource step loop.
-- Evaluates one instruction and advances the thread or backtracks to an alternate branch.
stepVMWithMetrics
  :: RegexTarget s
  => MatchMode
  -> Regex
  -> s
  -> Int               -- ^ Outer loop start index
  -> ThreadState       -- ^ Current active thread
  -> [ThreadState]     -- ^ Backtrack stack
  -> VMContext         -- ^ Step metrics accumulator
  -> Fuel              -- ^ Fuel limit
  -> MatchOutcome s
stepVMWithMetrics mode regex target startIdx th stack ctx fuel =
  case fuel of
    StepBudget budget | ctx.vmStepsConsumed >= budget ->
      let metrics = buildStepMetrics fuel target ctx (1 + length stack)
          cont = MatchContinuation
            { savedMatchMode  = mode
            , savedRegex      = regex
            , savedTarget     = target
            , savedStartIndex = startIdx
            , currentThread   = th
            , backtrackStack  = stack
            , currentSteps    = ctx.vmStepsConsumed
            }
      in MatchOutcome (OutOfFuel cont) metrics
    _ ->
      let !steps' = ctx.vmStepsConsumed + 1
          !units' = max ctx.vmUnitsScanned th.strIdx
          !depth' = max ctx.vmPeakDepth (length stack)
          !ctx' = VMContext steps' units' depth'
          prog = regex.program
          pc' = th.pc
      in if pc' < 0 || pc' >= V.length prog
           then backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel
           else case prog V.! pc' of
             OpChar expected ->
               case seqIndex target th.strIdx of
                 Just c | c == expected ->
                   let !unitsScanned' = max units' (th.strIdx + 1)
                   in stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack ctx' { vmUnitsScanned = unitsScanned' } fuel
                 _ -> backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpClass cls ->
               case seqIndex target th.strIdx of
                 Just c | matchClass cls c ->
                   let !unitsScanned' = max units' (th.strIdx + 1)
                   in stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack ctx' { vmUnitsScanned = unitsScanned' } fuel
                 _ -> backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpAnchor anchor ->
               if matchAnchor anchor th.strIdx target
                 then stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1 } stack ctx' fuel
                 else backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpSplit d1 d2 ->
               let branch2 = th { pc = pc' + d2 }
                   branch1 = th { pc = pc' + d1 }
                   !depthAfterSplit = max depth' (1 + length stack)
                   !ctxSplit = ctx' { vmPeakDepth = depthAfterSplit }
               in stepVMWithMetrics mode regex target startIdx branch1 (branch2 : stack) ctxSplit fuel

             OpJump d ->
               stepVMWithMetrics mode regex target startIdx th { pc = pc' + d } stack ctx' fuel

             OpSaveStart idx ->
               let !saves' = setCaptureStart idx th.strIdx th.saves
               in stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, saves = saves' } stack ctx' fuel

             OpSaveEnd idx ->
               let !saves' = setCaptureEnd idx th.strIdx th.saves
               in stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, saves = saves' } stack ctx' fuel

             OpAssertLookahead subProg isPos ->
               let subRegex = regex { program = subProg }
                   subOutcome = runMatchAtWithMode MatchStandard subRegex target th.strIdx (StepBudget 1000)
                   subMatched = case subOutcome of
                     Matched _ -> True
                     _         -> False
               in if subMatched == isPos
                    then stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1 } stack ctx' fuel
                    else backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpAssertLookbehind subProg isPos ->
               let subRegex = regex { program = subProg }
                   subMatched = any (\prevOff ->
                     case runMatchAtWithMode MatchStandard subRegex target prevOff (StepBudget 1000) of
                       Matched res -> res.matchOffset + res.matchLength == th.strIdx
                       _           -> False
                     ) [max 0 (th.strIdx - 20) .. th.strIdx]
               in if subMatched == isPos
                    then stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1 } stack ctx' fuel
                    else backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpMatchBackRef idx ->
               case getCaptureSpan idx th.saves of
                 Just (st, en) ->
                   let refLen = en - st
                       refSlice = seqSlice st refLen target
                       targetSlice = seqSlice th.strIdx refLen target
                       !unitsScanned' = max units' (th.strIdx + refLen)
                   in if seqToText refSlice == seqToText targetSlice
                        then stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + refLen } stack ctx' { vmUnitsScanned = unitsScanned' } fuel
                        else backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel
                 _ -> backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpRangeNat minVal maxVal ->
               case scanWord64 target th.strIdx of
                 (Just val, !len) | val >= minVal && val <= maxVal ->
                   let !unitsScanned' = max units' (th.strIdx + len)
                   in stepVMWithMetrics mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + len } stack ctx' { vmUnitsScanned = unitsScanned' } fuel
                 _ -> backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel

             OpCallout num tag ->
               -- PCRE Callout: construct inspection payload and resumption hook [ref:note-pcre-callout-resumption]
               let payload = CalloutPayload
                     { calloutNumber     = num
                     , calloutTag        = tag
                     , calloutPosition   = th.strIdx
                     , calloutMatchStart = startIdx
                     , calloutCaptures   = th.saves
                     , calloutSubject    = target
                     }
                   resumeCont decision = case decision of
                     ProceedMatch ->
                       MatchContinuation
                         { savedMatchMode   = mode
                         , savedRegex       = regex
                         , savedTarget      = target
                         , savedStartIndex  = startIdx
                         , currentThread    = th { pc = pc' + 1 }
                         , backtrackStack   = stack
                         , currentSteps     = steps'
                         }
                     ForceBacktrack ->
                       case stack of
                         (nextTh : rest) ->
                           MatchContinuation
                             { savedMatchMode   = mode
                             , savedRegex       = regex
                             , savedTarget      = target
                             , savedStartIndex  = startIdx
                             , currentThread    = nextTh
                             , backtrackStack   = rest
                             , currentSteps     = steps'
                             }
                         [] ->
                           let nextStart = startIdx + 1
                               freshTh = ThreadState 0 nextStart (emptyCaptureSlots (regex.maxGroupIndex + 1))
                           in MatchContinuation
                             { savedMatchMode   = mode
                             , savedRegex       = regex
                             , savedTarget      = target
                             , savedStartIndex  = nextStart
                             , currentThread    = freshTh
                             , backtrackStack   = []
                             , currentSteps     = steps'
                             }
                     AbortEvaluation ->
                       MatchContinuation
                         { savedMatchMode   = mode
                         , savedRegex       = regex
                         , savedTarget      = target
                         , savedStartIndex  = seqLength target + 1
                         , currentThread    = ThreadState (V.length prog + 1) (seqLength target + 1) (emptyCaptureSlots 0)
                         , backtrackStack   = []
                         , currentSteps     = steps'
                         }
                   metrics = buildStepMetrics fuel target ctx' (1 + length stack)
               in MatchOutcome (SuspendedCallout payload resumeCont) metrics

             OpMatch ->
               let res = buildResult regex target th steps'
               in case mode of
                 MatchNotEmpty | res.matchLength == 0 ->
                   -- Reject empty match under PCRE2_NOTEMPTY semantics: backtrack!
                   backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx' fuel
                 _ ->
                   let metrics = buildStepMetrics fuel target ctx' 0
                   in MatchOutcome (Matched res) metrics

-- | Backtrack to next pending thread on stack, or advance outer search loop.
backtrackOrNextStart
  :: RegexTarget s
  => MatchMode
  -> Regex
  -> s
  -> Int
  -> [ThreadState]
  -> Int
  -> Fuel
  -> MatchStatus s
backtrackOrNextStart mode regex target startIdx stack steps fuel =
  let ctx = VMContext steps startIdx (length stack)
      outcome = backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx fuel
  in outcome.outcomeStatus

-- | Backtrack to next pending thread on stack, or advance outer search loop with metrics.
backtrackOrNextStartWithMetrics
  :: RegexTarget s
  => MatchMode
  -> Regex
  -> s
  -> Int
  -> [ThreadState]
  -> VMContext
  -> Fuel
  -> MatchOutcome s
backtrackOrNextStartWithMetrics mode regex target startIdx stack ctx fuel =
  case stack of
    (nextTh : rest) ->
      stepVMWithMetrics mode regex target startIdx nextTh rest ctx fuel
    [] ->
      case regex.program V.!? 0 of
        Just (OpSaveStart 0) ->
          case regex.program V.!? 1 of
            Just (OpAnchor AnchorStart) ->
              let metrics = buildStepMetrics fuel target ctx 0
              in MatchOutcome NoMatch metrics
            _ -> advanceStart
        _ -> advanceStart
  where
    advanceStart =
      let nextStart = startIdx + 1
      in if nextStart <= seqLength target
           then
             let freshThread = ThreadState 0 nextStart (emptyCaptureSlots (regex.maxGroupIndex + 1))
                 !units' = max ctx.vmUnitsScanned nextStart
             in stepVMWithMetrics mode regex target nextStart freshThread [] ctx { vmUnitsScanned = units' } fuel
           else
             let metrics = buildStepMetrics fuel target ctx 0
             in MatchOutcome NoMatch metrics

-- | Construct a final 't:MatchResult' from the terminating thread registers.
buildResult :: RegexTarget s => Regex -> s -> ThreadState -> Int -> MatchResult s
buildResult regex target th steps =
  let (mOff, mLen) = case getCaptureStart 0 th.saves of
        Just st -> case getCaptureEnd 0 th.saves of
          Just en -> (st, en - st)
          Nothing -> (st, th.strIdx - st)
        Nothing -> (0, th.strIdx)
      fullSlice = seqSlice mOff mLen target
      caps = toCaptureMap th.saves target
      namedCaps = Map.fromList
        [ (name, capSlice)
        | (name, idx) <- Map.toList regex.groupNames
        , Just capSlice <- [Map.lookup idx caps]
        ]
  in MatchResult
      { matchedSlice  = fullSlice
      , matchOffset   = mOff
      , matchLength   = mLen
      , captureSlots  = th.saves
      , captureGroups = caps
      , namedGroups   = namedCaps
      , stepsConsumed = steps
      }

-- | Test if a character satisfies a character class specification.
matchClass :: CharClass -> Char -> Bool
matchClass cls c = case cls of
  ClassAny -> c /= '\n'
  ClassDigit -> isDigit c
  ClassNotDigit -> not (isDigit c)
  ClassWord -> isAlphaNum c || c == '_'
  ClassNotWord -> not (isAlphaNum c || c == '_')
  ClassSpace -> isSpace c
  ClassNotSpace -> not (isSpace c)
  ClassSet ranges negated ->
    let inRanges = any (inRange c) ranges
    in if negated then not inRanges else inRanges
  where
    inRange ch (RangeSingle s) = ch == s
    inRange ch (RangeBetween s e) = ch >= s && ch <= e

-- | Assert if a boundary condition holds at a specific character position.
matchAnchor :: RegexTarget s => Anchor -> Int -> s -> Bool
matchAnchor anchor idx target = case anchor of
  AnchorStart -> idx == 0
  AnchorEnd   -> idx == seqLength target
  AnchorWordBoundary ->
    let prevIsWord = case seqIndex target (idx - 1) of
          Just p  -> isAlphaNum p || p == '_'
          Nothing -> False
        currIsWord = case seqIndex target idx of
          Just cur -> isAlphaNum cur || cur == '_'
          Nothing  -> False
    in prevIsWord /= currIsWord
  AnchorNotWordBoundary ->
    not (matchAnchor AnchorWordBoundary idx target)

-- | Scan contiguous ASCII decimal digits starting at offset 'start' and compute
-- the 64-bit unsigned natural number value and character length.
-- Returns @(Just val, length)@ on successful scan without overflow, or
-- @(Nothing, length)@ if no digits exist or overflow occurred.
-- Executes deterministically in $O(\text{digits})$ time with zero allocation.
-- [ref:note-lucene-interval-syntax]
scanWord64 :: RegexTarget s => s -> Int -> (Maybe Word64, Int)
scanWord64 target start = go start 0 0
  where
    go idx !acc !count =
      case seqIndex target idx of
        Just c | isDigit c ->
          let d = fromIntegral (ord c - ord '0') :: Word64
              overflow = acc > (maxBound - d) `div` 10
          in if overflow
               then (Nothing, count + 1 + countRest (idx + 1))
               else go (idx + 1) (acc * 10 + d) (count + 1)
        _ -> if count == 0 then (Nothing, 0) else (Just acc, count)

    countRest idx =
      case seqIndex target idx of
        Just c | isDigit c -> 1 + countRest (idx + 1)
        _                  -> 0
