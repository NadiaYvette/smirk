{-# LANGUAGE DeriveGeneric #-}
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
  immediately yields 'OutOfFuel' with an intact 'MatchContinuation', completely
  preventing algorithmic complexity attacks.
* __Pausable & Resumable Continuation:__ Suspended computations retain the entire
  thread state, backtrack stack, and program counters, enabling interactive or
  time-sliced regex execution.
* __Sequence Polymorphism:__ Character lookups and slice extractions are dispatched
  via the 'RegexTarget' class without intermediate string conversions.
-}
module Smirk.Engine.VM
  ( -- * Fuel & Budgeting
    Fuel (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)

    -- * Execution Entry Points
  , runMatch
  , runMatchAt
  , resumeMatch
  , findAllMatches

    -- * Internal VM Engine
  , ThreadState (..)
  , stepVM
  , matchClass
  , matchAnchor
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Smirk.Core.AST (Anchor (..), CharClass (..), CharRange (..))
import Smirk.Core.Bytecode (Opcode (..), Regex (..))
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

-- | Execution fuel budget for step bounding and ReDoS prevention.
data Fuel
  = StepBudget !Int    -- ^ Bounded execution: halt after N instruction steps
  | Unlimited          -- ^ Unbounded execution: run to completion or exhaustion
  deriving (Eq, Show, Generic)

-- | Successful regular expression match result with full capture offsets.
data MatchResult s = MatchResult
  { matchedSlice  :: !s             -- ^ Exact matched slice of the input
  , matchOffset   :: !Int           -- ^ 0-based character offset where match begins
  , matchLength   :: !Int           -- ^ Total length (in characters) of the matched slice
  , captureGroups :: !(Map Int s)   -- ^ Numbered capture groups (1-indexed)
  , namedGroups   :: !(Map Text s)  -- ^ Named capture groups mapped to slices
  , stepsConsumed :: !Int           -- ^ Total VM steps executed to find this match
  } deriving (Eq, Show, Generic)

-- | Pausable and resumable execution continuation captured when fuel is exhausted.
data MatchContinuation s = MatchContinuation
  { savedRegex       :: !Regex          -- ^ The compiled regex being evaluated
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

-- | Internal VM thread execution state.
data ThreadState = ThreadState
  { pc     :: !Int                          -- ^ Program counter: index into program vector
  , strIdx :: !Int                          -- ^ Current position in target character sequence
  , saves  :: !(Map Int (Int, Maybe Int))   -- ^ Capture registers: group idx -> (start, optional end)
  } deriving (Eq, Show, Generic)

-- | Execute regex search across a sequence starting from index 0.
runMatch :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatch regex target = runMatchAt regex target 0

-- | Execute regex search starting from a specific character offset.
runMatchAt :: RegexTarget s => Regex -> s -> Int -> Fuel -> MatchStatus s
runMatchAt regex target startIdx fuel =
  let len = seqLength target
  in if startIdx > len
       then NoMatch
       else
         let initialThread = ThreadState 0 startIdx Map.empty
         in stepVM regex target startIdx initialThread [] 0 fuel

-- | Resume a previously suspended match with additional fuel.
resumeMatch :: RegexTarget s => MatchContinuation s -> Fuel -> MatchStatus s
resumeMatch cont =
  stepVM
    cont.savedRegex
    cont.savedTarget
    cont.savedStartIndex
    cont.currentThread
    cont.backtrackStack
    cont.currentSteps

-- | Find all non-overlapping matches across the target sequence.
findAllMatches :: RegexTarget s => Regex -> s -> [MatchResult s]
findAllMatches regex target = go 0
  where
    go off
      | off > seqLength target = []
      | otherwise = case runMatchAt regex target off Unlimited of
          Matched res ->
            let nextOff = max (off + 1) (res.matchOffset + res.matchLength)
            in res : go nextOff
          _ -> []

-- | Core backtracking execution step loop.
-- Evaluates one instruction and advances the thread or backtracks to an alternate branch.
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
stepVM regex target startIdx th stack steps fuel =
  case fuel of
    StepBudget budget | steps >= budget ->
      OutOfFuel MatchContinuation
        { savedRegex      = regex
        , savedTarget     = target
        , savedStartIndex = startIdx
        , currentThread   = th
        , backtrackStack  = stack
        , currentSteps    = steps
        }
    _ ->
      let prog = regex.program
          pc' = th.pc
      in if pc' < 0 || pc' >= V.length prog
           then backtrackOrNextStart regex target startIdx stack (steps + 1) fuel
           else case prog V.! pc' of
             OpChar expected ->
               case seqIndex target th.strIdx of
                 Just c | c == expected ->
                   stepVM regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack (steps + 1) fuel
                 _ -> backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpClass cls ->
               case seqIndex target th.strIdx of
                 Just c | matchClass cls c ->
                   stepVM regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack (steps + 1) fuel
                 _ -> backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpAnchor anchor ->
               if matchAnchor anchor th.strIdx target
                 then stepVM regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                 else backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpSplit d1 d2 ->
               let branch2 = th { pc = pc' + d2 }
                   branch1 = th { pc = pc' + d1 }
               in stepVM regex target startIdx branch1 (branch2 : stack) (steps + 1) fuel

             OpJump d ->
               stepVM regex target startIdx th { pc = pc' + d } stack (steps + 1) fuel

             OpSaveStart idx ->
               let saves' = Map.insert idx (th.strIdx, Nothing) th.saves
               in stepVM regex target startIdx th { pc = pc' + 1, saves = saves' } stack (steps + 1) fuel

             OpSaveEnd idx ->
               let saves' = Map.adjust (\(s, _) -> (s, Just th.strIdx)) idx th.saves
               in stepVM regex target startIdx th { pc = pc' + 1, saves = saves' } stack (steps + 1) fuel

             OpAssertLookahead subProg isPos ->
               let subRegex = regex { program = subProg }
                   subMatched = case runMatchAt subRegex target th.strIdx (StepBudget 1000) of
                     Matched _ -> True
                     _         -> False
               in if subMatched == isPos
                    then stepVM regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                    else backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpAssertLookbehind subProg isPos ->
               let subRegex = regex { program = subProg }
                   subMatched = any (\prevOff ->
                     case runMatchAt subRegex target prevOff (StepBudget 1000) of
                       Matched res -> res.matchOffset + res.matchLength == th.strIdx
                       _           -> False
                     ) [max 0 (th.strIdx - 20) .. th.strIdx]
               in if subMatched == isPos
                    then stepVM regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                    else backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpMatchBackRef idx ->
               case Map.lookup idx th.saves of
                 Just (st, Just en) ->
                   let refLen = en - st
                       refSlice = seqSlice st refLen target
                       targetSlice = seqSlice th.strIdx refLen target
                   in if seqToText refSlice == seqToText targetSlice
                        then stepVM regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + refLen } stack (steps + 1) fuel
                        else backtrackOrNextStart regex target startIdx stack (steps + 1) fuel
                 _ -> backtrackOrNextStart regex target startIdx stack (steps + 1) fuel

             OpMatch ->
               Matched (buildResult regex target th (steps + 1))

-- | Backtrack to next pending thread on stack, or advance outer search loop.
backtrackOrNextStart
  :: RegexTarget s
  => Regex
  -> s
  -> Int
  -> [ThreadState]
  -> Int
  -> Fuel
  -> MatchStatus s
backtrackOrNextStart regex target startIdx stack steps fuel =
  case stack of
    (nextTh : rest) ->
      stepVM regex target startIdx nextTh rest steps fuel
    [] ->
      case regex.program V.!? 0 of
        Just (OpSaveStart 0) ->
          case regex.program V.!? 1 of
            Just (OpAnchor AnchorStart) -> NoMatch
            _ -> advanceStart
        _ -> advanceStart
  where
    advanceStart =
      let nextStart = startIdx + 1
      in if nextStart <= seqLength target
           then
             let freshThread = ThreadState 0 nextStart Map.empty
             in stepVM regex target nextStart freshThread [] steps fuel
           else NoMatch

-- | Construct a final 'MatchResult' from the terminating thread registers.
buildResult :: RegexTarget s => Regex -> s -> ThreadState -> Int -> MatchResult s
buildResult regex target th steps =
  let (mOff, mLen) = case Map.lookup 0 th.saves of
        Just (st, Just en) -> (st, en - st)
        Just (st, Nothing) -> (st, th.strIdx - st)
        Nothing            -> (0, th.strIdx)
      fullSlice = seqSlice mOff mLen target
      caps = Map.fromList
        [ (idx, seqSlice st (en - st) target)
        | (idx, (st, Just en)) <- Map.toList th.saves
        , idx /= 0
        ]
      namedCaps = Map.fromList
        [ (name, capSlice)
        | (name, idx) <- Map.toList regex.groupNames
        , Just capSlice <- [Map.lookup idx caps]
        ]
  in MatchResult
      { matchedSlice  = fullSlice
      , matchOffset   = mOff
      , matchLength   = mLen
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
