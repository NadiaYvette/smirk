{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Smirk.Engine.VM
  ( Fuel (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , resumeMatch
  , findAllMatches
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

-- | Execution fuel budget for step bounding and ReDoS prevention.
data Fuel
  = StepBudget !Int
  | Unlimited
  deriving (Eq, Show, Generic)

-- | Successful regular expression match result.
data MatchResult s = MatchResult
  { matchedSlice  :: !s
  , matchOffset   :: !Int
  , matchLength   :: !Int
  , captureGroups :: !(Map Int s)
  , namedGroups   :: !(Map Text s)
  , stepsConsumed :: !Int
  } deriving (Eq, Show, Generic)

-- | Pausable and resumable execution continuation.
data MatchContinuation s = MatchContinuation
  { savedRegex       :: !Regex
  , savedTarget      :: !s
  , savedStartIndex  :: !Int
  , currentThread    :: !ThreadState
  , backtrackStack   :: ![ThreadState]
  , currentSteps     :: !Int
  }

-- | Verdict of a matching operation.
data MatchStatus s
  = Matched !(MatchResult s)
  | NoMatch
  | OutOfFuel !(MatchContinuation s)

-- | Internal VM thread execution state.
data ThreadState = ThreadState
  { pc    :: !Int
  , strIdx :: !Int
  , saves  :: !(Map Int (Int, Maybe Int))
  } deriving (Eq, Show, Generic)

-- | Execute regex search across a sequence starting from index 0.
runMatch :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatch regex target fuel = runMatchAt regex target 0 fuel

-- | Execute regex search starting from a specific offset.
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
resumeMatch cont fuel =
  stepVM
    cont.savedRegex
    cont.savedTarget
    cont.savedStartIndex
    cont.currentThread
    cont.backtrackStack
    cont.currentSteps
    fuel

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
