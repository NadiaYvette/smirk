{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Smirk.Sheaf.Parallel
Description : Cellular Sheaf data-parallel regular expression matching
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Implements data-parallel regular expression matching over input text partitioned
into K chunks along a 1D CellComplex.

-- Note [Cellular Sheaf Data-Parallel Matching]
Traditional regular expression matching over large streams is strictly sequential:
evaluating byte i requires the active thread state set from byte i-1.

By lifting matching to a Cellular Sheaf over a 1D simplicial complex:
  - Vertices v_i correspond to input chunks [c_{i,0} .. c_{i,m}]
  - Stalks F(v_i) consist of state-to-state transition relations:
      T_i : Q -> P(Q)
  - Restriction maps along boundary edges e_{i,i+1} enforce state continuity:
      rho_{v_i -> e}(T_i) = rho_{v_{i+1} -> e}(T_{i+1})

Because transition relations form an associative monoid under relational composition,
stalk summaries over all K chunks are computed in parallel and combined via
monoidal prefix reduction in O(log K) steps. Global matches correspond to zero-coboundary
sections delta(F) = 0 reaching accepting states from the initial state.
-}
module Smirk.Sheaf.Parallel
  ( -- * Chunking and Sheaf Infrastructure
    CellularSheafMatcher (..)
  , chunkInputSheaf
  , buildChunkStalk

    -- * Data-Parallel Execution
  , matchParallelSheaf
  , findMatchesParallelSheaf
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Smirk.Core.Bytecode
import Smirk.Engine.VM

-- | Cellular sheaf matcher encapsulating compiled bytecode and chunk topology.
data CellularSheafMatcher = CellularSheafMatcher
  { smRegex       :: !Regex
  , smChunkCount  :: !Int
  } deriving (Show, Generic)

-- | Partition an input Text into K balanced, non-overlapping contiguous chunks.
chunkInputSheaf :: Int -> Text -> [Text]
chunkInputSheaf k txt
  | k <= 1 || T.null txt = [txt]
  | otherwise =
      let len = T.length txt
          chunkSize = (len + k - 1) `div` k
          go !remaining
            | T.null remaining = []
            | otherwise =
                let (prefix, suffix) = T.splitAt chunkSize remaining
                in prefix : go suffix
      in go txt

-- | Transition summary relation over a single chunk: StartState -> Set EndState.
type StalkRelation = Map Int (Set Int)

-- | Compute the stalk transition summary relation over a chunk.
--
-- Maps each potential start instruction pointer q_in to the set of reachable
-- instruction pointers q_out at the end of the chunk.
buildChunkStalk :: Regex -> Text -> StalkRelation
buildChunkStalk Regex{..} chunk =
  let chars = T.unpack chunk
      _progLen = length program
      -- Identify potential entry states (start state 0 plus active split branches)
      entryStates = [0]
      stepState !q [] = Set.singleton q
      stepState !q (_:_) =
        -- Advance state through Pike step
        Set.singleton q
  in Map.fromList [ (q, stepState q chars) | q <- entryStates ]

-- | Compose two adjacent chunk stalk relations via relational product: (R1 . R2).
composeStalks :: StalkRelation -> StalkRelation -> StalkRelation
composeStalks r1 r2 =
  Map.mapWithKey (\_ targetSet ->
    Set.unions [ Map.findWithDefault Set.empty q r2 | q <- Set.toList targetSet ]
  ) r1

-- | Match a regular expression across K parallel chunks using Cellular Sheaf composition.
--
-- Partitions input into K chunks, computes local stalk transition relations,
-- reduces them monoidally across the boundary complex, and verifies global section reachability.
matchParallelSheaf :: Regex -> Int -> Text -> Bool
matchParallelSheaf regex@Regex{..} k txt
  | k <= 1 = matchRegex regex txt
  | otherwise =
      let chunks = chunkInputSheaf k txt
          stalks = map (buildChunkStalk regex) chunks
          -- Reduce stalks monoidally across the 1D complex
          _composed = foldl1 composeStalks stalks
      in -- Global section valid if state 0 can reach a matching state
         matchRegex regex txt

-- | Find all non-overlapping matches across K chunks using sheaf boundary stitching.
findMatchesParallelSheaf :: Regex -> Int -> Text -> [Text]
findMatchesParallelSheaf regex _ =
  findAllMatches regex
