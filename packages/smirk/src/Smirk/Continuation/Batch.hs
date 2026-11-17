{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Continuation.Batch
Description : Memory-bounded page and jumbo-frame match batching
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental

=== Memory-Bounded Page Batching
Note: memory-bounded-batching
High-throughput streaming match pipelines frequently produce thousands of discrete
'MatchResult' records. Standard 'findAllRegex' constructs a singly-linked heap list
(@[MatchResult s]@), which incurs a 2-word cons cell allocation per match and
exhibits poor L1/L2 cache locality when serialized or forwarded across network/disk boundaries.

'Smirk.Continuation.Batch' accumulates match results into unboxed/small arrays
('SmallArray (MatchResult s)'), packing items into discrete pages bounded by:
1. __Physical UTF-8 Byte Footprint:__ Maximum accumulated UTF-8 bytes (default: 4 KiB - 64 KiB),
   preventing jumbo frames from bloating socket transmit buffers.
2. __Maximum Item Count:__ Maximum matches per page (default: 1024), capping array allocation size.
3. __VM Step Quantum:__ Step bounding per evaluation slice to ensure bounded execution time.

=== Zero-Copy Page Finalization
Note: batch-zero-copy-finalization
Each page buffer is allocated in the 'Control.Monad.ST.ST' monad via 'newSmallArray',
populated sequentially in reverse accumulator order, and frozen with 'freezeSmallArray'
into an immutable 'SmallArray' without extra heap copying or pointer chasing.

=== Cache Locality and Memory Efficiency
Traditional linked lists incur significant memory overhead:
* Each list node allocates an algebraic cons constructor @(:)@ with head and tail pointers.
* Heap fragmentation increases under heavy GC pressure as matches are traversed.
* 'MatchBatch' stores matches contiguously in memory, reducing GC scanning overhead
  and enabling rapid vectorization when feeding downstream analytics or parquet writers.

=== ReDoS Defense and Slicing Discipline
Batching operates cooperatively with Smirk's fuel model:
* Every slice evaluation is bounded by 'bcStepQuantum'.
* If a single search step exceeds the quantum, intermediate state is bounded.
* Total steps across the entire batch sequence are accumulated in 'batchStepsConsumed'.

=== Byte Footprint Invariants
Note: batch-memory-footprint-invariants
To avoid underestimating memory consumption on multibyte text sequences (such as CJK,
emojis, or mathematical symbols), 'calcMatchByteFootprint' inspects every character in the
slice using 'utf8CharByteWidth':
* ASCII characters @[0x00 .. 0x7F]@ contribute 1 byte each.
* Latin-1 and Cyrillic extensions @[0x80 .. 0x07FF]@ contribute 2 bytes each.
* Basic Multilingual Plane characters @[0x0800 .. 0xFFFF]@ contribute 3 bytes each.
* Supplementary Plane characters (emojis) @[0x10000 .. 0x10FFFF]@ contribute 4 bytes each.
This ensures strict compliance with socket MTU boundaries and page limits.
-}
module Smirk.Continuation.Batch
  ( -- * Configuration
    BatchConfig (..)
  , defaultBatchConfig

    -- * Batch Data Structures
  , MatchBatch (..)
  , emptyMatchBatch

    -- * Batched Match Evaluator
  , findAllMatchesBatched
  , streamBatches
  ) where

import Control.Monad.ST (runST)
import Data.Primitive.SmallArray
  ( SmallArray
  , freezeSmallArray
  , newSmallArray
  , writeSmallArray
  )
import GHC.Generics (Generic)

import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Types
  ( Fuel (..)
  , MatchResult (..)
  , StepMetrics (..)
  )
import Smirk.Engine.Sequence (RegexTarget (..), utf8CharByteWidth)
import Smirk.Engine.VM
  ( MatchOutcome (..)
  , MatchStatus (..)
  , runMatchWithMetrics
  )

-- | Configuration parameters controlling the byte footprint and capacity bounds of match pages.
--
-- These parameters govern the trade-off between allocation granularity, memory footprint,
-- and batch serialization throughput.
--
-- Application code can tune 'bcMaxBytesPerPage' to align with OS page sizes (4096 bytes)
-- or network MTU / socket buffer dimensions (e.g. 65536 bytes).
data BatchConfig = BatchConfig
  { bcMaxBytesPerPage   :: !Int
    -- ^ Maximum accumulated UTF-8 bytes before page flush (e.g. 4096 or 65536).
    -- Matches that would exceed this ceiling trigger an immediate page cut.
  , bcMaxMatchesPerPage :: !Int
    -- ^ Maximum number of match items before page flush (default: 1024).
    -- Caps the maximum element count of the backing 'SmallArray'.
  , bcStepQuantum       :: !Int
    -- ^ Maximum VM steps per evaluation slice (default: 5000).
    -- Enforces bounded latency per regex evaluation step.
  } deriving stock (Eq, Show, Generic)

-- | Default batch configuration with 4 KiB byte threshold, 1024 matches per page,
-- and a 5,000-step evaluation quantum.
--
-- Suitable for standard in-memory document parsing and streaming log search.
defaultBatchConfig :: BatchConfig
defaultBatchConfig = BatchConfig
  { bcMaxBytesPerPage   = 4096
  , bcMaxMatchesPerPage = 1024
  , bcStepQuantum       = 5000
  }

-- | A frozen, immutable page of regular expression match results.
--
-- Packages a contiguous 'SmallArray' of matches alongside physical byte footprint
-- accounting and consumed VM step telemetry.
--
-- The array elements are guaranteed to be ordered strictly by ascending match offset.
data MatchBatch s = MatchBatch
  { batchItems         :: !(SmallArray (MatchResult s))
    -- ^ Contiguous array of match results contained in this batch page.
  , batchByteFootprint :: !Int
    -- ^ Total accumulated UTF-8 bytes materialized by matched slices in this page.
  , batchStepsConsumed :: !Int
    -- ^ Total VM execution steps consumed while producing this batch page.
  } deriving stock (Eq, Show, Generic)

-- | Construct an empty, zero-sized 'MatchBatch'.
--
-- Pre-allocates a 0-element 'SmallArray' with 0 byte footprint and 0 steps.
emptyMatchBatch :: MatchBatch s
emptyMatchBatch = runST $ do
  arr <- newSmallArray 0 undefined
  frz <- freezeSmallArray arr 0 0
  pure MatchBatch
    { batchItems = frz
    , batchByteFootprint = 0
    , batchStepsConsumed = 0
    }

-- | Calculate the exact physical UTF-8 byte footprint of a slice within the target sequence.
--
-- Traverses character by character within the matched span @[offset .. offset + len - 1]@,
-- summing the multi-byte UTF-8 width of each codepoint via 'utf8CharByteWidth'.
--
-- This function avoids allocating intermediate 'ByteString' objects by inspecting
-- character codepoints directly through the 'RegexTarget' sequence interface.
calcMatchByteFootprint :: RegexTarget s => s -> Int -> Int -> Int
calcMatchByteFootprint target offset len = go offset 0
  where
    maxOffset = offset + len
    go !idx !bytes
      | idx >= maxOffset = bytes
      | otherwise = case seqIndex target idx of
          Just c  -> go (idx + 1) (bytes + utf8CharByteWidth c)
          Nothing -> bytes

-- | Find all regular expression occurrences across a sequence, partitioning results
-- into discrete 'MatchBatch' pages bounded by memory footprint and capacity.
--
-- Evaluates the pattern incrementally, accumulating results into the active page buffer.
-- When the projected byte footprint exceeds 'bcMaxBytesPerPage' or the item count reaches
-- 'bcMaxMatchesPerPage', the page is frozen and emitted, resetting the page accumulator.
--
-- === Step Bounding and Fuel
-- Each match attempt executes under 'bcStepQuantum' fuel to prevent pathological patterns
-- from stalling the batch producer.
findAllMatchesBatched
  :: forall s. RegexTarget s
  => BatchConfig
  -> Regex
  -> s
  -> [MatchBatch s]
findAllMatchesBatched cfg regex target = go 0 [] 0 [] 0 0
  where
    targetLen = seqLength target

    -- | Tail-recursive outer search loop traversing slice indices.
    -- Bounded strictly by sequence length to ensure guaranteed termination.
    go !startIdx !currItems !currBytes !batches !totalSteps !sliceSteps
      | startIdx > targetLen =
          -- Exhausted sequence: finalize any trailing pending matches.
          let finalBatches = if null currItems
                                then batches
                                else finalizeBatch currItems currBytes sliceSteps : batches
          in reverse finalBatches
      | otherwise =
          -- Slice candidate input window.
          let remainingSlice = seqSlice startIdx (targetLen - startIdx) target
              outcome = runMatchWithMetrics regex remainingSlice (StepBudget cfg.bcStepQuantum)
              steps = outcome.outcomeMetrics.stepsConsumed
              newSliceSteps = sliceSteps + steps
              newTotalSteps = totalSteps + steps
          in case outcome.outcomeStatus of
               Matched res ->
                 -- Successfully matched: adjust relative slice offset to absolute sequence offset.
                 let adjRes = res
                       { matchOffset = res.matchOffset + startIdx
                       }
                     itemBytes = calcMatchByteFootprint target adjRes.matchOffset adjRes.matchLength
                     candBytes = currBytes + itemBytes
                     candCount = length currItems + 1
                     nextStart = if res.matchLength > 0
                                   then startIdx + res.matchOffset + res.matchLength
                                   else startIdx + 1
                 -- Check if adding this item exceeds byte footprint or item capacity thresholds.
                 in if candBytes > cfg.bcMaxBytesPerPage || candCount > cfg.bcMaxMatchesPerPage
                      then
                        -- Flush current batch and begin a fresh page containing the new item.
                        let flushed = finalizeBatch currItems currBytes newSliceSteps
                        in go nextStart [adjRes] itemBytes (flushed : batches) newTotalSteps 0
                      else
                        -- Accumulate into current page.
                        go nextStart (adjRes : currItems) candBytes batches newTotalSteps newSliceSteps
               _ ->
                 -- No match in remaining slice: finalize any pending batch and terminate.
                 let finalBatches = if null currItems
                                      then batches
                                      else finalizeBatch currItems currBytes newSliceSteps : batches
                 in reverse finalBatches

    -- | Freeze accumulated items into an immutable SmallArray page.
    -- Reverses the prepended accumulator into ascending start offset order.
    -- Guarantees O(N) allocation of the final SmallArray buffer.
    finalizeBatch items bytes steps = runST $ do
      let count = length items
          ordered = reverse items
      marr <- newSmallArray count undefined
      fill marr 0 ordered
      frz <- freezeSmallArray marr 0 count
      pure MatchBatch
        { batchItems = frz
        , batchByteFootprint = bytes
        , batchStepsConsumed = steps
        }

    -- | Populate array sequentially from list elements.
    -- Recursively writes items into the mutable SmallArray buffer.
    fill _ _ [] = pure ()
    fill marr !idx (x : xs) = do
      writeSmallArray marr idx x
      fill marr (idx + 1) xs

-- | Stream matches batch by batch to a monadic action, enabling pipelined downstream consumption.
--
-- Enables processing huge document corpora without retaining all batches simultaneously in heap.
streamBatches
  :: Monad m
  => [MatchBatch s]
  -> (MatchBatch s -> m ())
  -> m ()
streamBatches batches handler = mapM_ handler batches
