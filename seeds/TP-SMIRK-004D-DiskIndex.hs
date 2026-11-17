{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Smirk.Continuation.DiskIndex
Description : Compact posting metric, block-aligned batching, and external sort run generator
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental

=== High-Throughput Inverted Posting Indexing
Note: disk-index-posting-descriptors
When indexing massive textual repositories (such as codebase search engines, log analyzers,
or document archives), extracting and retaining raw matched string slices produces huge
heap allocation overhead and high GC pressure.

'Smirk.Continuation.DiskIndex' projects regex matches directly into fixed-width, 20-byte
binary posting descriptors ('MatchIndexEntry') that record:
* @mieDocId@: 32-bit unsigned document identifier (4 bytes).
* @mieUtf8ByteOffset@: 64-bit unsigned physical byte offset in document (8 bytes).
* @mieUtf8ByteLength@: 32-bit unsigned match byte length (4 bytes).
* @miePatternId@: 32-bit unsigned matched pattern identifier (4 bytes).

Total unboxed footprint: $4 + 8 + 4 + 4 = 20$ bytes per posting.

=== Disk Block Alignment
Note: disk-block-alignment
Modern storage media (NVMe SSDs and Linux page caches) operate on 4096-byte (4 KiB)
hardware blocks. 'IndexRun' batches posting descriptors into block-aligned runs:
* With 20 bytes per descriptor, a 4 KiB block holds up to 204 descriptors (4080 bytes)
  plus 16 bytes of block header metadata.
* Pre-sorting runs by @(mieDocId, mieUtf8ByteOffset, miePatternId)@ enables direct
  external merge-sorting and sequential block writes without random I/O thrashing.

=== Memory Footprint Invariants
Unlike naive list accumulation, 'generateSortedIndexRun' enforces:
1. __Zero String Retention:__ Matched substring slices are never copied into the heap;
   only unboxed byte offset metrics are retained.
2. __Bounded In-Flight Run Capacity:__ When the accumulated run reaches 'dicMaxEntries',
   it is immediately finalized and emitted.
3. __Deterministic Ordering:__ Descriptors within each run are sorted stably via 'sortIndexEntries'.
-}
module Smirk.Continuation.DiskIndex
  ( -- * Posting Descriptor
    MatchIndexEntry (..)
  , sizeofMatchIndexEntry

    -- * Index Run & Page
  , IndexRun (..)
  , DiskIndexConfig (..)
  , defaultDiskIndexConfig

    -- * Run Generation
  , generateSortedIndexRun
  , sortIndexEntries
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word32, Word64)
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

-- | Compact unboxed posting descriptor (20 bytes total).
--
-- Represents a single inverted index occurrence without holding matched text.
data MatchIndexEntry = MatchIndexEntry
  { mieDocId          :: !Word32
    -- ^ 4-byte unsigned document identifier.
  , mieUtf8ByteOffset :: !Word64
    -- ^ 8-byte unsigned physical UTF-8 byte offset in document.
  , mieUtf8ByteLength :: !Word32
    -- ^ 4-byte unsigned UTF-8 byte length of matched slice.
  , miePatternId      :: !Word32
    -- ^ 4-byte unsigned matched pattern identifier.
  } deriving stock (Eq, Ord, Show, Generic)

-- | Nominal physical size of a single 'MatchIndexEntry' in bytes (4 + 8 + 4 + 4 = 20).
sizeofMatchIndexEntry :: Int
sizeofMatchIndexEntry = 20

-- | Configuration parameters controlling block-aligned index run generation.
data DiskIndexConfig = DiskIndexConfig
  { dicBlockSizeBytes :: !Int
    -- ^ Page/block size in bytes (e.g. 4096 or 65536).
  , dicMaxEntries     :: !Int
    -- ^ Maximum entries per run before flush (default: 4096 / 20 = 204).
  } deriving stock (Eq, Show, Generic)

-- | Default disk index configuration with 4096-byte blocks and 204 entries per run.
defaultDiskIndexConfig :: DiskIndexConfig
defaultDiskIndexConfig = DiskIndexConfig
  { dicBlockSizeBytes = 4096
  , dicMaxEntries     = 4096 `div` sizeofMatchIndexEntry
  }

-- | A sorted run of posting descriptors aligned to disk-block boundaries.
--
-- Ready for direct serialization to external storage or multi-way merge sort.
data IndexRun = IndexRun
  { irEntries      :: ![MatchIndexEntry]
    -- ^ Strictly ordered posting entries in this run.
  , irTotalEntries :: !Int
    -- ^ Total count of posting entries in this run.
  , irByteSize     :: !Int
    -- ^ Total serialized byte footprint of posting entries (irTotalEntries * 20).
  } deriving stock (Eq, Show, Generic)

-- | Sort a list of 'MatchIndexEntry' descriptors stably by document ID, byte offset, and pattern ID.
sortIndexEntries :: [MatchIndexEntry] -> [MatchIndexEntry]
sortIndexEntries = sortBy (comparing (\e -> (e.mieDocId, e.mieUtf8ByteOffset, e.miePatternId)))

-- | Compute physical UTF-8 byte offset and byte length for a character match span.
--
-- Uses 'utf8CharByteWidth' across character offsets to accurately map character positions
-- to physical byte offsets in UTF-8 encoded storage files.
calcUtf8Span :: RegexTarget s => s -> Int -> Int -> (Word64, Word32)
calcUtf8Span target offset len =
  let byteOffset = fromIntegral (calcBytes 0 offset)
      byteLen = fromIntegral (calcBytes offset len)
  in (byteOffset, byteLen)
  where
    calcBytes start count = go start 0
      where
        limit = start + count
        go !idx !acc
          | idx >= limit = acc
          | otherwise = case seqIndex target idx of
              Just c  -> go (idx + 1) (acc + utf8CharByteWidth c)
              Nothing -> acc

-- | Generate sorted index runs across a target document with the given pattern and document ID.
--
-- Traverses the target sequence, finds matching occurrences, projects them into
-- 20-byte 'MatchIndexEntry' records, and flushes block-aligned 'IndexRun' batches.
generateSortedIndexRun
  :: forall s. RegexTarget s
  => DiskIndexConfig
  -> Word32  -- ^ Document ID
  -> Word32  -- ^ Pattern ID
  -> Regex
  -> s
  -> [IndexRun]
generateSortedIndexRun cfg docId patId regex target = go 0 [] []
  where
    targetLen = seqLength target

    go !startIdx !currEntries !runs
      | startIdx > targetLen =
          let finalRuns = if null currEntries
                            then runs
                            else finalizeRun currEntries : runs
          in reverse finalRuns
      | otherwise =
          let slice = seqSlice startIdx (targetLen - startIdx) target
              matchRes = runMatchWithMetrics regex slice Unlimited
          in case matchRes.outcomeStatus of
               Matched res ->
                 let absCharOffset = res.matchOffset + startIdx
                     (byteOffset, byteLen) = calcUtf8Span target absCharOffset res.matchLength
                     entry = MatchIndexEntry
                       { mieDocId          = docId
                       , mieUtf8ByteOffset = byteOffset
                       , mieUtf8ByteLength = byteLen
                       , miePatternId      = patId
                       }
                     nextStart = if res.matchLength > 0
                                   then absCharOffset + res.matchLength
                                   else absCharOffset + 1
                     candEntries = entry : currEntries
                 in if length candEntries >= cfg.dicMaxEntries
                      then
                        let flushed = finalizeRun candEntries
                        in go nextStart [] (flushed : runs)
                      else
                        go nextStart candEntries runs
               _ ->
                 let finalRuns = if null currEntries
                                   then runs
                                   else finalizeRun currEntries : runs
                 in reverse finalRuns

    -- | Finalize accumulated entries into a sorted, block-aligned IndexRun.
    finalizeRun entries =
      let sorted = sortIndexEntries entries
          count = length sorted
      in IndexRun
        { irEntries      = sorted
        , irTotalEntries = count
        , irByteSize     = count * sizeofMatchIndexEntry
        }
