{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Smirk.Continuation.NetworkFilter
Description : Eagerly-yielding incremental streaming filter for low-latency network I/O
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental

=== Eager Streaming Network Filtering
Note: streaming-network-filter
High-throughput networking services (such as HTTP reverse proxies, event brokers, and
packet inspection appliances) receive payloads in small, discrete network packets or TLS
chunks (typically 1.5 KiB to 16 KiB). Holding incoming packets until full batch pages
(e.g. 64 KiB) are filled introduces unacceptable tail latency on sparse match streams.

'Smirk.Continuation.NetworkFilter' provides an eagerly-yielding streaming regex filter:
1. __Immediate Emission:__ As each network chunk arrives via 'feedStreamChunk', any matches
   spanning the current buffer horizon are returned immediately in 'fcoMatches'.
2. __Prefix Evacuation:__ Prefix characters that fall strictly behind the active search
   horizon and lookbehind window @w@ are safely discarded via 'evacuatePrefix', preventing
   unbounded buffer growth across long-lived network streams.
3. __Lookbehind Window Preservation:__ The static type-level lookbehind bound @(w :: Nat)@
   in 'GradedCursor' guarantees that lookbehind assertions (@(?<=...)@ and @(?<!...)@)
   continue to inspect required historical context across chunk boundaries.

=== GradedCursor Evacuation Mechanics
Note: graded-cursor-evacuation
When matching across streaming buffers, the engine must distinguish between:
* __Dead Prefix:__ Bytes preceding @minActiveOffset - w@ that can never be referenced by
  any in-flight thread or future lookbehind assertion.
* __Live Lookbehind Horizon:__ The trailing @w@ bytes preceding the active scan index.
* __Active Buffer:__ Bytes currently under inspection or awaiting matching.

=== Lookbehind Horizon Preservation Invariant
Note: network-lookbehind-preservation
Let $w$ be the maximum lookbehind horizon specified at the type level.
For any active thread at character position $p$, the required historical lookbehind
window spans $[p - w, p]$.
By setting the minimum active offset $m = \min \{ p \mid \text{thread } p \text{ is active} \}$,
all character indices strictly less than $m - w$ can never influence future match transitions.
'evacuatePrefix' reclaims exactly $\max(0, m - w)$ characters, converting them to physical
UTF-8 byte counts and freeing the underlying buffer slice.

=== Streaming Latency vs Memory Footprint
While 'Smirk.Continuation.Batch' buffers matches to maximize bulk array throughput,
'Smirk.Continuation.NetworkFilter' is optimized for immediate reactive pipelines:
* Matches are dispatched immediately upon chunk arrival without page buffering.
* Memory consumption is strictly bounded by the chunk size plus lookbehind horizon $w$.
* Reclaimed bytes provide physical accounting for backpressure and GC telemetry.
-}
module Smirk.Continuation.NetworkFilter
  ( -- * Stream Filter State & Types
    StreamFilterState (..)
  , initialStreamFilterState
  , FilterChunkOutcome (..)

    -- * Stream Filter Functions
  , feedStreamChunk
  , filterStreamChunks
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)

import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Cursor
  ( EvacuatedBytes (..)
  , GradedCursor (..)
  , advanceMinActive
  , evacuatePrefix
  , mkGradedCursor
  )
import Smirk.Core.Types
  ( Fuel (..)
  , MatchResult (..)
  )
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM
  ( MatchOutcome (..)
  , MatchStatus (..)
  , runMatchWithMetricsAt
  )

-- | Internal streaming state tracking accumulated buffer and cursor horizon.
--
-- Parameterized by lookbehind window @(w :: Nat)@ and sequence type @s@.
data StreamFilterState (w :: Nat) s = StreamFilterState
  { sfsCursor         :: !(GradedCursor w s)
    -- ^ Lookbehind cursor maintaining active sequence buffer and horizon offsets.
  , sfsTotalBytesRead :: !Int
    -- ^ Cumulative character/unit count fed into this stream filter.
  , sfsTotalReclaimed :: !EvacuatedBytes
    -- ^ Cumulative UTF-8 bytes safely reclaimed via prefix evacuation.
  } deriving stock (Show, Generic)

-- | Construct an empty initial stream filter state with an empty cursor buffer.
initialStreamFilterState :: forall (w :: Nat) s. Monoid s => StreamFilterState w s
initialStreamFilterState = StreamFilterState
  { sfsCursor         = mkGradedCursor mempty
  , sfsTotalBytesRead = 0
  , sfsTotalReclaimed = EvacuatedBytes 0
  }

-- | Outcome of feeding a single input chunk into the streaming filter.
--
-- Pairs emitted matches with reclaimed byte telemetry and pending buffer metrics.
data FilterChunkOutcome s = FilterChunkOutcome
  { fcoMatches          :: ![MatchResult s]
    -- ^ Newly discovered matches completely contained within or terminating in this chunk.
  , fcoEvacuatedBytes   :: !EvacuatedBytes
    -- ^ UTF-8 bytes safely evacuated and reclaimed from the cursor prefix during this step.
  , fcoPendingBufferLen :: !Int
    -- ^ Character count remaining in the cursor buffer after prefix evacuation.
  } deriving stock (Eq, Show, Generic)

-- | Feed a new chunk of data into the incremental stream filter.
--
-- Appends the chunk to the cursor buffer, executes the regex engine across newly available
-- text, advances the active scan horizon, and evacuates dead prefix bytes.
feedStreamChunk
  :: forall (w :: Nat) s. (KnownNat w, RegexTarget s, Semigroup s)
  => Regex
  -> s
  -> StreamFilterState w s
  -> (FilterChunkOutcome s, StreamFilterState w s)
feedStreamChunk regex newChunk state =
  let combined = state.sfsCursor.cursorTarget <> newChunk
      combinedLen = seqLength combined
      cursorUpdated = state.sfsCursor { cursorTarget = combined }
      baseOff = state.sfsCursor.cursorBaseOffset
      -- Active search begins strictly at or after cursorMinActiveOffset.
      -- Any prefix before cursorMinActiveOffset is historical context retained for lookbehind.
      startSearchIdx = max 0 (state.sfsCursor.cursorMinActiveOffset - baseOff)
      (foundMatches, nextScanOffset) = scanMatches startSearchIdx [] combined combinedLen
      -- Advance cursorMinActiveOffset in absolute stream coordinates.
      newMinActive = baseOff + nextScanOffset
      cursorWithMin = advanceMinActive newMinActive cursorUpdated
      (evacuated, evacuatedCursor) = evacuatePrefix cursorWithMin
      outcome = FilterChunkOutcome
        { fcoMatches          = foundMatches
        , fcoEvacuatedBytes   = evacuated
        , fcoPendingBufferLen = seqLength evacuatedCursor.cursorTarget
        }
      newState = StreamFilterState
        { sfsCursor         = evacuatedCursor
        , sfsTotalBytesRead = state.sfsTotalBytesRead + seqLength newChunk
        , sfsTotalReclaimed = state.sfsTotalReclaimed + evacuated
        }
  in (outcome, newState)
  where
    -- | Scan matches incrementally from startIdx until reaching end of buffer.
    scanMatches !startIdx !acc target targetLen
      | startIdx > targetLen = (reverse acc, startIdx)
      | otherwise =
          case runMatchWithMetricsAt regex target startIdx Unlimited of
            MatchOutcome (Matched res) _ ->
              let adjRes = res
                    { matchOffset = state.sfsCursor.cursorBaseOffset + res.matchOffset
                    }
                  nextStart = max (startIdx + 1) (res.matchOffset + res.matchLength)
              in scanMatches nextStart (adjRes : acc) target targetLen
            _ -> (reverse acc, startIdx)

-- | Filter an entire stream of chunks incrementally, accumulating matches and telemetry.
--
-- Processes each chunk sequentially through 'feedStreamChunk', folding over the stream
-- and returning the complete list of matches alongside total evacuated bytes.
filterStreamChunks
  :: forall (w :: Nat) s. (KnownNat w, RegexTarget s, Monoid s)
  => Regex
  -> [s]
  -> ([MatchResult s], EvacuatedBytes)
filterStreamChunks regex chunks = go chunks initialStreamFilterState [] (EvacuatedBytes 0)
  where
    go [] _ accMatches totalEvac = (reverse accMatches, totalEvac)
    go (c : cs) state accMatches totalEvac =
      let (outcome, nextState) = feedStreamChunk @w regex c state
          newAcc = reverse outcome.fcoMatches ++ accMatches
          newEvac = totalEvac + outcome.fcoEvacuatedBytes
      in go cs nextState newAcc newEvac
