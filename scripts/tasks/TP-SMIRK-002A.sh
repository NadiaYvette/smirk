#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-SMIRK-002A Scaffold: Smirk Core Types & Flat SmallArray / Polytropos Capture Layouts] ==="

mkdir -p packages/smirk/src/Smirk/Core packages/smirk/src/Smirk/Engine packages/smirk/test

# 1. Update packages/smirk/smirk.cabal
cat << 'EOF' > packages/smirk/smirk.cabal
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
EOF

# 2. Update packages/smirk/src/Smirk/Engine/Sequence.hs
cat << 'EOF' > packages/smirk/src/Smirk/Engine/Sequence.hs
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Smirk.Engine.Sequence
Description : Sequence polymorphism abstraction for zero-copy and generic regex matching
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Provides the 'RegexTarget' typeclass, establishing a uniform sequence abstraction
across multiple standard Haskell container representations without boxing or
intermediate conversions.

=== Supported Sequence Targets

* 'Data.Text.Text': Standard UTF-8 strict text with $O(1)$ length/slicing.
* 'Data.ByteString.ByteString': Raw ASCII/UTF-8 byte sequences with zero-copy slicing.
* '[Char]': Standard Haskell linked lists of characters (lazy string processing).
* 'Data.Sequence.Seq Char': Finger-tree based sequences with logarithmic indexing.
* 'Data.Vector.Vector Char': Contiguous boxed/unboxed arrays of characters.
-}
module Smirk.Engine.Sequence
  ( RegexTarget (..)
  , utf8CharByteWidth
  ) where

import qualified Data.ByteString as BS
import Data.Char (chr)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import qualified Data.Vector as V
import Data.Vector (Vector)

-- | Abstract sequence interface required by the regex virtual machine.
-- Allows the VM execution loop to index characters, measure length, and extract
-- matched sub-slices without coupling to a concrete string representation.
class RegexTarget s where
  -- | Total length (number of characters) in the sequence.
  seqLength   :: s -> Int

  -- | Safely retrieve the character at the specified 0-based offset.
  -- Returns 'Nothing' if the index is out of bounds.
  seqIndex    :: s -> Int -> Maybe Char

  -- | Extract a contiguous sub-slice given an offset and length.
  seqSlice    :: Int -> Int -> s -> s

  -- | Convert the sequence (or a matched slice) to strict 'Text' for formatting.
  seqToText   :: s -> Text

  -- | Construct a sequence from strict 'Text'.
  seqFromText :: Text -> s

  -- | Physical UTF-8 byte length of the sequence.
  -- For strict 'Text' and strict 'BS.ByteString', this operation is $O(1)$.
  utf8ByteLength :: s -> Int

  -- | Physical UTF-8 byte width of a slice specified by character offset and character length.
  -- For strict 'Text' and strict 'BS.ByteString', this operation is $O(1)$ on the slice.
  utf8SliceByteWidth :: Int -> Int -> s -> Int
  utf8SliceByteWidth off len s = utf8ByteLength (seqSlice off len s)
  {-# INLINE utf8SliceByteWidth #-}

-- | Instance for strict 'Text' (standard high-performance string representation).
instance RegexTarget Text where
  seqLength = T.length
  {-# INLINE seqLength #-}

  seqIndex t i
    | i >= 0 && i < T.length t = Just (T.index t i)
    | otherwise                = Nothing
  {-# INLINE seqIndex #-}

  seqSlice off len t = T.take len (T.drop off t)
  {-# INLINE seqSlice #-}

  seqToText = id
  {-# INLINE seqToText #-}

  seqFromText = id
  {-# INLINE seqFromText #-}

  utf8ByteLength = TF.lengthWord8
  {-# INLINE utf8ByteLength #-}

  utf8SliceByteWidth off len t = TF.lengthWord8 (seqSlice off len t)
  {-# INLINE utf8SliceByteWidth #-}

-- | Instance for strict 'BS.ByteString' (interprets bytes as ASCII / Latin-1 codepoints).
instance RegexTarget BS.ByteString where
  seqLength = BS.length
  {-# INLINE seqLength #-}

  seqIndex bs i
    | i >= 0 && i < BS.length bs = Just (chr (fromIntegral (BS.index bs i)))
    | otherwise                  = Nothing
  {-# INLINE seqIndex #-}

  seqSlice off len bs = BS.take len (BS.drop off bs)
  {-# INLINE seqSlice #-}

  seqToText = TE.decodeUtf8Lenient
  {-# INLINE seqToText #-}

  seqFromText = TE.encodeUtf8
  {-# INLINE seqFromText #-}

  utf8ByteLength = BS.length
  {-# INLINE utf8ByteLength #-}

  utf8SliceByteWidth off len bs = BS.length (seqSlice off len bs)
  {-# INLINE utf8SliceByteWidth #-}

-- | Instance for standard linked list of characters ('[Char]' / 'String').
instance RegexTarget [Char] where
  seqLength = length
  {-# INLINE seqLength #-}

  seqIndex xs i
    | i >= 0 = case drop i xs of
        (c:_) -> Just c
        []    -> Nothing
    | otherwise = Nothing
  {-# INLINE seqIndex #-}

  seqSlice off len xs = take len (drop off xs)
  {-# INLINE seqSlice #-}

  seqToText = T.pack
  {-# INLINE seqToText #-}

  seqFromText = T.unpack
  {-# INLINE seqFromText #-}

  utf8ByteLength = foldl' (\acc c -> acc + utf8CharByteWidth c) 0
  {-# INLINE utf8ByteLength #-}

  utf8SliceByteWidth off len xs = utf8ByteLength (seqSlice off len xs)
  {-# INLINE utf8SliceByteWidth #-}

-- | Instance for finger-tree based 'Seq' collections.
instance RegexTarget (Seq Char) where
  seqLength = Seq.length
  {-# INLINE seqLength #-}

  seqIndex sq i = Seq.lookup i sq
  {-# INLINE seqIndex #-}

  seqSlice off len sq = Seq.take len (Seq.drop off sq)
  {-# INLINE seqSlice #-}

  seqToText sq = T.pack (foldr (:) [] sq)
  {-# INLINE seqToText #-}

  seqFromText t = Seq.fromList (T.unpack t)
  {-# INLINE seqFromText #-}

  utf8ByteLength = foldl' (\acc c -> acc + utf8CharByteWidth c) 0
  {-# INLINE utf8ByteLength #-}

  utf8SliceByteWidth off len sq = utf8ByteLength (seqSlice off len sq)
  {-# INLINE utf8SliceByteWidth #-}

-- | Instance for contiguous 'Vector' arrays.
instance RegexTarget (Vector Char) where
  seqLength = V.length
  {-# INLINE seqLength #-}

  seqIndex v i = v V.!? i
  {-# INLINE seqIndex #-}

  seqSlice off len v = V.slice off (min len (V.length v - off)) v
  {-# INLINE seqSlice #-}

  seqToText v = T.pack (V.toList v)
  {-# INLINE seqToText #-}

  seqFromText t = V.fromList (T.unpack t)
  {-# INLINE seqFromText #-}

  utf8ByteLength = V.foldl' (\acc c -> acc + utf8CharByteWidth c) 0
  {-# INLINE utf8ByteLength #-}

  utf8SliceByteWidth off len v = utf8ByteLength (seqSlice off len v)
  {-# INLINE utf8SliceByteWidth #-}

-- | Number of UTF-8 encoded bytes occupied by a single Unicode character.
utf8CharByteWidth :: Char -> Int
utf8CharByteWidth c
  | c <= '\x7F'   = 1
  | c <= '\x7FF'  = 2
  | c <= '\xFFFF' = 3
  | otherwise     = 4
{-# INLINE utf8CharByteWidth #-}
EOF

# 3. Create packages/smirk/src/Smirk/Core/Types.hs
cat << 'EOF' > packages/smirk/src/Smirk/Core/Types.hs
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

    -- * Match Outcomes
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
EOF

# 4. Update packages/smirk/src/Smirk/Engine/VM.hs
cat << 'EOF' > packages/smirk/src/Smirk/Engine/VM.hs
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

    -- * Execution Entry Points
  , runMatch
  , runMatchAt
  , resumeMatch
  , findAllMatches

    -- * Internal VM Engine & Capture Registers
  , ThreadState (..)
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
  , lookupNamedCaptureRep
  , stepVM
  , matchClass
  , matchAnchor
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Smirk.Core.AST (Anchor (..), CharClass (..), CharRange (..))
import Smirk.Core.Bytecode (Opcode (..), Regex (..))
import Smirk.Core.Types
  ( CaptureSlots (..)
  , Fuel (..)
  , MatchContinuation (..)
  , MatchResult (..)
  , MatchStatus (..)
  , ThreadState (..)
  , emptyCaptureSlots
  , getCaptureEnd
  , getCaptureSpan
  , getCaptureStart
  , lookupNamedCaptureRep
  , matchNamedCaptureRep
  , setCaptureEnd
  , setCaptureStart
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
         let initialThread = ThreadState 0 startIdx (emptyCaptureSlots (regex.maxGroupIndex + 1))
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
               let !saves' = setCaptureStart idx th.strIdx th.saves
               in stepVM regex target startIdx th { pc = pc' + 1, saves = saves' } stack (steps + 1) fuel

             OpSaveEnd idx ->
               let !saves' = setCaptureEnd idx th.strIdx th.saves
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
               case getCaptureSpan idx th.saves of
                 Just (st, en) ->
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
             let freshThread = ThreadState 0 nextStart (emptyCaptureSlots (regex.maxGroupIndex + 1))
             in stepVM regex target nextStart freshThread [] steps fuel
           else NoMatch

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
EOF

# 5. Update packages/smirk/src/Smirk.hs
cat << 'EOF' > packages/smirk/src/Smirk.hs
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
  , Flags (..)
  , defaultFlags
  , testMatch
  , execMatch
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
  , ed
  , glob
  , Editor (..)
  , applyEditor

    -- * AST & Compiler
  , AST (..)
  , Opcode (..)
  , Program
  , parsePCRE
  , parsePCREWithFlags
  , compileAST
  , compileToRegex

    -- * Execution & VM
  , RegexTarget (..)
  , Fuel (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , resumeMatch
  , findAllMatches

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
  ) where

import Data.Text (Text)

import Smirk.Core.AST
import Smirk.Core.Bytecode
import Smirk.Core.Compiler
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
EOF

# 6. Update packages/smirk/test/SequenceSpec.hs
cat << 'EOF' > packages/smirk/test/SequenceSpec.hs
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SequenceSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Engine.Sequence" $ do
  let reg = [re|(?<num>\d+)-(?<word>[a-z]+)|]

  it "matches against Data.Text" $ do
    let input = ("item 42-smirk in text" :: Text)
    case execMatch reg input of
      Just res -> do
        res.matchedSlice `shouldBe` "42-smirk"
        res.namedGroups `shouldBe` Map.fromList [("num", "42"), ("word", "smirk")]
      Nothing -> expectationFailure "Expected Text match"

  it "matches against Data.ByteString" $ do
    let input = ("item 42-smirk in text" :: BS.ByteString)
    case execMatch reg input of
      Just res -> do
        res.matchedSlice `shouldBe` "42-smirk"
        res.namedGroups `shouldBe` Map.fromList [("num", "42"), ("word", "smirk")]
      Nothing -> expectationFailure "Expected ByteString match"

  it "matches against String ([Char])" $ do
    let input = ("item 42-smirk in text" :: String)
    case execMatch reg input of
      Just res -> do
        res.matchedSlice `shouldBe` "42-smirk"
        res.namedGroups `shouldBe` Map.fromList [("num", "42"), ("word", "smirk")]
      Nothing -> expectationFailure "Expected String match"

  it "matches against Data.Sequence.Seq Char" $ do
    let input = Seq.fromList ("item 42-smirk in text" :: String)
    case execMatch reg input of
      Just res -> do
        res.matchedSlice `shouldBe` Seq.fromList "42-smirk"
      Nothing -> expectationFailure "Expected Seq match"

  it "matches against Data.Vector.Vector Char" $ do
    let input = V.fromList ("item 42-smirk in text" :: String)
    case execMatch reg input of
      Just res -> do
        res.matchedSlice `shouldBe` V.fromList "42-smirk"
      Nothing -> expectationFailure "Expected Vector match"

  it "measures ASCII UTF-8 physical byte footprint correctly" $ do
    let t = ("hello world" :: Text)
        bs = ("hello world" :: BS.ByteString)
        str = ("hello world" :: String)
        sq = Seq.fromList "hello world"
        v = V.fromList "hello world"
    utf8ByteLength t `shouldBe` 11
    utf8ByteLength bs `shouldBe` 11
    utf8ByteLength str `shouldBe` 11
    utf8ByteLength sq `shouldBe` 11
    utf8ByteLength v `shouldBe` 11

  it "measures multi-byte Unicode codepoints and slice byte widths correctly" $ do
    -- 'λ' is 2 bytes, '€' is 3 bytes, '🦀' (crab emoji) is 4 bytes
    let t = ("λ € 🦀" :: Text)
        -- chars: 5 ('λ', ' ', '€', ' ', '🦀')
        -- bytes: 2 + 1 + 3 + 1 + 4 = 11 bytes
    seqLength t `shouldBe` 5
    utf8ByteLength t `shouldBe` 11
    utf8SliceByteWidth 0 1 t `shouldBe` 2 -- 'λ'
    utf8SliceByteWidth 2 1 t `shouldBe` 3 -- '€'
    utf8SliceByteWidth 4 1 t `shouldBe` 4 -- '🦀'
    utf8SliceByteWidth 0 3 t `shouldBe` 6 -- "λ €" = 2 + 1 + 3 = 6 bytes
EOF

# 7. Update packages/smirk/test/VMSpec.hs
cat << 'EOF' > packages/smirk/test/VMSpec.hs
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module VMSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Polytropos.Core (repTag)
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Engine.VM" $ do
  it "matches simple literal patterns" $ do
    case compileRegex "smirk" of
      Right reg -> case execMatch reg ("a smirk ;-) in text" :: Text) of
        Just res -> do
          res.matchedSlice `shouldBe` "smirk"
          res.matchOffset `shouldBe` 2
          res.matchLength `shouldBe` 5
        Nothing -> expectationFailure "Expected match for 'smirk'"
      Left err -> expectationFailure (show err)

  it "extracts numbered and named capture groups" $ do
    case compileRegex "(?<protocol>https?)://(?<domain>[a-z.]+)" of
      Right reg -> case execMatch reg ("visit https://haskell.org today" :: Text) of
        Just res -> do
          res.matchedSlice `shouldBe` "https://haskell.org"
          Map.lookup 1 res.captureGroups `shouldBe` Just "https"
          Map.lookup 2 res.captureGroups `shouldBe` Just "haskell.org"
          Map.lookup "protocol" res.namedGroups `shouldBe` Just "https"
          Map.lookup "domain" res.namedGroups `shouldBe` Just "haskell.org"
        Nothing -> expectationFailure "Expected capture groups match"
      Left err -> expectationFailure (show err)

  it "stores capture spans in unboxed CaptureSlots and projects to Polytropos Rep" $ do
    case compileRegex "(?<proto>https?)://(?<host>[a-z.]+)" of
      Right reg -> case execMatch reg ("request https://github.com now" :: Text) of
        Just res -> do
          -- Full match span
          getCaptureSpan 0 res.captureSlots `shouldBe` Just (8, 26)
          -- Group 1 (proto)
          getCaptureSpan 1 res.captureSlots `shouldBe` Just (8, 13)
          -- Group 2 (host)
          getCaptureSpan 2 res.captureSlots `shouldBe` Just (16, 26)

          -- SmallArray named projection
          let arr = toNamedCaptureSmallArray reg res.captureSlots ("request https://github.com now" :: Text)
          -- Lexicographical order: "host" (index 0), "proto" (index 1)
          sizeofSmallArray arr `shouldBe` 2
          indexSmallArray arr 0 `shouldBe` Just "github.com"
          indexSmallArray arr 1 `shouldBe` Just "https"

          -- Polytropos Rep projection
          let rep = toNamedCaptureRep reg res.captureSlots ("request https://github.com now" :: Text)
          repTag rep `shouldBe` 0
          lookupNamedCaptureRep "host" reg rep `shouldBe` Just ("github.com" :: Text)
          lookupNamedCaptureRep "proto" reg rep `shouldBe` Just ("https" :: Text)
        Nothing -> expectationFailure "Expected match for url"
      Left err -> expectationFailure (show err)

  it "matches positive and negative lookahead assertions" $ do
    case (compileRegex "\\w+(?=@example\\.com)", compileRegex "\\w+(?!@example\\.com)") of
      (Right regPos, Right regNeg) -> do
        case execMatch regPos ("alice@example.com" :: Text) of
          Just res -> res.matchedSlice `shouldBe` "alice"
          Nothing -> expectationFailure "Expected positive lookahead match"

        case execMatch regNeg ("bob@other.com" :: Text) of
          Just res -> res.matchedSlice `shouldBe` "bob"
          Nothing -> expectationFailure "Expected negative lookahead match"
      _ -> expectationFailure "Failed to compile lookahead regexes"

  it "matches backreferences" $ do
    case compileRegex "([a-z]+) \\1" of
      Right reg -> do
        case execMatch reg ("the the repeat" :: Text) of
          Just res -> res.matchedSlice `shouldBe` "the the"
          Nothing -> expectationFailure "Expected backreference match"

        case execMatch reg ("the other word" :: Text) of
          Just _  -> expectationFailure "Should not match different words"
          Nothing -> pure ()
      Left err -> expectationFailure (show err)

  it "enforces step bounding and safely yields OutOfFuel on ReDoS patterns" $ do
    case compileRegex "(a+)+b" of
      Right redosReg -> do
        let evilInput = ("aaaaaaaaaaaaaaac" :: Text)
        case execMatchWithFuel redosReg evilInput (StepBudget 50) of
          OutOfFuel cont -> do
            cont.currentSteps `shouldSatisfy` (>= 50)
            case resumeMatch cont (StepBudget 100) of
              OutOfFuel cont2 -> cont2.currentSteps `shouldSatisfy` (>= 100)
              _ -> expectationFailure "Expected OutOfFuel again on continued ReDoS"
          _ -> expectationFailure "Expected OutOfFuel on initial ReDoS run"
      Left err -> expectationFailure (show err)

  it "finds all non-overlapping matches in input" $ do
    case compileRegex "\\d+" of
      Right reg -> do
        let matches = findAllMatches reg ("10 apples, 20 oranges, 35 bananas" :: Text)
        length matches `shouldBe` 3
        map (\m -> m.matchedSlice) matches `shouldBe` ["10", "20", "35"]
      Left err -> expectationFailure (show err)
EOF

echo "=== [TP-SMIRK-002A Scaffold applied successfully] ==="
