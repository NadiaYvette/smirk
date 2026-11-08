#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "${REPO_ROOT}"

echo "=== [TP-SMIRK-002B Implementation Scaffold] ==="

echo "  Writing packages/smirk/smirk.cabal..."
mkdir -p "$(dirname "packages/smirk/smirk.cabal")"
cat << 'EOF_PACKAGES_SMIRK_SMIRK_CABAL' > "packages/smirk/smirk.cabal"
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
        Smirk.Core.Evacuable
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
EOF_PACKAGES_SMIRK_SMIRK_CABAL

echo "  Writing packages/smirk/src/Smirk/Core/Evacuable.hs..."
mkdir -p "$(dirname "packages/smirk/src/Smirk/Core/Evacuable.hs")"
cat << 'EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_EVACUABLE_HS' > "packages/smirk/src/Smirk/Core/Evacuable.hs"
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Smirk.Core.Evacuable
Description : Static AST nullability, lookbehind bounds, and non-empty sequence slicing
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Provides static analysis of regular expression abstract syntax trees (AST) to decide
nullability ($\varepsilon \in L(r)$) and lookbehind horizons at compile time.
Also defines non-empty match witnesses ('NonEmptySlice', 'NonEmptyTarget') and
execution modes ('MatchMode', 'MatchNotEmpty') implementing PCRE2_NOTEMPTY semantics.

=== Static AST Nullability
[tag:note-static-ast-nullability]
Determining whether a regular expression can match the empty string ($\varepsilon \in L(r)$)
is crucial for streaming compilers, tokenizer safety, and ReDoS prevention.
'isNullableAST' computes nullability statically without generating bytecode or allocating
virtual machine state.

=== Non-Empty Target Polymorphism
[tag:note-non-empty-target-polymorphism]
A 'NonEmptySlice' witnesses at the type level that a matched substring has a character
length $\ge 1$. The associated 'NonEmptyTarget' type family maps each sequence representation
(such as 'String') to its canonical non-empty carrier (such as 'Data.List.NonEmpty.NonEmpty Char').
-}
module Smirk.Core.Evacuable
  ( -- * Static AST Analysis
    isNullableAST
  , astMinLength
  , astMaxLength
  , maxLookbehindHorizon
  , collectLookbehinds

    -- * Non-Empty Slices & Targets
  , NonEmptySlice (..)
  , mkNonEmptySlice
  , unsafeNonEmptySlice
  , NonEmptyTarget
  , ToNonEmptyTarget (..)

    -- * Match Execution Modes & Results
  , MatchMode (..)
  , NonEmptyMatchResult (..)
  , toNonEmptyMatchResult
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Smirk.Core.AST
  ( AST (..)
  , LookDir (..)
  )
import Smirk.Core.Types
  ( CaptureSlots (..)
  , MatchMode (..)
  , MatchResult (..)
  )
import Smirk.Engine.Sequence (RegexTarget (..))

-- | Statically determine whether a regular expression AST is nullable,
-- i.e., whether the empty string is in its language: @\varepsilon \in L(r)@.
isNullableAST :: AST -> Bool
isNullableAST = \case
  Lit _                   -> False
  Class _                 -> False
  AnchorAssert _          -> True
  Seq xs                  -> all isNullableAST xs
  Alt xs                  -> any isNullableAST xs
  Repeat minCount _ _ sub -> minCount == 0 || isNullableAST sub
  Group _ _ sub           -> isNullableAST sub
  NonCapturing sub        -> isNullableAST sub
  Lookaround {}           -> True
  BackRef _               -> True
  NamedBackRef _          -> True
  Empty                   -> True

-- | Statically calculate the minimum character match length of an AST.
astMinLength :: AST -> Int
astMinLength = \case
  Lit _                   -> 1
  Class _                 -> 1
  AnchorAssert _          -> 0
  Seq xs                  -> sum (map astMinLength xs)
  Alt xs                  -> if null xs then 0 else minimum (map astMinLength xs)
  Repeat minCount _ _ sub -> minCount * astMinLength sub
  Group _ _ sub           -> astMinLength sub
  NonCapturing sub        -> astMinLength sub
  Lookaround {}           -> 0
  BackRef _               -> 0
  NamedBackRef _          -> 0
  Empty                   -> 0

-- | Statically calculate the maximum character match length of an AST,
-- or 'Nothing' if the match length is unbounded (e.g. unbounded repetition).
astMaxLength :: AST -> Maybe Int
astMaxLength = \case
  Lit _                   -> Just 1
  Class _                 -> Just 1
  AnchorAssert _          -> Just 0
  Seq xs                  -> sumMaybes (map astMaxLength xs)
  Alt xs                  -> if null xs then Just 0 else maxMaybes (map astMaxLength xs)
  Repeat _ maxCount _ sub -> case maxCount of
    Nothing -> case astMaxLength sub of
      Just 0 -> Just 0
      _      -> Nothing
    Just m  -> case astMaxLength sub of
      Nothing   -> if m == 0 then Just 0 else Nothing
      Just cLen -> Just (m * cLen)
  Group _ _ sub           -> astMaxLength sub
  NonCapturing sub        -> astMaxLength sub
  Lookaround {}           -> Just 0
  BackRef _               -> Nothing
  NamedBackRef _          -> Nothing
  Empty                   -> Just 0
  where
    sumMaybes [] = Just 0
    sumMaybes (Nothing : _) = Nothing
    sumMaybes (Just n : rest) = (n +) <$> sumMaybes rest

    maxMaybes [] = Just 0
    maxMaybes ms = foldr combine (Just 0) ms
      where
        combine Nothing _ = Nothing
        combine _ Nothing = Nothing
        combine (Just a) (Just b) = Just (max a b)

-- | Traverse an AST and collect all inner sub-expressions of lookbehind assertions.
collectLookbehinds :: AST -> [AST]
collectLookbehinds = \case
  Lookaround Lookbehind _ sub -> sub : collectLookbehinds sub
  Lookaround Lookahead _ sub  -> collectLookbehinds sub
  Seq xs                      -> concatMap collectLookbehinds xs
  Alt xs                      -> concatMap collectLookbehinds xs
  Repeat _ _ _ sub            -> collectLookbehinds sub
  Group _ _ sub               -> collectLookbehinds sub
  NonCapturing sub            -> collectLookbehinds sub
  _                           -> []

-- | Compute the static maximum lookbehind horizon required by an AST.
-- If the AST contains lookbehind assertions, returns the maximum horizon across all
-- lookbehinds. If the AST contains no lookbehind assertions, returns the maximum match
-- length of the expression itself (treating it as a standalone lookbehind pattern).
-- Returns 'Nothing' if any lookbehind pattern is unbounded.
maxLookbehindHorizon :: AST -> Maybe Int
maxLookbehindHorizon ast =
  let lbs = collectLookbehinds ast
  in if null lbs
       then astMaxLength ast
       else foldr (combine . astMaxLength) (Just 0) lbs
  where
    combine Nothing _ = Nothing
    combine _ Nothing = Nothing
    combine (Just a) (Just b) = Just (max a b)

-- | Witness that a matched slice contains at least one character (matchLength >= 1).
newtype NonEmptySlice s = NonEmptySlice { unNonEmptySlice :: s }
  deriving stock (Eq, Show, Generic)

-- | Safely construct a 'NonEmptySlice' if the slice contains at least 1 character.
mkNonEmptySlice :: RegexTarget s => s -> Maybe (NonEmptySlice s)
mkNonEmptySlice s
  | seqLength s >= 1 = Just (NonEmptySlice s)
  | otherwise        = Nothing

-- | Unsafely construct a 'NonEmptySlice' without checking length.
unsafeNonEmptySlice :: s -> NonEmptySlice s
unsafeNonEmptySlice = NonEmptySlice

-- | Type family mapping a target sequence type to its canonical non-empty carrier.
type family NonEmptyTarget s where
  NonEmptyTarget String        = NonEmpty Char
  NonEmptyTarget Text          = NonEmptySlice Text
  NonEmptyTarget ByteString    = NonEmptySlice ByteString
  NonEmptyTarget (Seq Char)    = NonEmptySlice (Seq Char)
  NonEmptyTarget (Vector Char) = NonEmptySlice (Vector Char)
  NonEmptyTarget s             = NonEmptySlice s

-- | Type class providing bidirectional conversion between a sequence's
-- 'NonEmptySlice' witness and its canonical 'NonEmptyTarget'.
class RegexTarget s => ToNonEmptyTarget s where
  toNonEmptyTarget :: NonEmptySlice s -> NonEmptyTarget s
  fromNonEmptyTarget :: NonEmptyTarget s -> s

instance ToNonEmptyTarget String where
  toNonEmptyTarget (NonEmptySlice (c : cs)) = c :| cs
  toNonEmptyTarget (NonEmptySlice [])       = error "Smirk.Core.Evacuable: empty NonEmptySlice String"
  fromNonEmptyTarget (c :| cs)             = c : cs

instance ToNonEmptyTarget Text where
  toNonEmptyTarget = id
  fromNonEmptyTarget = unNonEmptySlice

instance ToNonEmptyTarget ByteString where
  toNonEmptyTarget = id
  fromNonEmptyTarget = unNonEmptySlice

instance ToNonEmptyTarget (Seq Char) where
  toNonEmptyTarget = id
  fromNonEmptyTarget = unNonEmptySlice

instance ToNonEmptyTarget (Vector Char) where
  toNonEmptyTarget = id
  fromNonEmptyTarget = unNonEmptySlice

-- | Successful match result guaranteeing a non-empty matched slice (length >= 1).
data NonEmptyMatchResult s = NonEmptyMatchResult
  { matchedSlice  :: !(NonEmptySlice s) -- ^ Exact non-empty matched slice
  , matchOffset   :: !Int               -- ^ 0-based character offset where match begins
  , matchLength   :: !Int               -- ^ Total length (>= 1) of the matched slice
  , captureSlots  :: !CaptureSlots      -- ^ Flat unboxed capture registers
  , captureGroups :: !(Map Int s)       -- ^ Numbered capture groups
  , namedGroups   :: !(Map Text s)      -- ^ Named capture groups
  , stepsConsumed :: !Int               -- ^ Total VM steps executed
  } deriving stock (Eq, Show, Generic)

-- | Convert a standard 'MatchResult' to a 'NonEmptyMatchResult' if length >= 1.
toNonEmptyMatchResult :: RegexTarget s => MatchResult s -> Maybe (NonEmptyMatchResult s)
toNonEmptyMatchResult res = do
  neSlice <- mkNonEmptySlice res.matchedSlice
  pure NonEmptyMatchResult
    { matchedSlice  = neSlice
    , matchOffset   = res.matchOffset
    , matchLength   = res.matchLength
    , captureSlots  = res.captureSlots
    , captureGroups = res.captureGroups
    , namedGroups   = res.namedGroups
    , stepsConsumed = res.stepsConsumed
    }
EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_EVACUABLE_HS

echo "  Writing packages/smirk/src/Smirk/Core/Types.hs..."
mkdir -p "$(dirname "packages/smirk/src/Smirk/Core/Types.hs")"
cat << 'EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_TYPES_HS' > "packages/smirk/src/Smirk/Core/Types.hs"
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
EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_TYPES_HS

echo "  Writing packages/smirk/src/Smirk/Engine/VM.hs..."
mkdir -p "$(dirname "packages/smirk/src/Smirk/Engine/VM.hs")"
cat << 'EOF_PACKAGES_SMIRK_SRC_SMIRK_ENGINE_VM_HS' > "packages/smirk/src/Smirk/Engine/VM.hs"
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

    -- * Execution Modes
  , MatchMode (..)

    -- * Execution Entry Points
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
  , stepVMWithMode
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
  , MatchMode (..)
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

-- | Execute regex search across a sequence starting from index 0 using standard matching.
runMatch :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatch = runMatchWithMode MatchStandard

-- | Execute regex search starting from a specific character offset using standard matching.
runMatchAt :: RegexTarget s => Regex -> s -> Int -> Fuel -> MatchStatus s
runMatchAt = runMatchAtWithMode MatchStandard

-- | Execute regex search across a sequence under an explicit 'MatchMode'.
runMatchWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Fuel -> MatchStatus s
runMatchWithMode mode regex target = runMatchAtWithMode mode regex target 0

-- | Execute regex search starting from a specific character offset under an explicit 'MatchMode'.
runMatchAtWithMode :: RegexTarget s => MatchMode -> Regex -> s -> Int -> Fuel -> MatchStatus s
runMatchAtWithMode mode regex target startIdx fuel =
  let len = seqLength target
  in if startIdx > len
       then NoMatch
       else
         let initialThread = ThreadState 0 startIdx (emptyCaptureSlots (regex.maxGroupIndex + 1))
         in stepVMWithMode mode regex target startIdx initialThread [] 0 fuel

-- | Execute regex search enforcing 'MatchNotEmpty' (PCRE2_NOTEMPTY semantics: rejects empty matches).
runMatchNotEmpty :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
runMatchNotEmpty = runMatchWithMode MatchNotEmpty

-- | Execute non-empty regex search returning the first non-empty match, if any.
execMatchNotEmpty :: RegexTarget s => Regex -> s -> Maybe (MatchResult s)
execMatchNotEmpty reg target = case runMatchNotEmpty reg target Unlimited of
  Matched res -> Just res
  _           -> Nothing

-- | Resume a previously suspended match with additional fuel, preserving its 'MatchMode'.
resumeMatch :: RegexTarget s => MatchContinuation s -> Fuel -> MatchStatus s
resumeMatch cont =
  stepVMWithMode
    cont.savedMatchMode
    cont.savedRegex
    cont.savedTarget
    cont.savedStartIndex
    cont.currentThread
    cont.backtrackStack
    cont.currentSteps

-- | Find all non-overlapping matches across the target sequence in standard matching mode.
findAllMatches :: RegexTarget s => Regex -> s -> [MatchResult s]
findAllMatches = findAllMatchesWithMode MatchStandard

-- | Find all non-overlapping matches under an explicit 'MatchMode'.
findAllMatchesWithMode :: RegexTarget s => MatchMode -> Regex -> s -> [MatchResult s]
findAllMatchesWithMode mode regex target = go 0
  where
    go off
      | off > seqLength target = []
      | otherwise = case runMatchAtWithMode mode regex target off Unlimited of
          Matched res ->
            let nextOff = max (off + 1) (res.matchOffset + res.matchLength)
            in res : go nextOff
          _ -> []

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
-- Evaluates one instruction and advances the thread or backtracks to an alternate branch.
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
  case fuel of
    StepBudget budget | steps >= budget ->
      OutOfFuel MatchContinuation
        { savedMatchMode  = mode
        , savedRegex      = regex
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
           then backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel
           else case prog V.! pc' of
             OpChar expected ->
               case seqIndex target th.strIdx of
                 Just c | c == expected ->
                   stepVMWithMode mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack (steps + 1) fuel
                 _ -> backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpClass cls ->
               case seqIndex target th.strIdx of
                 Just c | matchClass cls c ->
                   stepVMWithMode mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + 1 } stack (steps + 1) fuel
                 _ -> backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpAnchor anchor ->
               if matchAnchor anchor th.strIdx target
                 then stepVMWithMode mode regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                 else backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpSplit d1 d2 ->
               let branch2 = th { pc = pc' + d2 }
                   branch1 = th { pc = pc' + d1 }
               in stepVMWithMode mode regex target startIdx branch1 (branch2 : stack) (steps + 1) fuel

             OpJump d ->
               stepVMWithMode mode regex target startIdx th { pc = pc' + d } stack (steps + 1) fuel

             OpSaveStart idx ->
               let !saves' = setCaptureStart idx th.strIdx th.saves
               in stepVMWithMode mode regex target startIdx th { pc = pc' + 1, saves = saves' } stack (steps + 1) fuel

             OpSaveEnd idx ->
               let !saves' = setCaptureEnd idx th.strIdx th.saves
               in stepVMWithMode mode regex target startIdx th { pc = pc' + 1, saves = saves' } stack (steps + 1) fuel

             OpAssertLookahead subProg isPos ->
               let subRegex = regex { program = subProg }
                   subMatched = case runMatchAtWithMode MatchStandard subRegex target th.strIdx (StepBudget 1000) of
                     Matched _ -> True
                     _         -> False
               in if subMatched == isPos
                    then stepVMWithMode mode regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                    else backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpAssertLookbehind subProg isPos ->
               let subRegex = regex { program = subProg }
                   subMatched = any (\prevOff ->
                     case runMatchAtWithMode MatchStandard subRegex target prevOff (StepBudget 1000) of
                       Matched res -> res.matchOffset + res.matchLength == th.strIdx
                       _           -> False
                     ) [max 0 (th.strIdx - 20) .. th.strIdx]
               in if subMatched == isPos
                    then stepVMWithMode mode regex target startIdx th { pc = pc' + 1 } stack (steps + 1) fuel
                    else backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpMatchBackRef idx ->
               case getCaptureSpan idx th.saves of
                 Just (st, en) ->
                   let refLen = en - st
                       refSlice = seqSlice st refLen target
                       targetSlice = seqSlice th.strIdx refLen target
                   in if seqToText refSlice == seqToText targetSlice
                        then stepVMWithMode mode regex target startIdx th { pc = pc' + 1, strIdx = th.strIdx + refLen } stack (steps + 1) fuel
                        else backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel
                 _ -> backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel

             OpMatch ->
               let res = buildResult regex target th (steps + 1)
               in case mode of
                 MatchNotEmpty | res.matchLength == 0 ->
                   -- Reject empty match under PCRE2_NOTEMPTY semantics: backtrack!
                   backtrackOrNextStart mode regex target startIdx stack (steps + 1) fuel
                 _ ->
                   Matched res

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
  case stack of
    (nextTh : rest) ->
      stepVMWithMode mode regex target startIdx nextTh rest steps fuel
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
             in stepVMWithMode mode regex target nextStart freshThread [] steps fuel
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
EOF_PACKAGES_SMIRK_SRC_SMIRK_ENGINE_VM_HS

echo "  Writing packages/smirk/src/Smirk/Core/Quote.hs..."
mkdir -p "$(dirname "packages/smirk/src/Smirk/Core/Quote.hs")"
cat << 'EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_QUOTE_HS' > "packages/smirk/src/Smirk/Core/Quote.hs"
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      : Smirk.Core.Quote
Description : Compile-time QuasiQuoters for PCRE expressions and search-and-replace editors
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Provides Template Haskell QuasiQuoters that validate regular expressions and
stream-editor replacement templates at compile time, eliminating runtime parse
failures and overhead.

=== Available QuasiQuoters

* '[re|...|]': Compile-time validated PCRE regular expressions.
* '[ed|search///replace|]': Compile-time validated sed-style stream editor.
-}
module Smirk.Core.Quote
  ( -- * QuasiQuoters
    re
  , reNE
  , ed

    -- * Non-Nullable Compilation
  , compileNonEmptyRegex

    -- * Editor Definition & Application
  , Editor (..)
  , applyEditor
  , replaceMatch
  ) where

import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Smirk.Core.Bytecode (Regex)
import Smirk.Core.Compiler (compileToRegex)
import Smirk.Core.Evacuable (isNullableAST)
import Smirk.Core.Parser (parsePCRE)
import Smirk.Engine.Sequence (RegexTarget (..))
import Smirk.Engine.VM (MatchResult (..), findAllMatches)

-- | QuasiQuoter for compile-time verified PCRE regular expressions.
-- Rejects invalid PCRE patterns at compile time with descriptive syntax errors.
--
-- === Example
--
-- @
-- let r = [re|(?<protocol>https?)://(?<domain>[a-z.]+)|]
-- @
re :: QuasiQuoter
re = QuasiQuoter
  { quoteExp  = quoteRegexExp
  , quotePat  = error "Smirk.re: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.re: Type quasiquote not supported"
  , quoteDec  = error "Smirk.re: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for regex patterns.
quoteRegexExp :: String -> Q Exp
quoteRegexExp patternStr =
  case parsePCRE (T.pack patternStr) of
    Left err -> fail ("Smirk regex compile error: " <> T.unpack err)
    Right _  ->
      [| case parsePCRE (T.pack $(stringE patternStr)) of
           Right ast -> compileToRegex (T.pack $(stringE patternStr)) ast
           Left e    -> error ("Smirk regex internal error: " <> T.unpack e)
       |]

-- | QuasiQuoter for compile-time verified non-nullable PCRE regular expressions.
-- Statically rejects both invalid PCRE syntax and nullable patterns (patterns that
-- can match the empty string, i.e., @\varepsilon \in L(r)@) at compile time.
--
-- === Example
--
-- @
-- let r = [reNE|(?<protocol>https?)://(?<domain>[a-z.]+)|]
-- @
reNE :: QuasiQuoter
reNE = QuasiQuoter
  { quoteExp  = quoteRegexExpNE
  , quotePat  = error "Smirk.reNE: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.reNE: Type quasiquote not supported"
  , quoteDec  = error "Smirk.reNE: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for non-nullable regex patterns.
quoteRegexExpNE :: String -> Q Exp
quoteRegexExpNE patternStr =
  case parsePCRE (T.pack patternStr) of
    Left err -> fail ("Smirk regex compile error: " <> T.unpack err)
    Right ast
      | isNullableAST ast ->
          fail ("Smirk regex compile error: pattern '" <> patternStr <> "' is nullable (matches empty string), rejected by [reNE|...|]")
      | otherwise ->
          [| case parsePCRE (T.pack $(stringE patternStr)) of
               Right parsedAst -> compileToRegex (T.pack $(stringE patternStr)) parsedAst
               Left e          -> error ("Smirk regex internal error: " <> T.unpack e)
           |]

-- | Compile a PCRE regular expression requiring that the pattern is non-nullable.
-- Returns 'Left' if parsing fails or if the pattern matches the empty string.
compileNonEmptyRegex :: Text -> Either Text Regex
compileNonEmptyRegex input = do
  ast <- parsePCRE input
  if isNullableAST ast
    then Left ("Pattern is nullable (matches empty string): " <> input)
    else pure (compileToRegex input ast)

-- | Structured find-and-replace editor record.
data Editor s = Editor
  { editRegex       :: !Regex   -- ^ Compiled search regular expression
  , editReplacement :: !Text    -- ^ Replacement template string
  , editGlobal      :: !Bool    -- ^ If True, replace all non-overlapping occurrences; if False, first only
  } deriving (Eq, Show, Generic)

-- | QuasiQuoter for compile-time verified search-and-replace editors using @///@ delimiters.
--
-- Format: @[ed|search_pattern///replacement_string|]@ or @[ed|search///replace///1|]@ (for single replacement).
--
-- === Example
--
-- @
-- let edScript = [ed|foo([0-9]+)///bar${1}|]
-- applyEditor edScript "foo42" == "bar42"
-- @
ed :: QuasiQuoter
ed = QuasiQuoter
  { quoteExp  = quoteEditorExp
  , quotePat  = error "Smirk.ed: Pattern matching quasiquote not supported"
  , quoteType = error "Smirk.ed: Type quasiquote not supported"
  , quoteDec  = error "Smirk.ed: Declaration quasiquote not supported"
  }

-- | Compile-time expression quoter for search-and-replace editor templates.
quoteEditorExp :: String -> Q Exp
quoteEditorExp rawStr =
  case T.splitOn "///" (T.pack rawStr) of
    [searchPat, replaceStr] -> validateAndBuild searchPat replaceStr True
    [searchPat, replaceStr, opts] ->
      let isGlobal = not ("1" `T.isInfixOf` opts)
      in validateAndBuild searchPat replaceStr isGlobal
    _ ->
      fail "Smirk.ed: Expected format [ed|search_pattern///replacement_template|]"
  where
    validateAndBuild searchPat replaceStr isGlobal =
      case parsePCRE searchPat of
        Left err -> fail ("Smirk editor search regex error: " <> T.unpack err)
        Right _  ->
          [| Editor
               { editRegex = case parsePCRE (T.pack $(stringE (T.unpack searchPat))) of
                   Right ast -> compileToRegex (T.pack $(stringE (T.unpack searchPat))) ast
                   Left e    -> error ("Smirk editor search regex error: " <> T.unpack e)
               , editReplacement = T.pack $(stringE (T.unpack replaceStr))
               , editGlobal = $([| isGlobal |])
               }
           |]

-- | Apply an 't:Editor' transformation to a target sequence.
-- Finds matching slices and substitutes them according to the replacement template.
applyEditor :: RegexTarget s => Editor s -> s -> s
applyEditor editor target =
  let matches = findAllMatches editor.editRegex target
      selectedMatches = if editor.editGlobal then matches else take 1 matches
  in if null selectedMatches
       then target
       else
         let pieces = reconstructSlices target 0 selectedMatches editor.editReplacement
         in seqFromText (T.concat pieces)

-- | Reconstruct a sequence by interleaving untouched slices with substituted replacements.
reconstructSlices :: RegexTarget s => s -> Int -> [MatchResult s] -> Text -> [Text]
reconstructSlices target curIdx [] _ =
  let remLen = seqLength target - curIdx
  in [seqToText (seqSlice curIdx remLen target)]
reconstructSlices target curIdx (m : ms) replTemplate =
  let prefixLen = m.matchOffset - curIdx
      prefix = seqToText (seqSlice curIdx prefixLen target)
      replaced = replaceMatch m replTemplate
      nextIdx = m.matchOffset + m.matchLength
  in prefix : replaced : reconstructSlices target nextIdx ms replTemplate

-- | Expand capture group variables (${1}, ${name}, \1, $0, $&) in a replacement template.
replaceMatch :: RegexTarget s => MatchResult s -> Text -> Text
replaceMatch res = go
  where
    go txt
      | T.null txt = ""
      | "${" `T.isPrefixOf` txt =
          let rest = T.drop 2 txt
              (varName, after) = T.breakOn "}" rest
              varVal = lookupVar (T.unpack varName)
          in if "}" `T.isPrefixOf` after
               then varVal <> go (T.drop 1 after)
               else "${" <> go rest
      | "\\" `T.isPrefixOf` txt && T.length txt >= 2 =
          let digitChar = T.index txt 1
          in if isDigit digitChar
               then lookupVar [digitChar] <> go (T.drop 2 txt)
               else "\\" <> go (T.drop 1 txt)
      | "$&" `T.isPrefixOf` txt || "$0" `T.isPrefixOf` txt =
          seqToText res.matchedSlice <> go (T.drop 2 txt)
      | otherwise =
          let (chunk, rest) = T.break (\c -> c == '$' || c == '\\') txt
          in if T.null chunk
               then T.take 1 txt <> go (T.drop 1 txt)
               else chunk <> go rest

    lookupVar str = case reads str :: [(Int, String)] of
      [(idx, "")] -> maybe "" seqToText (Map.lookup idx res.captureGroups)
      _           -> maybe "" seqToText (Map.lookup (T.pack str) res.namedGroups)
EOF_PACKAGES_SMIRK_SRC_SMIRK_CORE_QUOTE_HS

echo "  Writing packages/smirk/src/Smirk.hs..."
mkdir -p "$(dirname "packages/smirk/src/Smirk.hs")"
cat << 'EOF_PACKAGES_SMIRK_SRC_SMIRK_HS' > "packages/smirk/src/Smirk.hs"
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
  , compileNonEmptyRegex
  , Flags (..)
  , defaultFlags
  , testMatch
  , execMatch
  , execMatchNotEmpty
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
  , reNE
  , ed
  , glob
  , Editor (..)
  , applyEditor

    -- * AST Analysis & Evacuability
  , AST (..)
  , isNullableAST
  , astMinLength
  , astMaxLength
  , maxLookbehindHorizon
  , collectLookbehinds

    -- * Non-Empty Slices & Targets
  , NonEmptySlice (..)
  , mkNonEmptySlice
  , unsafeNonEmptySlice
  , NonEmptyTarget
  , ToNonEmptyTarget (..)
  , NonEmptyMatchResult (..)
  , toNonEmptyMatchResult

    -- * Compiler & Bytecode
  , Opcode (..)
  , Program
  , parsePCRE
  , parsePCREWithFlags
  , compileAST
  , compileToRegex

    -- * Execution & VM Modes
  , RegexTarget (..)
  , Fuel (..)
  , MatchMode (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , runMatchWithMode
  , runMatchAtWithMode
  , runMatchNotEmpty
  , resumeMatch
  , findAllMatches
  , findAllMatchesWithMode
  , findAllMatchesNotEmpty

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
import Smirk.Core.Evacuable
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
EOF_PACKAGES_SMIRK_SRC_SMIRK_HS

echo "  Writing packages/smirk/test/ParserSpec.hs..."
mkdir -p "$(dirname "packages/smirk/test/ParserSpec.hs")"
cat << 'EOF_PACKAGES_SMIRK_TEST_PARSERSPEC_HS' > "packages/smirk/test/ParserSpec.hs"
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

import Smirk.Core.AST
import Smirk.Core.Evacuable
import Smirk.Core.Parser

spec :: Spec
spec = describe "Smirk.Core.Parser" $ do
  it "parses simple literal sequence" $ do
    let res = parsePCRE "smirk"
    case res of
      Right (Seq [Lit 's', Lit 'm', Lit 'i', Lit 'r', Lit 'k']) -> pure ()
      other -> expectationFailure $ "Expected Seq of chars but got: " <> show other

  it "parses character classes and digits" $ do
    let res = parsePCRE "[a-z0-9]+\\s*\\d+"
    case res of
      Right (Seq [ Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z', RangeBetween '0' '9'] False))
                 , Repeat 0 Nothing RepGreedy (Class ClassSpace)
                 , Repeat 1 Nothing RepGreedy (Class ClassDigit)
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  it "parses named capture groups and lookahead assertions" $ do
    let res = parsePCRE "(?<user>[a-zA-Z]+)(?=@domain\\.com)"
    case res of
      Right (Seq [ Group (Just "user") Nothing (Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z', RangeBetween 'A' 'Z'] False)))
                 , Lookaround Lookahead True (Seq [Lit '@', Lit 'd', Lit 'o', Lit 'm', Lit 'a', Lit 'i', Lit 'n', Lit '.', Lit 'c', Lit 'o', Lit 'm'])
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  it "parses backreferences" $ do
    let res = parsePCRE "([a-z]+)-\\1"
    case res of
      Right (Seq [ Group Nothing Nothing (Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z'] False)))
                 , Lit '-'
                 , BackRef 1
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  describe "Smirk.Core.Evacuable (Static Nullability & Lookbehind Horizon)" $ do
    describe "isNullableAST" $ do
      it "identifies non-nullable literal and character class expressions" $ do
        case parsePCRE "smirk" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)
        case parsePCRE "[a-z0-9]+" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)

      it "identifies nullable repetition expressions (*, ?, {0,n})" $ do
        case parsePCRE "a*" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a?" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "(a*)+" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a{0,5}" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a{1,5}" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)

      it "identifies nullable alternations and assertions" $ do
        case parsePCRE "a|b*" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a|b" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)
        case parsePCRE "^$" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)

    describe "maxLookbehindHorizon" $ do
      it "computes static lookbehind bounds for bounded lookbehind assertions" $ do
        case parsePCRE "(?<=abc)def" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Just 3
          Left e    -> expectationFailure (show e)
        case parsePCRE "(?<=a{2,5})b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Just 5
          Left e    -> expectationFailure (show e)

      it "returns Nothing for unbounded lookbehind assertions" $ do
        case parsePCRE "(?<=a+)b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Nothing
          Left e    -> expectationFailure (show e)
        case parsePCRE "(?<=a*)b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Nothing
          Left e    -> expectationFailure (show e)
EOF_PACKAGES_SMIRK_TEST_PARSERSPEC_HS

echo "  Writing packages/smirk/test/QuoteSpec.hs..."
mkdir -p "$(dirname "packages/smirk/test/QuoteSpec.hs")"
cat << 'EOF_PACKAGES_SMIRK_TEST_QUOTESPEC_HS' > "packages/smirk/test/QuoteSpec.hs"
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module QuoteSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Core.Quote ([re|...|] & [ed|...|])" $ do
  describe "[re|...|] Compile-Time QuasiQuoter" $ do
    it "matches compile-time validated PCRE regular expression" $ do
      let regex = [re|(?<greeting>hello|hi)\s+(?<target>[a-zA-Z]+)|]
      case execMatch regex ("hello world, smirk says hi" :: Text) of
        Just res -> do
          res.matchedSlice `shouldBe` "hello world"
          res.namedGroups `shouldBe` Map.fromList [("greeting", "hello"), ("target", "world")]
        Nothing -> expectationFailure "Expected QuasiQuoted regex to match"

  describe "[ed|...|] Compile-Time Search-and-Replace Editor" $ do
    it "replaces patterns with capture substitutions" $ do
      let editor = [ed|foo([0-9]+)///bar${1}|]
          input = ("foo10 and foo20" :: Text)
          result = applyEditor editor input
      result `shouldBe` "bar10 and bar20"

    it "replaces with full match $& variable" $ do
      let editor = [ed|\b[a-z]{4}\b///[$&]|]
          input = ("test code with text" :: Text)
          result = applyEditor editor input
      result `shouldBe` "[test] [code] [with] [text]"

  describe "[reNE|...|] Compile-Time Non-Nullable QuasiQuoter" $ do
    it "matches compile-time validated non-empty regular expressions" $ do
      let regex = [reNE|[a-zA-Z]+|]
      case execMatch regex ("hello world" :: Text) of
        Just res -> res.matchedSlice `shouldBe` "hello"
        Nothing  -> expectationFailure "Expected [reNE|...|] regex to match"

    it "validates that non-nullable regexes succeed and nullable fail via compileNonEmptyRegex" $ do
      case compileNonEmptyRegex "a+" of
        Right _  -> pure ()
        Left err -> expectationFailure ("Expected non-nullable 'a+' to compile: " <> show err)
      case compileNonEmptyRegex "a*" of
        Left err -> err `shouldBe` "Pattern is nullable (matches empty string): a*"
        Right _  -> expectationFailure "Expected nullable pattern 'a*' to be rejected"
      case compileNonEmptyRegex "a|b*" of
        Left err -> err `shouldBe` "Pattern is nullable (matches empty string): a|b*"
        Right _  -> expectationFailure "Expected nullable alternation to be rejected"
EOF_PACKAGES_SMIRK_TEST_QUOTESPEC_HS

echo "  Writing packages/smirk/test/VMSpec.hs..."
mkdir -p "$(dirname "packages/smirk/test/VMSpec.hs")"
cat << 'EOF_PACKAGES_SMIRK_TEST_VMSPEC_HS' > "packages/smirk/test/VMSpec.hs"
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module VMSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
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

  describe "MatchNotEmpty (PCRE2_NOTEMPTY Semantics) & NonEmptySlice" $ do
    it "rejects empty match and finds non-empty match under MatchNotEmpty" $ do
      case compileRegex "a*" of
        Right reg -> do
          -- Under standard mode on "b", "a*" matches "" at index 0
          case runMatch reg ("b" :: Text) Unlimited of
            Matched res -> res.matchLength `shouldBe` 0
            _           -> expectationFailure "Expected empty match in standard mode"
          -- Under MatchNotEmpty mode on "b", "a*" rejects "" and yields NoMatch
          case runMatchNotEmpty reg ("b" :: Text) Unlimited of
            NoMatch -> pure ()
            _       -> expectationFailure "Expected NoMatch under MatchNotEmpty for 'b'"
          -- Under MatchNotEmpty mode on "ba", "a*" skips empty prefix and matches "a" at index 1
          case runMatchNotEmpty reg ("ba" :: Text) Unlimited of
            Matched res -> do
              res.matchOffset `shouldBe` 1
              res.matchLength `shouldBe` 1
              res.matchedSlice `shouldBe` "a"
            _ -> expectationFailure "Expected MatchNotEmpty to match 'a' at offset 1"
        Left e -> expectationFailure (show e)

    it "finds all non-empty matches with findAllMatchesNotEmpty" $ do
      case compileRegex "a*" of
        Right reg -> do
          let matches = findAllMatchesNotEmpty reg ("aba" :: Text)
          map (\m -> m.matchedSlice) matches `shouldBe` ["a", "a"]
        Left e -> expectationFailure (show e)

    it "constructs NonEmptySlice and converts to NonEmptyTarget" $ do
      let neSlice = mkNonEmptySlice ("hello" :: String)
      case neSlice of
        Just s -> do
          let neList = toNonEmptyTarget s
          neList `shouldBe` ('h' :| "ello")
          fromNonEmptyTarget @String neList `shouldBe` "hello"
        Nothing -> expectationFailure "Expected Just NonEmptySlice"
      mkNonEmptySlice ("" :: String) `shouldBe` Nothing

    it "execMatchNotEmpty returns NonEmptyMatchResult" $ do
      case compileRegex "[0-9]+" of
        Right reg -> do
          case execMatchNotEmpty reg ("abc123xyz" :: Text) of
            Just res -> do
              res.matchOffset `shouldBe` 3
              res.matchLength `shouldBe` 3
              res.matchedSlice `shouldBe` "123"
              case toNonEmptyMatchResult res of
                Just neRes -> unNonEmptySlice neRes.matchedSlice `shouldBe` "123"
                Nothing    -> expectationFailure "Expected toNonEmptyMatchResult"
            Nothing -> expectationFailure "Expected execMatchNotEmpty to match"
        Left e -> expectationFailure (show e)
EOF_PACKAGES_SMIRK_TEST_VMSPEC_HS

echo "=== [TP-SMIRK-002B Scaffold Applied Successfully] ==="
