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
