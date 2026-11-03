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
  ) where

import qualified Data.ByteString as BS
import Data.Char (chr)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
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
