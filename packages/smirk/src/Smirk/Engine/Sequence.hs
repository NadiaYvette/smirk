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
