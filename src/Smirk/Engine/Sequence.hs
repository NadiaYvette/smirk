{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | Abstract sequence abstraction for zero-copy and generic regex matching.
class RegexTarget s where
  seqLength   :: s -> Int
  seqIndex    :: s -> Int -> Maybe Char
  seqSlice    :: Int -> Int -> s -> s -- ^ offset, length
  seqToText   :: s -> Text
  seqFromText :: Text -> s

instance RegexTarget Text where
  seqLength = T.length
  seqIndex t i
    | i >= 0 && i < T.length t = Just (T.index t i)
    | otherwise                = Nothing
  seqSlice off len t = T.take len (T.drop off t)
  seqToText = id
  seqFromText = id

instance RegexTarget BS.ByteString where
  seqLength = BS.length
  seqIndex bs i
    | i >= 0 && i < BS.length bs = Just (chr (fromIntegral (BS.index bs i)))
    | otherwise                  = Nothing
  seqSlice off len bs = BS.take len (BS.drop off bs)
  seqToText = TE.decodeUtf8Lenient
  seqFromText = TE.encodeUtf8

instance RegexTarget [Char] where
  seqLength = length
  seqIndex xs i
    | i >= 0 = case drop i xs of
        (c:_) -> Just c
        []    -> Nothing
    | otherwise = Nothing
  seqSlice off len xs = take len (drop off xs)
  seqToText = T.pack
  seqFromText = T.unpack

instance RegexTarget (Seq Char) where
  seqLength = Seq.length
  seqIndex sq i = Seq.lookup i sq
  seqSlice off len sq = Seq.take len (Seq.drop off sq)
  seqToText sq = T.pack (foldr (:) [] sq)
  seqFromText t = Seq.fromList (T.unpack t)

instance RegexTarget (Vector Char) where
  seqLength = V.length
  seqIndex v i = v V.!? i
  seqSlice off len v = V.slice off (min len (V.length v - off)) v
  seqToText v = T.pack (V.toList v)
  seqFromText t = V.fromList (T.unpack t)
