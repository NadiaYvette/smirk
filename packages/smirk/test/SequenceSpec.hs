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
