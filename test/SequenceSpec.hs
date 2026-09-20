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
