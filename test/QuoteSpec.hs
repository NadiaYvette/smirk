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
