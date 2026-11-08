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
