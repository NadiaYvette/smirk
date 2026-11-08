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
