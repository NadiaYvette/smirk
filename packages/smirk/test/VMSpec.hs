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
