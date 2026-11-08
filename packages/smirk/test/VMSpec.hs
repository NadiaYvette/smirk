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

  describe "Lucene INTERVAL Execution & Numeric Range Scanning" $ do
    it "matches numbers within inclusive natural interval [min..max]" $ do
      case compileRegex "(?<0..100>)" of
        Right reg -> do
          case execMatch reg ("42" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "42"
              res.matchLength `shouldBe` 2
            Nothing -> expectationFailure "Expected match for 42"
          case execMatch reg ("0" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "0"
              res.matchLength `shouldBe` 1
            Nothing -> expectationFailure "Expected match for 0"
          case execMatch reg ("100" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "100"
              res.matchLength `shouldBe` 3
            Nothing -> expectationFailure "Expected match for 100"
        Left e -> expectationFailure (show e)

    it "rejects numbers outside the specified natural interval" $ do
      case compileRegex "(?<10..50>)" of
        Right reg -> do
          execMatch reg ("9" :: Text) `shouldBe` Nothing
          execMatch reg ("51" :: Text) `shouldBe` Nothing
          execMatch reg ("500" :: Text) `shouldBe` Nothing
        Left e -> expectationFailure (show e)

    it "matches composite date formats using chained intervals" $ do
      case compileRegex "(?<1900..2099>)-(?<1..12>)-(?<1..31>)" of
        Right reg -> do
          case execMatch reg ("Date: 2026-11-08 recorded" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "2026-11-08"
              res.matchOffset `shouldBe` 6
              res.matchLength `shouldBe` 10
            Nothing -> expectationFailure "Expected match for date"
        Left e -> expectationFailure (show e)

    it "matches standalone <min..max> syntax when intervalFlag is enabled" $ do
      case compileRegexWithFlags (enableIntervals defaultFlags) "<0..255>" of
        Right reg -> do
          case execMatch reg ("octet 192 in ip" :: Text) of
            Just res -> res.matchedSlice `shouldBe` "192"
            Nothing -> expectationFailure "Expected match for 192"
        Left e -> expectationFailure (show e)

    it "scanWord64 correctly extracts unsigned 64-bit values and lengths" $ do
      scanWord64 ("42abc" :: Text) 0 `shouldBe` (Just 42, 2)
      scanWord64 ("0" :: Text) 0 `shouldBe` (Just 0, 1)
      scanWord64 ("007" :: Text) 0 `shouldBe` (Just 7, 3)
      scanWord64 ("non-digit" :: Text) 0 `shouldBe` (Nothing, 0)

  describe "PCRE Callout VM Execution" $ do
    it "executes across callouts as zero-width inspection hooks" $ do
      case compileRegex "(?C)hello(?C1)" of
        Right reg -> do
          case execMatch reg ("say hello now" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "hello"
              res.matchOffset `shouldBe` 4
            Nothing -> expectationFailure "Expected match for hello with callouts"
        Left e -> expectationFailure (show e)

    it "preserves thread state and captures across interspersed tagged callouts" $ do
      case compileRegex "(?C\"start\")(?<tag>[a-z0-9]+)(?C2\"mid\"):(?<val>\\d+)(?C\"end\")" of
        Right reg -> do
          case execMatch reg ("item: val42:99" :: Text) of
            Just res -> do
              res.matchedSlice `shouldBe` "val42:99"
              Map.lookup "tag" res.namedGroups `shouldBe` Just "val42"
              Map.lookup "val" res.namedGroups `shouldBe` Just "99"
            Nothing -> expectationFailure "Expected match for tagged callouts pattern"
        Left e -> expectationFailure (show e)

  describe "Multi-Resource StepMetrics & MatchOutcome (TP-SMIRK-002E)" $ do
    it "tracks steps, units scanned, and utf8 bytes on direct match" $ do
      case compileRegex "smirk" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("a smirk ;-)" :: Text) Unlimited
          case outcome.outcomeStatus of
            Matched res -> do
              res.matchedSlice `shouldBe` "smirk"
              outcome.outcomeMetrics.stepsConsumed `shouldSatisfy` (> 0)
              outcome.outcomeMetrics.unitsScanned `shouldBe` 7
              outcome.outcomeMetrics.utf8BytesMaterialized `shouldBe` 7
              outcome.outcomeMetrics.activeThreadCount `shouldBe` 0
              outcome.outcomeMetrics.fuelRemaining `shouldBe` Unlimited
            _ -> expectationFailure "Expected Matched outcome"
        Left e -> expectationFailure (show e)

    it "tracks peak backtrack depth and non-zero active thread count on suspension" $ do
      case compileRegex "(a|b|c)d" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("cd" :: Text) Unlimited
          case outcome.outcomeStatus of
            Matched res -> do
              res.matchedSlice `shouldBe` "cd"
              outcome.outcomeMetrics.peakBacktrackDepth `shouldSatisfy` (>= 1)
            _ -> expectationFailure "Expected Matched outcome"
        Left e -> expectationFailure (show e)

    it "tracks multi-byte UTF-8 bytes materialized correctly" $ do
      case compileRegex "λ-calculus" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("λ-calculus" :: Text) Unlimited
          case outcome.outcomeStatus of
            Matched res -> do
              res.matchedSlice `shouldBe` "λ-calculus"
              -- "λ-calculus" is 10 characters, but 11 UTF-8 bytes (λ is 2 bytes: 0xCE 0xBB)
              outcome.outcomeMetrics.unitsScanned `shouldBe` 10
              outcome.outcomeMetrics.utf8BytesMaterialized `shouldBe` 11
            _ -> expectationFailure "Expected Matched outcome"
        Left e -> expectationFailure (show e)

    it "records remaining fuel and active thread count on OutOfFuel suspension" $ do
      case compileRegex "(a+)+b" of
        Right redosReg -> do
          let outcome = runMatchWithMetrics redosReg ("aaaaaaaaaaaaaaac" :: Text) (StepBudget 40)
          case outcome.outcomeStatus of
            OutOfFuel cont -> do
              outcome.outcomeMetrics.stepsConsumed `shouldSatisfy` (>= 40)
              outcome.outcomeMetrics.fuelRemaining `shouldBe` StepBudget 0
              outcome.outcomeMetrics.activeThreadCount `shouldSatisfy` (>= 1)
              cont.currentSteps `shouldSatisfy` (>= 40)
            _ -> expectationFailure "Expected OutOfFuel outcome"
        Left e -> expectationFailure (show e)

  describe "SuspendedCallout Interception & Resumption Decisions" $ do
    it "yields SuspendedCallout with payload and resumes with ProceedMatch" $ do
      case compileRegex "(?C42\"tag1\")hello" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("hello world" :: Text) Unlimited
          case outcome.outcomeStatus of
            SuspendedCallout payload resume -> do
              payload.calloutNumber `shouldBe` 42
              payload.calloutTag `shouldBe` Just "tag1"
              payload.calloutPosition `shouldBe` 0
              payload.calloutMatchStart `shouldBe` 0
              payload.calloutSubject `shouldBe` "hello world"
              outcome.outcomeMetrics.activeThreadCount `shouldSatisfy` (>= 1)
              -- Resume with ProceedMatch
              let resumed = resumeMatchWithMetrics (resume ProceedMatch) Unlimited
              case resumed.outcomeStatus of
                Matched res -> res.matchedSlice `shouldBe` "hello"
                _ -> expectationFailure "Expected match upon ProceedMatch"
            _ -> expectationFailure "Expected SuspendedCallout"
        Left e -> expectationFailure (show e)

    it "forces backtrack to alternate branch upon ForceBacktrack decision" $ do
      case compileRegex "(?C1\"first\")foo|bar" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("bar" :: Text) Unlimited
          case outcome.outcomeStatus of
            SuspendedCallout payload resume -> do
              payload.calloutNumber `shouldBe` 1
              payload.calloutTag `shouldBe` Just "first"
              -- Reject "foo" branch via ForceBacktrack: should backtrack and take "bar" branch!
              let resumed = resumeMatchWithMetrics (resume ForceBacktrack) Unlimited
              case resumed.outcomeStatus of
                Matched res -> res.matchedSlice `shouldBe` "bar"
                _ -> expectationFailure "Expected match on 'bar' after ForceBacktrack"
            _ -> expectationFailure "Expected SuspendedCallout on branch 1"
        Left e -> expectationFailure (show e)

    it "aborts evaluation immediately upon AbortEvaluation decision" $ do
      case compileRegex "(?C99)hello" of
        Right reg -> do
          let outcome = runMatchWithMetrics reg ("hello" :: Text) Unlimited
          case outcome.outcomeStatus of
            SuspendedCallout _ resume -> do
              let aborted = resumeMatchWithMetrics (resume AbortEvaluation) Unlimited
              aborted.outcomeStatus `shouldBe` NoMatch
            _ -> expectationFailure "Expected SuspendedCallout"
        Left e -> expectationFailure (show e)

  describe "Pre-Allocated VMFrame & VMFramePool" $ do
    it "round-trips ThreadState to VMFrame and back" $ do
      let th = ThreadState 5 12 (emptyCaptureSlots 3)
          fr = threadToFrame th
      fr.framePc `shouldBe` 5
      fr.frameStrIdx `shouldBe` 12
      let th' = frameToThread fr
      th'.pc `shouldBe` 5
      th'.strIdx `shouldBe` 12

    it "packs and reads frames from pre-allocated VMFramePool" $ do
      let th1 = ThreadState 1 10 (emptyCaptureSlots 2)
          th2 = ThreadState 2 20 (emptyCaptureSlots 2)
          pool = threadsToFramePool [th1, th2]
          fr0 = readFrame pool 0
          fr1 = readFrame pool 1
      fr0.framePc `shouldBe` 1
      fr0.frameStrIdx `shouldBe` 10
      fr1.framePc `shouldBe` 2
      fr1.frameStrIdx `shouldBe` 20
      let recovered = framePoolToThreads pool 2
      map (\t -> (t.pc, t.strIdx)) recovered `shouldBe` [(1, 10), (2, 20)]
