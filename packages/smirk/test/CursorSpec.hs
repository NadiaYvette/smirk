{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : CursorSpec
Description : Test suite for linear continuations and graded lookbehind cursors
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module CursorSpec (spec) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Core.Cursor (Linear & Graded Modal Input Cursor Discipline)" $ do
  describe "Linear Continuation Discipline (-XLinearTypes)" $ do
    it "resumes single-shot linear continuation to successful match" $ do
      case compileRegex "hello\\s+world" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("hello    world" :: Text) (StepBudget 3) of
            OutOfFuel cont -> do
              let outcome = resumeLinear cont Unlimited
              case outcomeToMaybeResult outcome of
                Just res -> do
                  res.matchedSlice `shouldBe` "hello    world"
                  res.matchOffset `shouldBe` 0
                Nothing -> expectationFailure "Expected resumeLinear to find match"
            _ -> expectationFailure "Expected initial runMatch to halt with OutOfFuel"

    it "resumes using LinearContinuation wrapper via resumeLinearCont" $ do
      case compileRegex "abc[0-9]+def" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("xyzabc12345defghi" :: Text) (StepBudget 4) of
            OutOfFuel cont -> do
              let linCont = toLinearContinuation cont
                  outcome = resumeLinearCont linCont Unlimited
              case outcome.outcomeStatus of
                Matched res -> res.matchedSlice `shouldBe` "abc12345def"
                _           -> expectationFailure "Expected Matched"
            _ -> expectationFailure "Expected OutOfFuel"

    it "executes via mkResumeLinear and runResumeLinear function wrapper" $ do
      case compileRegex "pattern" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("search for pattern here" :: Text) (StepBudget 2) of
            OutOfFuel cont -> do
              let runner = mkResumeLinear
                  outcome = runResumeLinear runner cont Unlimited
              case outcomeToMaybeResult outcome of
                Just res -> res.matchedSlice `shouldBe` "pattern"
                Nothing  -> expectationFailure "Expected match"
            _ -> expectationFailure "Expected OutOfFuel"

    it "handles NoMatch verdict when resumed with unlimited fuel" $ do
      case compileRegex "nonexistent" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("some long input string that does not have the word" :: Text) (StepBudget 5) of
            OutOfFuel cont -> do
              let outcome = resumeLinear cont Unlimited
              outcome.outcomeStatus `shouldBe` NoMatch
              outcomeToMaybeResult outcome `shouldBe` Nothing
            _ -> expectationFailure "Expected OutOfFuel"

  describe "Graded Lookbehind Cursor (GradedCursor w s)" $ do
    it "reflects static lookbehind horizon at type level" $ do
      let c0 = mkGradedCursor ("test" :: Text) :: GradedCursor 0 Text
          c3 = mkGradedCursor ("test" :: Text) :: GradedCursor 3 Text
          c8 = mkGradedCursor ("test" :: Text) :: GradedCursor 8 Text
      cursorHorizon c0 `shouldBe` 0
      cursorHorizon c3 `shouldBe` 3
      cursorHorizon c8 `shouldBe` 8

    it "evacuates prefix on horizon 0 immediately" $ do
      let c = mkGradedCursor ("hello world" :: Text) :: GradedCursor 0 Text
          cAdv = advanceMinActive 6 c
      cursorHorizon cAdv `shouldBe` 0
      safeEvacuationHorizon cAdv `shouldBe` 6
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 6
      c'.cursorTarget `shouldBe` "world"
      cursorRemainingLength c' `shouldBe` 5

    it "preserves lookbehind window w on GradedCursor w" $ do
      -- Target length 10: "abcdefghij"
      -- Horizon = 3. Active thread at offset 7.
      -- Safe evacuation = 7 - 3 = 4 chars ("abcd" can be evacuated).
      let c = mkGradedCursor ("abcdefghij" :: Text) :: GradedCursor 3 Text
          cAdv = advanceMinActive 7 c
      safeEvacuationHorizon cAdv `shouldBe` 4
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 4
      c'.cursorBaseOffset `shouldBe` 4
      c'.cursorTarget `shouldBe` "efghij"
      cursorRemainingLength c' `shouldBe` 6
      -- Relative offset: absolute 7 is at relative index (7 - 4) = 3 ('h')
      cursorToRelativeOffset c' 7 `shouldBe` Just 3
      cursorFromRelativeOffset c' 3 `shouldBe` 7
      -- Relative index 0..2 ("efg") is intact in c'.cursorTarget
      cursorSlice 0 3 c' `shouldBe` "efg"
      -- Absolute index 2 was evacuated
      cursorToRelativeOffset c' 2 `shouldBe` Nothing

    it "handles zero evacuation when active offset <= horizon" $ do
      let c = mkGradedCursor ("abcdef" :: Text) :: GradedCursor 5 Text
          cAdv = advanceMinActive 3 c
      safeEvacuationHorizon cAdv `shouldBe` 0
      let (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 0
      c'.cursorBaseOffset `shouldBe` 0
      c'.cursorTarget `shouldBe` "abcdef"

    it "reclaims multi-byte UTF-8 sequences safely on Text" $ do
      -- 'λ' is 2 bytes, '€' is 3 bytes, '🦀' is 4 bytes
      -- "λ € 🦀 rest": 3 chars ("λ €") takes 2 + 1 + 3 = 6 UTF-8 bytes
      let t = ("λ € 🦀 rest" :: Text)
          c = mkGradedCursor t :: GradedCursor 0 Text
          cAdv = advanceMinActive 3 c -- after "λ €" (3 chars, 6 UTF-8 bytes)
          (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 3
      c'.cursorTarget `shouldBe` " 🦀 rest"
      cursorRemainingLength c' `shouldBe` 7

    it "evacuates raw byte sequences on ByteString" $ do
      let bs = ("hello world" :: ByteString)
          c = mkGradedCursor bs :: GradedCursor 0 ByteString
          cAdv = advanceMinActive 6 c
          (evac, c') = evacuatePrefix cAdv
      evac `shouldBe` 6
      c'.cursorBaseOffset `shouldBe` 6
      c'.cursorTarget `shouldBe` "world"
      cursorRemainingLength c' `shouldBe` 5

    it "extracts minimum active offset from suspended MatchContinuation" $ do
      case compileRegex "deep.*pattern" of
        Left err -> expectationFailure (T.unpack err)
        Right r -> do
          case runMatch r ("deep long input string with pattern" :: Text) (StepBudget 10) of
            OutOfFuel cont -> do
              let c = cursorFromContinuation @2 cont
              cursorHorizon c `shouldBe` 2
              c.cursorMinActiveOffset `shouldBe` minimumActiveOffset cont
              let safe = safeEvacuationHorizon c
              safe `shouldBe` max 0 (minimumActiveOffset cont - 2)
            _ -> expectationFailure "Expected OutOfFuel"
