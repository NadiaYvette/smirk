{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module NetworkFilterSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Smirk

spec :: Spec
spec = describe "Smirk.Continuation.NetworkFilter (Eager Streaming Network Filter)" $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"

  it "eagerly yields matches as chunks arrive without waiting for stream completion" $ do
    case mReg of
      Right reg -> do
        let chunk1 = ("header: timeout=500ms and " :: Text)
            chunk2 = ("status: max=2000ms" :: Text)
            state0 = initialStreamFilterState @0

            (out1, state1) = feedStreamChunk @0 reg chunk1 state0
        map (\m -> m.matchedSlice) out1.fcoMatches `shouldBe` ["timeout=500"]

        let (out2, _) = feedStreamChunk @0 reg chunk2 state1
        map (\m -> m.matchedSlice) out2.fcoMatches `shouldBe` ["max=2000"]
      Left err -> expectationFailure (show err)

  it "matches results identically to single-shot execution over full input" $ do
    case mReg of
      Right reg -> do
        let chunks = ["foo=1 ", "bar=2 ", "baz=3"] :: [Text]
            fullText = T.concat chunks
            (streamMatches, _) = filterStreamChunks @0 reg chunks
            singleShotMatches = findAllMatches reg fullText
        map (\m -> m.matchedSlice) streamMatches `shouldBe` map (\m -> m.matchedSlice) singleShotMatches
      Left err -> expectationFailure (show err)

  it "reclaims dead prefix bytes via GradedCursor evacuation" $ do
    case mReg of
      Right reg -> do
        let chunks = ["prefix_noise_data_before_match_key=100_suffix_noise"] :: [Text]
            (matches, evac) = filterStreamChunks @0 reg chunks
        length matches `shouldBe` 1
        (unEvacuatedBytes evac > 0) `shouldBe` True
      Left err -> expectationFailure (show err)
