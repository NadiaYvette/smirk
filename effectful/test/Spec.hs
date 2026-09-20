{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Effectful as Eff
import Test.Hspec

import Smirk
import qualified Smirk.Effect.Effectful as SmirkEff

main :: IO ()
main = hspec $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms" :: Text)
  describe "Smirk.Effect.Effectful (Effectful RegexEngine Interpreter)" $ do
    it "runs pure regex matching in Eff" $ do
      case mReg of
        Right reg -> do
          mRes <- Eff.runEff $ SmirkEff.runRegexEnginePure $ do
            SmirkEff.matchRegex reg target
          case mRes of
            Just res -> do
              res.matchedSlice `shouldBe` "timeout=500"
            Nothing -> expectationFailure "Expected match in Eff"
        Left err -> expectationFailure (show err)

    it "runs fuel-bounded matching in Eff" $ do
      case mReg of
        Right reg -> do
          status <- Eff.runEff $ SmirkEff.runRegexEnginePure $ do
            SmirkEff.matchRegexFuel reg target (StepBudget 1000)
          case status of
            Matched res -> res.matchedSlice `shouldBe` "timeout=500"
            _           -> expectationFailure "Expected Matched status"
        Left err -> expectationFailure (show err)
