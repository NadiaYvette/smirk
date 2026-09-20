{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module EffectSpec (spec) where

import Data.Text (Text)
import qualified Effectful as Eff
import qualified Polysemy as Poly
import Test.Hspec

import Smirk
import qualified Smirk.Effect.Effectful as SmirkEff
import qualified Smirk.Effect.Polysemy as SmirkPoly

spec :: Spec
spec = describe "Smirk.Effect (Effectful & Polysemy)" $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms" :: Text)

  describe "Effectful RegexEngine Interpreter" $ do
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

  describe "Polysemy RegexEngine Interpreter" $ do
    it "runs pure regex matching in Sem" $ do
      case mReg of
        Right reg -> do
          let mRes = Poly.run $ SmirkPoly.runRegexEnginePure $ do
                SmirkPoly.matchRegex reg target
          case mRes of
            Just res -> do
              res.matchedSlice `shouldBe` "timeout=500"
            Nothing -> expectationFailure "Expected match in Sem"
        Left err -> expectationFailure (show err)

    it "runs fuel-bounded matching in Sem" $ do
      case mReg of
        Right reg -> do
          let status = Poly.run $ SmirkPoly.runRegexEnginePure $ do
                SmirkPoly.matchRegexFuel reg target (StepBudget 1000)
          case status of
            Matched res -> res.matchedSlice `shouldBe` "timeout=500"
            _           -> expectationFailure "Expected Matched status"
        Left err -> expectationFailure (show err)
