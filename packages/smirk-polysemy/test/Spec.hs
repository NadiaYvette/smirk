{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Polysemy as Poly
import Test.Hspec

import Smirk
import qualified Smirk.Effect.Polysemy as SmirkPoly

main :: IO ()
main = hspec $ do
  let mReg = compileRegex "(?<key>[a-z]+)=(?<val>\\d+)"
      target = ("config: timeout=500ms" :: Text)
  describe "Smirk.Effect.Polysemy (Polysemy RegexEngine Interpreter)" $ do
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
