{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DPOMinedSpec
Description : Unit and property tests for Smirk.Core.DPO.Mined
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Verifies automated conditional DPO bytecode rule mining:
  - Abduction of disjoint character sets on Alt patterns
  - Bounded repetition non-overlap detection
  - Non-nullable prefix detection
  - Semantic preservation: unoptimized vs optimized regex matching equivalence
-}
module DPOMinedSpec (spec) where

import Test.Hspec

import Smirk.Core.AST (AST (..), CharClass (..))
import Smirk.Core.Compiler (compileRegex)
import Smirk.Core.DPO.Mined
import Smirk.Core.Parser (parseRegex)
import Smirk.Engine.VM (matchRegex)

spec :: Spec
spec = describe "Smirk.Core.DPO.Mined" $ do
  describe "mineDPORules" $ do
    it "mines disjoint class fusion on disjoint literal alternations" $ do
      case parseRegex "a|b" of
        Left err  -> expectationFailure $ "Parse failed: " ++ show err
        Right ast -> do
          let rules = mineDPORules ast
          length rules `shouldSatisfy` (>= 1)
          let r = head rules
          minedRuleName r `shouldBe` "DisjointClassFusion"
          minedIsVerified r `shouldBe` True

    it "mines disjoint class fusion on digit vs non-digit classes" $ do
      let ast = Alt [Class ClassDigit, Class ClassNotDigit]
          rules = mineDPORules ast
      length rules `shouldSatisfy` (>= 1)
      let r = head rules
      minedRuleName r `shouldBe` "DisjointClassFusion"
      case minedSideCondition r of
        DisjointCharClasses ClassDigit ClassNotDigit -> True `shouldBe` True
        _ -> expectationFailure "Expected DisjointCharClasses condition"

    it "mines bounded repetition rules on bounded repeat patterns" $ do
      case parseRegex "a{2,5}" of
        Left err  -> expectationFailure $ "Parse failed: " ++ show err
        Right ast -> do
          let rules = mineDPORules ast
          let boundRules = [ r | r <- rules, minedRuleName r == "BoundedRepetitionPrune" ]
          length boundRules `shouldSatisfy` (>= 1)
          let r = head boundRules
          case minedSideCondition r of
            BoundedRepetitionNonOverlap 2 5 -> True `shouldBe` True
            _ -> expectationFailure "Expected BoundedRepetitionNonOverlap 2 5"

    it "mines non-nullable prefix hoisting rules on sequential patterns" $ do
      case parseRegex "abc.*" of
        Left err  -> expectationFailure $ "Parse failed: " ++ show err
        Right ast -> do
          let rules = mineDPORules ast
          let prefixRules = [ r | r <- rules, minedRuleName r == "NonNullablePrefixHoist" ]
          length prefixRules `shouldSatisfy` (>= 1)
          let r = head prefixRules
          minedSideCondition r `shouldBe` NonNullablePrefix

  describe "optimizeRegexWithMinedRules" $ do
    it "preserves regex match semantics on test corpus" $ do
      let patternStrs = ["a|b", "[0-9]|\\D", "hello.*world", "a{2,4}b"]
      mapM_ (\p -> do
        case (parseRegex p, compileRegex p) of
          (Right ast, Right reg) -> do
            let rules = mineDPORules ast
                optReg = optimizeRegexWithMinedRules rules reg
            -- Verify identical behavior on positive and negative inputs
            matchRegex optReg "a" `shouldBe` matchRegex reg "a"
            matchRegex optReg "b" `shouldBe` matchRegex reg "b"
            matchRegex optReg "hello there world" `shouldBe` matchRegex reg "hello there world"
            matchRegex optReg "xyz" `shouldBe` matchRegex reg "xyz"
          _ -> expectationFailure $ "Compilation failed for: " ++ show p
        ) patternStrs
