{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MarpaSpec
Description : Unit and property tests for Smirk.Core.Marpa
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module MarpaSpec (spec) where

import Test.Hspec
import Smirk.Core.AST
import Smirk.Core.Budget
import Smirk.Core.Marpa

spec :: Spec
spec = describe "Smirk.Core.Marpa" $ do
  describe "buildGrammarFromAST & runMarpaRecognizer" $ do
    it "recognizes simple terminal literal tokens" $ do
      let ast = Lit 'a'
          grammar = buildGrammarFromAST ast
      runMarpaRecognizer grammar "a" budgetUnlimited `shouldBe` Right True
      runMarpaRecognizer grammar "b" budgetUnlimited `shouldBe` Right False

    it "recognizes character classes under budget" $ do
      let ast = Class ClassDigit
          grammar = buildGrammarFromAST ast
      runMarpaRecognizer grammar "5" budgetUnlimited `shouldBe` Right True
      runMarpaRecognizer grammar "x" budgetUnlimited `shouldBe` Right False

    it "halts gracefully when step budget is exhausted" $ do
      let ast = Lit 'z'
          grammar = buildGrammarFromAST ast
          exhaustedBudget = defaultBudget { maxVmSteps = 0 }
      case runMarpaRecognizer grammar "z" exhaustedBudget of
        Left err -> err `shouldContain` "ResourceBudget exceeded"
        Right _  -> expectationFailure "Expected ResourceBudget error but succeeded"
