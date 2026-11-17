{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : StrataSpec
Description : Unit and property tests for Smirk.Core.Strata
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module StrataSpec (spec) where

import Test.Hspec
import Smirk.Core.AST
import Smirk.Core.Strata

spec :: Spec
spec = describe "Smirk.Core.Strata" $ do
  describe "classifyAST (Language Stratum Classification)" $ do
    it "classifies literal sequences as StratumRegular" $ do
      let ast = Seq [Lit 'a', Lit 'b', Lit 'c']
      classifyAST ast `shouldBe` StratumRegular

    it "classifies character classes and alternations as StratumRegular" $ do
      let ast = Alt [Class ClassDigit, Class ClassWord]
      classifyAST ast `shouldBe` StratumRegular

    it "classifies repetitions and non-capturing groups as StratumRegular" $ do
      let ast = Repeat 1 Nothing RepGreedy (Seq [Lit 'x', Lit 'y'])
      classifyAST ast `shouldBe` StratumRegular

    it "classifies numbered backreferences as StratumBackref" $ do
      let ast = Seq [Group Nothing (Just 1) (Lit 'a'), BackRef 1]
      classifyAST ast `shouldBe` StratumBackref

    it "classifies named backreferences as StratumBackref" $ do
      let ast = Seq [Group (Just "tag") Nothing (Lit 'z'), NamedBackRef "tag"]
      classifyAST ast `shouldBe` StratumBackref

  describe "compileGlushkov & matchGlushkov (Bit-Parallel Fast Path)" $ do
    it "matches exact literal characters" $ do
      let ast = Seq [Lit 'f', Lit 'o', Lit 'o']
      case compileGlushkov ast of
        Nothing  -> expectationFailure "Failed to compile Glushkov NFA"
        Just nfa -> do
          matchGlushkov nfa "foo" `shouldBe` True
          matchGlushkov nfa "bar" `shouldBe` False
          matchGlushkov nfa "fo" `shouldBe` False
          matchGlushkov nfa "fooo" `shouldBe` False

    it "matches character classes" $ do
      let ast = Seq [Class ClassDigit, Class ClassDigit]
      case compileGlushkov ast of
        Nothing  -> expectationFailure "Failed to compile Glushkov NFA"
        Just nfa -> do
          matchGlushkov nfa "42" `shouldBe` True
          matchGlushkov nfa "9a" `shouldBe` False

    it "matches alternations correctly" $ do
      let ast = Alt [Seq [Lit 'c', Lit 'a', Lit 't'], Seq [Lit 'd', Lit 'o', Lit 'g']]
      case compileGlushkov ast of
        Nothing  -> expectationFailure "Failed to compile Glushkov NFA"
        Just nfa -> do
          matchGlushkov nfa "cat" `shouldBe` True
          matchGlushkov nfa "dog" `shouldBe` True
          matchGlushkov nfa "cow" `shouldBe` False

    it "refuses to compile StratumBackref patterns into Glushkov NFA" $ do
      let ast = Seq [Group Nothing (Just 1) (Lit 'a'), BackRef 1]
      compileGlushkov ast `shouldBe` Nothing
