{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ZshExpansionSyntaxSpec
Description : Test suite for Zsh-style parameter expansion typed AST and Megaparsec parser
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
-}
module ZshExpansionSyntaxSpec (spec) where

import Test.Hspec

import Smirk.Zsh.Expansion.Syntax
  ( StripOp (..)
  , Subscript (..)
  , SubstOp (..)
  , ZshFlag (..)
  , ZshParamExpansion (..)
  , parseZshParamExpansion
  )

spec :: Spec
spec = describe "Smirk.Zsh.Expansion.Syntax (Zsh Parameter Expansion Parser)" $ do
  describe "Basic Parameter and Flag Parsing" $ do
    it "parses simple variable ${foo}" $ do
      case parseZshParamExpansion "${foo}" of
        Right ast -> do
          ast.expQuoted `shouldBe` False
          ast.expFlags `shouldBe` []
          ast.expVarOrInner `shouldBe` Left "foo"
        Left err -> expectationFailure (show err)

    it "parses array preservation flag ${(@)foo}" $ do
      case parseZshParamExpansion "${(@)foo}" of
        Right ast -> ast.expFlags `shouldBe` [FlagAtArrayPreserve]
        Left err  -> expectationFailure (show err)

    it "parses split and join flags ${(s/:/)foo} and ${(j:,:)foo}" $ do
      case parseZshParamExpansion "${(s/:/)foo}" of
        Right ast -> ast.expFlags `shouldBe` [FlagSplit ":"]
        Left err  -> expectationFailure (show err)
      case parseZshParamExpansion "${(j:,:)foo}" of
        Right ast -> ast.expFlags `shouldBe` [FlagJoin ","]
        Left err  -> expectationFailure (show err)

  describe "Subscripts and Range Indexing" $ do
    it "parses 1-based single index ${foo[1]}" $ do
      case parseZshParamExpansion "${foo[1]}" of
        Right ast -> ast.expSubscript `shouldBe` Just (SubscriptSingle 1)
        Left err  -> expectationFailure (show err)

    it "parses range index ${foo[1,3]}" $ do
      case parseZshParamExpansion "${foo[1,3]}" of
        Right ast -> ast.expSubscript `shouldBe` Just (SubscriptRange 1 3)
        Left err  -> expectationFailure (show err)

  describe "Pattern Stripping and Substitution" $ do
    it "parses greedy suffix stripping ${(s/x/)foo%%1*}" $ do
      case parseZshParamExpansion "${(s/x/)foo%%1*}" of
        Right ast -> do
          ast.expFlags `shouldBe` [FlagSplit "x"]
          ast.expStrip `shouldBe` Just (StripSuffixGreedy "1*")
        Left err -> expectationFailure (show err)

    it "parses global substitution ${foo:gs/old/new/}" $ do
      case parseZshParamExpansion "${foo:gs/old/new/}" of
        Right ast -> ast.expSubst `shouldBe` Just (SubstOp True "old" "new")
        Left err  -> expectationFailure (show err)

  describe "Nested Expansions and Quoting" $ do
    it "parses nested expansion with array preservation \"${${(@)foo}[1]}\"" $ do
      case parseZshParamExpansion "\"${${(@)foo}[1]}\"" of
        Right ast -> do
          ast.expQuoted `shouldBe` True
          case ast.expVarOrInner of
            Right inner -> do
              inner.expFlags `shouldBe` [FlagAtArrayPreserve]
              inner.expVarOrInner `shouldBe` Left "foo"
            Left _ -> expectationFailure "Expected inner nested expansion"
        Left err -> expectationFailure (show err)
