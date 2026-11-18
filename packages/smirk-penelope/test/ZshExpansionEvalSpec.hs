{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : ZshExpansionEvalSpec
Description : Test suite for two-sorted scalar/array Penelope evaluator for Zsh expansions
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
-}
module ZshExpansionEvalSpec (spec) where

import Test.Hspec

import Smirk.Zsh.Expansion.Eval
  ( TwoSortedVal (..)
  , evalZshExpansion
  , smallArrayFromList
  , smallArrayToList
  )
import Smirk.Zsh.Expansion.Syntax (parseZshParamExpansion)

spec :: Spec
spec = describe "Smirk.Zsh.Expansion.Eval (Two-Sorted Zsh Evaluator)" $ do
  let env "foo" = Just (ValArray (smallArrayFromList ["bar", "baz"]))
      env "target" = Just (ValScalar "axb1end")
      env _     = Nothing

  describe "Canonical Zsh Nested Array and Quoting Semantics" $ do
    it "evaluates \"${(@)${foo}[1]}\" to scalar 'b' (coalesced inner array indexed as scalar)" $ do
      case parseZshParamExpansion "\"${(@)${foo}[1]}\"" of
        Right ast -> do
          res <- case evalZshExpansion env ast of
            Right val -> pure val
            Left err  -> expectationFailure (show err) >> pure (ValScalar "")
          res `shouldBe` ValScalar "b"
        Left err -> expectationFailure (show err)

    it "evaluates \"${${(@)foo}[1]}\" to scalar 'bar' (preserved inner array indexed by element)" $ do
      case parseZshParamExpansion "\"${${(@)foo}[1]}\"" of
        Right ast -> do
          res <- case evalZshExpansion env ast of
            Right val -> pure val
            Left err  -> expectationFailure (show err) >> pure (ValScalar "")
          res `shouldBe` ValScalar "bar"
        Left err -> expectationFailure (show err)

    it "evaluates ${(s/x/)target%%1*} by stripping suffix 1* before splitting on x" $ do
      case parseZshParamExpansion "${(s/x/)target%%1*}" of
        Right ast -> do
          res <- case evalZshExpansion env ast of
            Right val -> pure val
            Left err  -> expectationFailure (show err) >> pure (ValScalar "")
          case res of
            ValArray arr -> smallArrayToList arr `shouldBe` ["a", "b"]
            other        -> expectationFailure ("Expected ValArray, got: " <> show other)
        Left err -> expectationFailure (show err)
