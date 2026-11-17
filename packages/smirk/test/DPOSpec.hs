{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : DPOSpec
Description : Unit and property tests for Smirk.Core.DPO
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module DPOSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import Smirk.Core.AST (CharClass (..), CharRange (..))
import Smirk.Core.Bytecode
import Smirk.Core.DPO

spec :: Spec
spec = describe "Smirk.Core.DPO" $ do
  describe "Jump Threading (RuleJumpThreading)" $ do
    it "threads chained jumps directly to the terminal target" $ do
      -- Bytecode: 0: OpJump 1 (to 1), 1: OpJump 2 (to 3), 2: OpChar 'x', 3: OpMatch
      let prog = V.fromList [OpJump 1, OpJump 2, OpChar 'x', OpMatch]
          opt  = optimizeBytecodeDPO prog
      opt V.! 0 `shouldBe` OpJump 3

  describe "Character Class Fusion (RuleClassFusion)" $ do
    it "fuses alternation diamonds into unified character classes" $ do
      -- Pattern: OpSplit 1 3; OpChar 'a'; OpJump 2; OpChar 'b'; OpMatch
      let prog = V.fromList [OpSplit 1 3, OpChar 'a', OpJump 2, OpChar 'b', OpMatch]
          opt  = optimizeBytecodeDPO prog
      -- First instruction should now be an OpClass matching 'a' or 'b'
      case opt V.! 0 of
        OpClass (ClassSet [RangeSingle 'a', RangeSingle 'b'] False) -> pure ()
        other -> expectationFailure $ "Expected fused OpClass, got: " ++ show other

  describe "Idempotence and Safety" $ do
    it "preserves already optimal programs without mutation" $ do
      let prog = V.fromList [OpChar 'a', OpChar 'b', OpMatch]
      optimizeBytecodeDPO prog `shouldBe` prog
