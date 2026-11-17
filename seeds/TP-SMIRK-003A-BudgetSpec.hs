{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : BudgetSpec
Description : Test suite for multi-dimensional resource budget algebra
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
-}
module BudgetSpec (spec) where

import Data.IORef (readIORef)
import Test.Hspec

import Smirk.Core.Budget
import Smirk.Core.Types (Fuel (..), StepMetrics (..), emptyStepMetrics)

spec :: Spec
spec = describe "Smirk.Core.Budget (Multi-Dimensional Resource Algebra)" $ do

  describe "Meet-Semilattice Properties" $ do
    it "satisfies idempotence: meetBudget a a == a" $ do
      let b1 = ResourceBudget 1000 50 4096 10 100
      meetBudget b1 b1 `shouldBe` b1

    it "satisfies commutativity: meetBudget a b == meetBudget b a" $ do
      let b1 = ResourceBudget 1000 50 4096 10 100
          b2 = ResourceBudget 500 100 8192 5 200
      meetBudget b1 b2 `shouldBe` meetBudget b2 b1

    it "satisfies associativity: meetBudget a (meet b c) == meet (meet a b) c" $ do
      let b1 = ResourceBudget 1000 50 4096 10 100
          b2 = ResourceBudget 500 100 8192 5 200
          b3 = ResourceBudget 800 25 2048 20 50
      meetBudget b1 (meetBudget b2 b3) `shouldBe` meetBudget (meetBudget b1 b2) b3

    it "budgetEmpty acts as absorbing bottom" $ do
      let b1 = ResourceBudget 1000 50 4096 10 100
      meetBudget b1 budgetEmpty `shouldBe` budgetEmpty

  describe "Proportional Rate & ReDoS Discrimination" $ do
    it "permits normal long matches within proportional work rate" $ do
      let b = ResourceBudget 100000 5000 1048576 50 500
          -- 2000 steps for 100 units scanned -> 20 steps/unit < 50 steps/unit cap
          metrics = StepMetrics
            { stepsConsumed         = 2000
            , fuelRemaining         = StepBudget 98000
            , unitsScanned          = 100
            , utf8BytesMaterialized = 100
            , peakBacktrackDepth    = 10
            , activeThreadCount     = 1
            }
      isStepMetricsExceeded b metrics `shouldBe` False

    it "detects and halts catastrophic ReDoS rate explosions" $ do
      let b = ResourceBudget 100000 5000 1048576 50 500
          -- 15000 steps consumed while only scanning 10 characters -> explosive rate
          metrics = StepMetrics
            { stepsConsumed         = 15000
            , fuelRemaining         = StepBudget 85000
            , unitsScanned          = 10
            , utf8BytesMaterialized = 10
            , peakBacktrackDepth    = 50
            , activeThreadCount     = 1
            }
      isStepMetricsExceeded b metrics `shouldBe` True

  describe "Cumulative Resource Ceilings" $ do
    it "detects cumulative step count exhaustion" $ do
      let b = ResourceBudget 1000 100 4096 100 100
          metrics = (emptyStepMetrics Unlimited) { stepsConsumed = 1001 }
      isStepMetricsExceeded b metrics `shouldBe` True

    it "detects backtrack frame stack overflow" $ do
      let b = ResourceBudget 10000 50 4096 100 100
          metrics = (emptyStepMetrics Unlimited) { peakBacktrackDepth = 51 }
      isStepMetricsExceeded b metrics `shouldBe` True

    it "detects materialized UTF-8 memory footprint breach" $ do
      let b = ResourceBudget 10000 50 4096 100 100
          metrics = (emptyStepMetrics Unlimited) { utf8BytesMaterialized = 4097 }
      isStepMetricsExceeded b metrics `shouldBe` True

  describe "Hierarchical Quota Carving and Refund" $ do
    it "carves child budget and restores parent quota on exit" $ do
      let parentBudget = ResourceBudget 10000 100 4096 50 100
      hRoot <- newBudgetHierarchy parentBudget
      sysBefore <- readIORef hRoot.hierarchySystemTotal
      sysBefore `shouldBe` 10000

      let childReq = ResourceBudget 2000 50 2048 25 50
      withResourceBudget hRoot childReq $ \hChild -> do
        hChild.hierarchyLocalBudget.maxVmSteps `shouldBe` 2000
        hChild.hierarchyLocalBudget.maxBacktrackFrames `shouldBe` 50
        sysDuring <- readIORef hRoot.hierarchySystemTotal
        sysDuring `shouldBe` 8000

      -- Remainder restored after scope exit
      sysAfter <- readIORef hRoot.hierarchySystemTotal
      sysAfter `shouldBe` 10000
