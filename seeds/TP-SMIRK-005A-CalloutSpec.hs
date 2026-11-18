{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : CalloutSpec
Description : Specification tests for host-effectful PCRE callouts and lexical disambiguation
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
-}
module CalloutSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Smirk (compileRegex)
import Smirk.Core.Types
  ( CalloutDecision (..)
  , CalloutPayload (..)
  , Fuel (Unlimited)
  , MatchOutcome (..)
  , MatchResult (..)
  , MatchStatus (..)
  , emptyCaptureSlots
  )
import Smirk.Effect.Callout
  ( dispatchCallout
  , lookupSymbolCallout
  , resumeFromCalloutDecision
  , withSymbolTableCallout
  )

spec :: Spec
spec = describe "Smirk.Effect.Callout (Host-Effectful PCRE Callouts)" $ do
  it "dispatches callout payload to custom handler" $ do
    let payload = CalloutPayload 1 (Just "test") 0 0 (emptyCaptureSlots 0) ("hello" :: Text)
    dec <- dispatchCallout (\p -> pure (if p.calloutTag == Just "test" then ProceedMatch else ForceBacktrack)) payload
    dec `shouldBe` ProceedMatch

  it "verifies symbol-table lookup callout predicate" $ do
    let payload = CalloutPayload 2 (Just "isConstructor") 0 0 (emptyCaptureSlots 0) ("Foo" :: Text)
    dec1 <- lookupSymbolCallout (\t -> pure (t == "isConstructor")) payload
    dec1 `shouldBe` ProceedMatch
    dec2 <- lookupSymbolCallout (\t -> pure (t == "isModule")) payload
    dec2 `shouldBe` ForceBacktrack

  it "forces backtracking across lexical categories via withSymbolTableCallout" $ do
    case compileRegex "(?C\"isConstructor\")Foo|(?C\"isModule\")Foo" of
      Left err -> expectationFailure (show err)
      Right reg -> do
        -- Only "isModule" is accepted by the environment
        outcome <- withSymbolTableCallout (\t -> pure (t == "isModule")) reg ("Foo" :: Text) Unlimited
        case outcome.outcomeStatus of
          Matched res -> res.matchedSlice `shouldBe` "Foo"
          other -> expectationFailure ("Expected Matched, got: " <> show other)

  it "rejects when neither branch passes symbol table check" $ do
    case compileRegex "(?C\"isConstructor\")Foo|(?C\"isModule\")Foo" of
      Left err -> expectationFailure (show err)
      Right reg -> do
        outcome <- withSymbolTableCallout (\_ -> pure False) reg ("Foo" :: Text) Unlimited
        case outcome.outcomeStatus of
          NoMatch -> pure ()
          other -> expectationFailure ("Expected NoMatch, got: " <> show other)
