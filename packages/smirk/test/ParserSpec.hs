{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

import Smirk.Core.AST
import Smirk.Core.Evacuable
import Smirk.Core.Parser

spec :: Spec
spec = describe "Smirk.Core.Parser" $ do
  it "parses simple literal sequence" $ do
    let res = parsePCRE "smirk"
    case res of
      Right (Seq [Lit 's', Lit 'm', Lit 'i', Lit 'r', Lit 'k']) -> pure ()
      other -> expectationFailure $ "Expected Seq of chars but got: " <> show other

  it "parses character classes and digits" $ do
    let res = parsePCRE "[a-z0-9]+\\s*\\d+"
    case res of
      Right (Seq [ Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z', RangeBetween '0' '9'] False))
                 , Repeat 0 Nothing RepGreedy (Class ClassSpace)
                 , Repeat 1 Nothing RepGreedy (Class ClassDigit)
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  it "parses named capture groups and lookahead assertions" $ do
    let res = parsePCRE "(?<user>[a-zA-Z]+)(?=@domain\\.com)"
    case res of
      Right (Seq [ Group (Just "user") Nothing (Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z', RangeBetween 'A' 'Z'] False)))
                 , Lookaround Lookahead True (Seq [Lit '@', Lit 'd', Lit 'o', Lit 'm', Lit 'a', Lit 'i', Lit 'n', Lit '.', Lit 'c', Lit 'o', Lit 'm'])
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  it "parses backreferences" $ do
    let res = parsePCRE "([a-z]+)-\\1"
    case res of
      Right (Seq [ Group Nothing Nothing (Repeat 1 Nothing RepGreedy (Class (ClassSet [RangeBetween 'a' 'z'] False)))
                 , Lit '-'
                 , BackRef 1
                 ]) -> pure ()
      other -> expectationFailure $ "Unexpected AST: " <> show other

  describe "Smirk.Core.Evacuable (Static Nullability & Lookbehind Horizon)" $ do
    describe "isNullableAST" $ do
      it "identifies non-nullable literal and character class expressions" $ do
        case parsePCRE "smirk" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)
        case parsePCRE "[a-z0-9]+" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)

      it "identifies nullable repetition expressions (*, ?, {0,n})" $ do
        case parsePCRE "a*" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a?" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "(a*)+" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a{0,5}" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a{1,5}" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)

      it "identifies nullable alternations and assertions" $ do
        case parsePCRE "a|b*" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)
        case parsePCRE "a|b" of
          Right ast -> isNullableAST ast `shouldBe` False
          Left e    -> expectationFailure (show e)
        case parsePCRE "^$" of
          Right ast -> isNullableAST ast `shouldBe` True
          Left e    -> expectationFailure (show e)

    describe "maxLookbehindHorizon" $ do
      it "computes static lookbehind bounds for bounded lookbehind assertions" $ do
        case parsePCRE "(?<=abc)def" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Just 3
          Left e    -> expectationFailure (show e)
        case parsePCRE "(?<=a{2,5})b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Just 5
          Left e    -> expectationFailure (show e)

      it "returns Nothing for unbounded lookbehind assertions" $ do
        case parsePCRE "(?<=a+)b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Nothing
          Left e    -> expectationFailure (show e)
        case parsePCRE "(?<=a*)b" of
          Right ast -> maxLookbehindHorizon ast `shouldBe` Nothing
          Left e    -> expectationFailure (show e)
