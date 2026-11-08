{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ParserSpec (spec) where

import Test.Hspec

import Smirk.Core.AST
import Smirk.Core.Bytecode (Opcode (..), data IRangeNat, data ICallout)
import Smirk.Core.Compiler (compileAST)
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

  describe "PCRE Callout Syntax" $ do
    it "parses default callout (?C)" $ do
      case parsePCRE "(?C)" of
        Right (Callout 0 Nothing) -> pure ()
        other -> expectationFailure ("Expected Callout 0 Nothing, got: " <> show other)

    it "parses numbered callouts (?Cn)" $ do
      case parsePCRE "(?C42)" of
        Right (Callout 42 Nothing) -> pure ()
        other -> expectationFailure ("Expected Callout 42 Nothing, got: " <> show other)

    it "parses tagged callouts (?C\"tag\") and (?C'tag')" $ do
      case parsePCRE "(?C\"debug_trace\")" of
        Right (Callout 0 (Just "debug_trace")) -> pure ()
        other -> expectationFailure ("Expected Callout with double-quote tag, got: " <> show other)
      case parsePCRE "(?C'single_tag')" of
        Right (Callout 0 (Just "single_tag")) -> pure ()
        other -> expectationFailure ("Expected Callout with single-quote tag, got: " <> show other)

    it "parses numbered and tagged callouts (?Cn\"tag\")" $ do
      case parsePCRE "(?C12\"marker\")" of
        Right (Callout 12 (Just "marker")) -> pure ()
        other -> expectationFailure ("Expected Callout 12 marker, got: " <> show other)

    it "parses callouts interspersed in sequences" $ do
      case parsePCRE "(?C1)a(?C2)b(?C3)" of
        Right (Seq [Callout 1 Nothing, Lit 'a', Callout 2 Nothing, Lit 'b', Callout 3 Nothing]) -> pure ()
        other -> expectationFailure ("Expected interspersed callout sequence, got: " <> show other)

    it "compiles Callout to OpCallout / ICallout opcode" $ do
      compileAST (Callout 7 (Just "probe")) `shouldBe` [OpCallout 7 (Just "probe")]
      compileAST (Callout 7 (Just "probe")) `shouldBe` [ICallout 7 (Just "probe")]

    it "reports Callout as nullable with 0 match length" $ do
      let calloutNode = Callout 1 (Just "tag")
      isNullableAST calloutNode `shouldBe` True
      astMinLength calloutNode `shouldBe` 0
      astMaxLength calloutNode `shouldBe` Just 0

  describe "Lucene INTERVAL Numeric Range Syntax" $ do
    it "parses parenthesized interval ranges (?<min..max>)" $ do
      case parsePCRE "(?<0..100>)" of
        Right (IntervalNat 0 100) -> pure ()
        other -> expectationFailure ("Expected IntervalNat 0 100, got: " <> show other)
      case parsePCRE "(?<1900..2099>)" of
        Right (IntervalNat 1900 2099) -> pure ()
        other -> expectationFailure ("Expected IntervalNat 1900 2099, got: " <> show other)

    it "disambiguates (?<min..max>) from named capture groups (?<name>pattern)" $ do
      case parsePCRE "(?<count>\\d+)" of
        Right (Group (Just "count") Nothing (Repeat 1 Nothing RepGreedy (Class ClassDigit))) -> pure ()
        other -> expectationFailure ("Expected named group, got: " <> show other)
      case parsePCRE "(?<1..5>)" of
        Right (IntervalNat 1 5) -> pure ()
        other -> expectationFailure ("Expected IntervalNat 1 5, got: " <> show other)

    it "rejects inverted intervals where min > max" $ do
      case parsePCRE "(?<100..10>)" of
        Left _ -> pure ()
        Right ast -> expectationFailure ("Expected parse failure for 100..10, got: " <> show ast)

    it "parses standalone interval syntax <min..max> when intervalFlag is enabled" $ do
      let flags = enableIntervals defaultFlags
      case parsePCREWithFlags flags "<1..255>" of
        Right (IntervalNat 1 255) -> pure ()
        other -> expectationFailure ("Expected IntervalNat 1 255 with intervalFlag, got: " <> show other)

    it "parses <min..max> as literal/class sequence when intervalFlag is disabled" $ do
      case parsePCRE "<1..255>" of
        Right (Seq [Lit '<', Lit '1', Class ClassAny, Class ClassAny, Lit '2', Lit '5', Lit '5', Lit '>']) -> pure ()
        other -> expectationFailure ("Expected literal sequence for <1..255> without flag, got: " <> show other)

    it "compiles IntervalNat to OpRangeNat / IRangeNat opcode" $ do
      compileAST (IntervalNat 10 99) `shouldBe` [OpRangeNat 10 99]
      compileAST (IntervalNat 10 99) `shouldBe` [IRangeNat 10 99]

    it "reports IntervalNat as non-nullable with digit bounds" $ do
      let ivNode = IntervalNat 5 1000
      isNullableAST ivNode `shouldBe` False
      astMinLength ivNode `shouldBe` 1
      astMaxLength ivNode `shouldBe` Just 4
