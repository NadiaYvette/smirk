{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

import Smirk.Core.AST
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
