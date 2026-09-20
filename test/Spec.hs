module Main (main) where

import Test.Hspec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified EffectSpec
import qualified QuoteSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  EffectSpec.spec
  QuoteSpec.spec
