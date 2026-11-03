module Main (main) where

import Test.Hspec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified QuoteSpec
import qualified GlobSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  QuoteSpec.spec
  GlobSpec.spec
