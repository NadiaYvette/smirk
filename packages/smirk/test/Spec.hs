module Main (main) where

import Test.Hspec
import qualified BudgetSpec
import qualified CursorSpec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified QuoteSpec
import qualified GlobSpec

main :: IO ()
main = hspec $ do
  BudgetSpec.spec
  CursorSpec.spec
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  QuoteSpec.spec
  GlobSpec.spec
