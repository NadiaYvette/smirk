module Main (main) where

import Test.Hspec
import qualified BudgetSpec
import qualified StrataSpec
import qualified CursorSpec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified QuoteSpec
import qualified GlobSpec
import qualified BatchSpec
import qualified NetworkFilterSpec
import qualified DiskIndexSpec

main :: IO ()
main = hspec $ do
  StrataSpec.spec
  BudgetSpec.spec
  CursorSpec.spec
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  QuoteSpec.spec
  GlobSpec.spec
  BatchSpec.spec
  NetworkFilterSpec.spec
  DiskIndexSpec.spec
