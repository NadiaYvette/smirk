module Main (main) where

import Test.Hspec
import qualified BudgetSpec
import qualified StrataSpec
import qualified MarpaSpec
import qualified DPOSpec
import qualified SheafParallelSpec
import qualified DPOMinedSpec
import qualified CursorSpec
import qualified ParserSpec
import qualified VMSpec
import qualified SequenceSpec
import qualified QuoteSpec
import qualified GlobSpec
import qualified ZshExpansionSyntaxSpec
import qualified GlobZshSpec
import qualified BatchSpec
import qualified NetworkFilterSpec
import qualified DiskIndexSpec

main :: IO ()
main = hspec $ do
  StrataSpec.spec
  MarpaSpec.spec
  DPOSpec.spec
  SheafParallelSpec.spec
  DPOMinedSpec.spec
  BudgetSpec.spec
  CursorSpec.spec
  ParserSpec.spec
  VMSpec.spec
  SequenceSpec.spec
  QuoteSpec.spec
  GlobSpec.spec
  ZshExpansionSyntaxSpec.spec
  GlobZshSpec.spec
  BatchSpec.spec
  NetworkFilterSpec.spec
  DiskIndexSpec.spec
