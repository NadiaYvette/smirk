{-# LANGUAGE OverloadedStrings #-}

module Smirk.Core.Compiler
  ( compileAST
  , compileToRegex
  , assignGroupIndices
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V

import Smirk.Core.AST
import Smirk.Core.Bytecode

-- | Compile an AST into a high-level 'Regex' record.
compileToRegex :: Text -> AST -> Regex
compileToRegex patternStr rawAST =
  let (numberedAST, groupMap, maxIdx) = assignGroupIndices rawAST
      progOps = [OpSaveStart 0] ++ compileAST numberedAST ++ [OpSaveEnd 0, OpMatch]
  in Regex
      { originalPattern = patternStr
      , program         = V.fromList progOps
      , groupNames      = groupMap
      , maxGroupIndex   = maxIdx
      }

-- | Sequentially assign 1-based capture group indices in left-to-right order.
assignGroupIndices :: AST -> (AST, Map Text Int, Int)
assignGroupIndices rootAST =
  let (ast', (finalIdx, groupMap)) = go rootAST (1, Map.empty)
  in (ast', groupMap, finalIdx - 1)
  where
    go :: AST -> (Int, Map Text Int) -> (AST, (Int, Map Text Int))
    go ast (nextIdx, m) = case ast of
      Group mName _ inner ->
        let thisIdx = nextIdx
            m' = case mName of
              Just n  -> Map.insert n thisIdx m
              Nothing -> m
            (inner', st') = go inner (thisIdx + 1, m')
        in (Group mName (Just thisIdx) inner', st')
      Seq items ->
        let (items', st') = foldr (\item (accItems, accSt) ->
                                    let (item', accSt') = go item accSt
                                    in (item' : accItems, accSt')
                                  ) ([], (nextIdx, m)) (reverse items)
        in (Seq (reverse items'), st')
      Alt branches ->
        let (branches', st') = foldr (\b (accB, accSt) ->
                                       let (b', accSt') = go b accSt
                                       in (b' : accB, accSt')
                                     ) ([], (nextIdx, m)) (reverse branches)
        in (Alt (reverse branches'), st')
      Repeat minN mMax kind inner ->
        let (inner', st') = go inner (nextIdx, m)
        in (Repeat minN mMax kind inner', st')
      NonCapturing inner ->
        let (inner', st') = go inner (nextIdx, m)
        in (NonCapturing inner', st')
      Lookaround dir isPos inner ->
        let (inner', st') = go inner (nextIdx, m)
        in (Lookaround dir isPos inner', st')
      other -> (other, (nextIdx, m))

-- | Compile AST node to a flat list of opcodes with relative jump resolution.
compileAST :: AST -> [Opcode]
compileAST = \case
  Empty -> []
  Lit c -> [OpChar c]
  Class cls -> [OpClass cls]
  AnchorAssert a -> [OpAnchor a]
  Seq items -> concatMap compileAST items
  Alt branches -> compileAlt branches
  Repeat minN mMax kind inner -> compileRepeat minN mMax kind inner
  Group _ (Just idx) inner -> [OpSaveStart idx] ++ compileAST inner ++ [OpSaveEnd idx]
  Group _ Nothing inner -> compileAST inner
  NonCapturing inner -> compileAST inner
  Lookaround Lookahead isPos inner ->
    let subProg = V.fromList (compileAST inner ++ [OpMatch])
    in [OpAssertLookahead subProg isPos]
  Lookaround Lookbehind isPos inner ->
    let subProg = V.fromList (compileAST inner ++ [OpMatch])
    in [OpAssertLookbehind subProg isPos]
  BackRef idx -> [OpMatchBackRef idx]
  NamedBackRef _ -> []

compileAlt :: [AST] -> [Opcode]
compileAlt [] = []
compileAlt [singleBranch] = compileAST singleBranch
compileAlt (b1 : bs) =
  let code1 = compileAST b1
      code2 = compileAlt bs
      len1 = length code1
      len2 = length code2
      splitOp = OpSplit 1 (1 + len1 + 1)
      jumpToEnd = OpJump (len2 + 1)
  in [splitOp] ++ code1 ++ [jumpToEnd] ++ code2

compileRepeat :: Int -> Maybe Int -> RepKind -> AST -> [Opcode]
compileRepeat 0 (Just 1) RepGreedy inner =
  let body = compileAST inner
      len = length body
  in [OpSplit 1 (len + 1)] ++ body
compileRepeat 0 (Just 1) RepLazy inner =
  let body = compileAST inner
      len = length body
  in [OpSplit (len + 1) 1] ++ body
compileRepeat 0 Nothing RepGreedy inner =
  let body = compileAST inner
      len = length body
      loop = [OpSplit 1 (len + 2)] ++ body ++ [OpJump (- (len + 1))]
  in loop
compileRepeat 0 Nothing RepLazy inner =
  let body = compileAST inner
      len = length body
      loop = [OpSplit (len + 2) 1] ++ body ++ [OpJump (- (len + 1))]
  in loop
compileRepeat 1 Nothing RepGreedy inner =
  let body = compileAST inner
      len = length body
  in body ++ [OpSplit (- len) 1]
compileRepeat 1 Nothing RepLazy inner =
  let body = compileAST inner
      len = length body
  in body ++ [OpSplit 1 (- len)]
compileRepeat minN mMax kind inner =
  let exact = concat (replicate minN (compileAST inner))
      extra = case mMax of
        Nothing -> compileRepeat 0 Nothing kind inner
        Just maxN | maxN > minN ->
          let remN = maxN - minN
              single = compileRepeat 0 (Just 1) kind inner
          in concat (replicate remN single)
        _ -> []
  in exact ++ extra
