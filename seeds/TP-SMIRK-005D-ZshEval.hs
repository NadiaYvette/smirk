{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Smirk.Zsh.Expansion.Eval
Description : Two-sorted scalar/array Penelope evaluator for Zsh parameter expansions
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0
Maintainer  : Nadia Chambers <nadia.yvette.chambers@ik.me>
Stability   : experimental
Portability : non-portable (GHC extensions)

Implements the two-sorted ('ValScalar' vs. 'ValArray') inside-out evaluation
pipeline over the 'Smirk.Zsh.Expansion.Syntax' AST, respecting exact Zsh
semantic fidelity for nested expansion, quote tracking, array preservation
under @(@)@, splitting, joining, and pattern stripping.
-}
module Smirk.Zsh.Expansion.Eval
  ( -- * Two-Sorted Value Domain
    TwoSortedVal (..)
  , toScalar
  , toArray
  , smallArrayToList
  , smallArrayFromList

    -- * Evaluation Engine
  , evalZshExpansion
  , applyExpansionFlags
  , applySubscript
  , applyStrip
  , applySubst
  ) where

import Control.Monad (forM_)
import Data.Primitive.SmallArray
  ( SmallArray
  , indexSmallArray
  , newSmallArray
  , runSmallArray
  , sizeofSmallArray
  , writeSmallArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Smirk (compileGlob, testMatch)
import Smirk.Zsh.Expansion.Syntax
  ( StripOp (..)
  , Subscript (..)
  , SubstOp (..)
  , ZshFlag (..)
  , ZshParamExpansion (..)
  )

-- | Two-sorted value domain distinguishing atomic scalar text from multi-word arrays.
data TwoSortedVal
  = ValScalar !Text                 -- ^ Atomic text string
  | ValArray !(SmallArray Text)     -- ^ Flat unboxed SmallArray of words
  deriving stock (Eq, Show, Generic)

-- | Convert any 'TwoSortedVal' to its scalar representation.
-- Array words are coalesced with space delimiters.
toScalar :: TwoSortedVal -> Text
toScalar (ValScalar t)   = t
toScalar (ValArray arr)  = T.intercalate " " (smallArrayToList arr)

-- | Convert any 'TwoSortedVal' to an array of words.
toArray :: TwoSortedVal -> SmallArray Text
toArray (ValArray arr)  = arr
toArray (ValScalar t)   = smallArrayFromList [t]

-- | Convert a 'SmallArray' into a list of elements.
smallArrayToList :: SmallArray a -> [a]
smallArrayToList sa = [ indexSmallArray sa i | i <- [0 .. sizeofSmallArray sa - 1] ]

-- | Construct an immutable 'SmallArray' from a list of elements.
smallArrayFromList :: [a] -> SmallArray a
smallArrayFromList xs =
  let !len = length xs
  in runSmallArray $ do
       marr <- newSmallArray len (error "uninitialized slot")
       forM_ (zip [0..] xs) $ \(i, x) -> writeSmallArray marr i x
       pure marr

-- | Evaluate a 'ZshParamExpansion' inside-out against an ambient variable lookup function.
evalZshExpansion ::
  (Text -> Maybe TwoSortedVal)
  -- ^ Environment variable resolver
  -> ZshParamExpansion
  -- ^ Parsed parameter expansion AST
  -> Either Text TwoSortedVal
evalZshExpansion envLookup ast = do
  -- 1. Inside-out evaluation: resolve variable or inner nested expansion
  rawVal <- case ast.expVarOrInner of
    Left varName -> case envLookup varName of
      Just val -> pure val
      Nothing  -> Left ("Variable not found in environment: " <> varName)
    Right innerAst -> evalZshExpansion envLookup innerAst

  -- 2. Quoting and array preservation interaction:
  -- If outer context is quoted, arrays coalesce to scalars unless (@) flag is present.
  let hasAtFlag = FlagAtArrayPreserve `elem` ast.expFlags
      valQuoted = if ast.expQuoted && not hasAtFlag
                    then ValScalar (toScalar rawVal)
                    else rawVal

  -- 3. Apply strip operators (#, ##, %, %%)
  valStripped <- case ast.expStrip of
    Just stripOp -> pure (applyStrip stripOp valQuoted)
    Nothing      -> pure valQuoted

  -- 4. Apply pattern substitutions (:s, :gs)
  valSubst <- case ast.expSubst of
    Just substOp -> pure (applySubst substOp valStripped)
    Nothing      -> pure valStripped

  -- 5. Apply modifier flags (s/sep/, j/sep/)
  let valFlagged = applyExpansionFlags ast.expFlags valSubst

  -- 6. Apply subscript indexing [1] or [1,3]
  valFinal <- case ast.expSubscript of
    Just sub -> pure (applySubscript sub valFlagged)
    Nothing  -> pure valFlagged

  pure valFinal

-- | Apply splitting and joining flags to a 'TwoSortedVal'.
applyExpansionFlags :: [ZshFlag] -> TwoSortedVal -> TwoSortedVal
applyExpansionFlags flags val = foldl applyFlag val flags
  where
    applyFlag current = \case
      FlagSplit sep -> case current of
        ValScalar txt -> ValArray (smallArrayFromList (T.splitOn sep txt))
        ValArray arr  ->
          let wordsList = concatMap (T.splitOn sep) (smallArrayToList arr)
          in ValArray (smallArrayFromList wordsList)
      FlagJoin sep -> case current of
        ValArray arr  -> ValScalar (T.intercalate sep (smallArrayToList arr))
        ValScalar txt -> ValScalar txt
      _ -> current

-- | Apply 1-based subscript indexing or slicing to a 'TwoSortedVal'.
applySubscript :: Subscript -> TwoSortedVal -> TwoSortedVal
applySubscript sub val = case val of
  ValArray arr ->
    let list = smallArrayToList arr
        len = length list
    in case sub of
      SubscriptSingle idx ->
        if idx >= 1 && idx <= len
          then ValScalar (list !! (idx - 1))
          else ValScalar ""
      SubscriptRange start end ->
        let s = max 1 start - 1
            e = min len end
            slice = if s < e then take (e - s) (drop s list) else []
        in ValArray (smallArrayFromList slice)
  ValScalar txt ->
    let len = T.length txt
    in case sub of
      SubscriptSingle idx ->
        if idx >= 1 && idx <= len
          then ValScalar (T.singleton (T.index txt (idx - 1)))
          else ValScalar ""
      SubscriptRange start end ->
        let s = max 1 start - 1
            e = min len end
            slice = if s < e then T.take (e - s) (T.drop s txt) else ""
        in ValScalar slice

-- | Apply prefix and suffix pattern stripping operators.
applyStrip :: StripOp -> TwoSortedVal -> TwoSortedVal
applyStrip stripOp = \case
  ValScalar txt -> ValScalar (stripText stripOp txt)
  ValArray arr  -> ValArray (smallArrayFromList (map (stripText stripOp) (smallArrayToList arr)))
  where
    stripText op txt = case op of
      StripSuffixGreedy pat -> stripSuffixBy (compileGlob (pat <> "$")) True txt
      StripSuffixNonGreedy pat -> stripSuffixBy (compileGlob (pat <> "$")) False txt
      StripPrefixGreedy pat -> stripPrefixBy (compileGlob ("^" <> pat)) True txt
      StripPrefixNonGreedy pat -> stripPrefixBy (compileGlob ("^" <> pat)) False txt

    stripSuffixBy (Right reg) greedy txt =
      let len = T.length txt
          suffixes = if greedy
                       then [ (i, T.drop i txt) | i <- [0 .. len] ]
                       else [ (i, T.drop i txt) | i <- reverse [0 .. len] ]
          matches = [ i | (i, s) <- suffixes, testMatch reg s ]
      in case matches of
           (cutIdx : _) -> T.take cutIdx txt
           []           -> txt
    stripSuffixBy _ _ txt = txt

    stripPrefixBy (Right reg) greedy txt =
      let len = T.length txt
          prefixes = if greedy
                       then [ (i, T.take i txt) | i <- reverse [0 .. len] ]
                       else [ (i, T.take i txt) | i <- [0 .. len] ]
          matches = [ i | (i, p) <- prefixes, testMatch reg p ]
      in case matches of
           (cutIdx : _) -> T.drop cutIdx txt
           []           -> txt
    stripPrefixBy _ _ txt = txt

-- | Apply pattern substitution operators.
applySubst :: SubstOp -> TwoSortedVal -> TwoSortedVal
applySubst op = \case
  ValScalar txt -> ValScalar (substText op txt)
  ValArray arr  -> ValArray (smallArrayFromList (map (substText op) (smallArrayToList arr)))
  where
    substText (SubstOp isGlob pat rep) txt =
      if isGlob
        then T.replace pat rep txt
        else case T.breakOn pat txt of
          (before, matchPart)
            | not (T.null matchPart) -> before <> rep <> T.drop (T.length pat) matchPart
            | otherwise              -> txt
