{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Smirk.Core.Strata
Description : Formal language classification and Glushkov bit-parallel fast-path
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Classifies PCRE patterns into formal language hierarchy strata:
  1. Regular expressions (StratumRegular) — fast-pathable via Glushkov bit-parallel NFA.
  2. Context-free grammars (StratumCFG) — recursive subpatterns requiring Marpa/Earley.
  3. Context-sensitive patterns (StratumBackref) — backreferences requiring Pike VM.

-- Note [Glushkov Bit-Parallel Fast-Path]
For regular expressions with <= 64 syntactic character positions, the Glushkov
position automaton is compiled into unboxed Word64 bitmasks:
  - Initial set of positions: Word64
  - Final set of positions: Word64
  - Follow set matrix: Vector Word64
  - Character predicate dispatch: Char -> Word64

Matching advances state sets via bitwise operations in strict O(n) time and O(1) space,
achieving zero-allocation throughput without backtracking.
-}
module Smirk.Core.Strata
  ( -- * Language Strata
    LanguageStratum (..)
  , classifyAST

    -- * Glushkov Bit-Parallel Automaton
  , GlushkovNFA (..)
  , compileGlushkov
  , stepGlushkov
  , matchGlushkov
  , matchGlushkovText
  ) where

import Data.Bits ((.&.), (.|.), setBit, testBit)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)

import Smirk.Core.AST

-- | Formal language hierarchy stratum for a regex pattern.
data LanguageStratum
  = StratumRegular  -- ^ Type-3 Regular language (Glushkov / Thompson NFA / DFA)
  | StratumCFG      -- ^ Type-2 Context-Free Grammar (Recursive (?R), (?1) subpatterns)
  | StratumBackref  -- ^ Type-1 Context-Sensitive (Numbered \1 or named \k<name> backreferences)
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Classify an Abstract Syntax Tree into its minimal required language stratum.
--
-- Traverses the AST structurally, computing the least upper bound in the stratum lattice:
-- StratumRegular < StratumCFG < StratumBackref.
classifyAST :: AST -> LanguageStratum
classifyAST = \case
  BackRef _ -> StratumBackref
  NamedBackRef _ -> StratumBackref
  Callout _ _ -> StratumCFG
  Seq xs -> foldl max StratumRegular (map classifyAST xs)
  Alt xs -> foldl max StratumRegular (map classifyAST xs)
  Repeat _ _ _ child -> classifyAST child
  Group _ _ child -> classifyAST child
  NonCapturing child -> classifyAST child
  Lookaround _ _ child -> classifyAST child
  Lit _ -> StratumRegular
  Class _ -> StratumRegular
  AnchorAssert _ -> StratumRegular
  IntervalNat _ _ -> StratumRegular
  Empty -> StratumRegular

-- | Unboxed Word64-backed Glushkov non-deterministic finite automaton.
--
-- Restricts regular patterns to <= 64 positions for bit-parallel O(1) state representation.
data GlushkovNFA = GlushkovNFA
  { gPosCount       :: !Int
  -- ^ Number of syntactic positions (<= 64)
  , gInitialStates  :: !Word64
  -- ^ Bitmask of initial active positions
  , gFinalStates    :: !Word64
  -- ^ Bitmask of accepting positions
  , gFollow         :: !(Vector Word64)
  -- ^ Flat follow-set transition table indexed by position
  , gMatchPos       :: !(Vector (Char -> Bool))
  -- ^ Position character acceptance predicates
  , gNullable       :: !Bool
  -- ^ True if epsilon is accepted
  } deriving (Generic)

instance Show GlushkovNFA where
  show GlushkovNFA{..} =
    "GlushkovNFA{gPosCount = " ++ show gPosCount ++ ", gNullable = " ++ show gNullable ++ "}"

-- | Compile a StratumRegular AST into a GlushkovNFA.
--
-- Returns Nothing if the pattern exceeds 64 positions or requires CFG/backreferences.
compileGlushkov :: AST -> Maybe GlushkovNFA
compileGlushkov ast
  | classifyAST ast /= StratumRegular = Nothing
  | otherwise =
      let (positions, nullable, firstSet, lastSet, followPairs) = linearize ast 0
          n = length positions
      in if n > 64
           then Nothing
           else
             let initialMask = toMask firstSet
                 finalMask   = toMask lastSet
                 followVec   = buildFollowTable n followPairs
                 matchVec    = V.fromList (map snd positions)
             in Just GlushkovNFA
                  { gPosCount      = n
                  , gInitialStates = initialMask
                  , gFinalStates   = finalMask
                  , gFollow        = followVec
                  , gMatchPos      = matchVec
                  , gNullable      = nullable
                  }

-- | Linearize regular AST into position-annotated tokens.
--
-- Returns (positions, nullable, firstSet, lastSet, followPairs).
linearize :: AST -> Int -> ([(Int, Char -> Bool)], Bool, [Int], [Int], [(Int, Int)])
linearize ast startIdx = case ast of
  Empty -> ([], True, [], [], [])
  AnchorAssert _ -> ([], True, [], [], [])
  Callout _ _ -> ([], True, [], [], [])
  Lit c ->
    let p = startIdx
    in ([(p, (== c))], False, [p], [p], [])
  Class cls ->
    let p = startIdx
        predFn = matchesClass cls
    in ([(p, predFn)], False, [p], [p], [])
  IntervalNat _ _ ->
    let p = startIdx
        predFn = isDigit
    in ([(p, predFn)], False, [p], [p], [])
  Seq xs ->
    foldl mergeSeq ([], True, [], [], []) (linearizeList xs startIdx)
  Alt xs ->
    foldl mergeAlt ([], False, [], [], []) (linearizeList xs startIdx)
  Repeat minR _ _ child ->
    let (cPos, cNull, cFirst, cLast, cFollow) = linearize child startIdx
        loopFollow = [(u, v) | u <- cLast, v <- cFirst]
        nullable = (minR == 0) || cNull
    in (cPos, nullable, cFirst, cLast, cFollow ++ loopFollow)
  Group _ _ child -> linearize child startIdx
  NonCapturing child -> linearize child startIdx
  Lookaround _ _ child -> linearize child startIdx
  BackRef _ -> ([], False, [], [], [])
  NamedBackRef _ -> ([], False, [], [], [])
  where
    linearizeList [] _ = []
    linearizeList (y:ys) idx =
      let r@(pos, _, _, _, _) = linearize y idx
          nextIdx = idx + length pos
      in r : linearizeList ys nextIdx

    mergeSeq (p1, n1, f1, l1, fol1) (p2, n2, f2, l2, fol2) =
      let crossFollow = [(u, v) | u <- l1, v <- f2]
          newFirst = if n1 then f1 ++ f2 else f1
          newLast  = if n2 then l1 ++ l2 else l2
      in (p1 ++ p2, n1 && n2, newFirst, newLast, fol1 ++ fol2 ++ crossFollow)

    mergeAlt (p1, n1, f1, l1, fol1) (p2, n2, f2, l2, fol2) =
      (p1 ++ p2, n1 || n2, f1 ++ f2, l1 ++ l2, fol1 ++ fol2)

-- | Test if a character satisfies a CharClass.
matchesClass :: CharClass -> Char -> Bool
matchesClass cls c = case cls of
  ClassAny -> c /= '\n'
  ClassDigit -> isDigit c
  ClassNotDigit -> not (isDigit c)
  ClassWord -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'
  ClassNotWord -> not (isAsciiLower c || isAsciiUpper c || isDigit c || c == '_')
  ClassSpace -> isSpace c
  ClassNotSpace -> not (isSpace c)
  ClassSet ranges negated ->
    let inRange (RangeSingle ch) = c == ch
        inRange (RangeBetween lo hi) = c >= lo && c <= hi
        m = any inRange ranges
    in if negated then not m else m

-- | Convert position list into Word64 bitmask.
toMask :: [Int] -> Word64
toMask = foldl (\acc p -> if p < 64 then setBit acc p else acc) 0

-- | Build follow table as a Vector of Word64 bitmasks.
buildFollowTable :: Int -> [(Int, Int)] -> Vector Word64
buildFollowTable n edges =
  let initial = V.replicate n (0 :: Word64)
  in V.accum (.|.) initial [(u, setBit 0 v) | (u, v) <- edges, u < n, v < 64]

-- | Transition active state bitmask across a single input character.
--
-- For each active position i, its follow positions that match c become active.
{-# INLINE stepGlushkov #-}
stepGlushkov :: GlushkovNFA -> Word64 -> Char -> Word64
stepGlushkov GlushkovNFA{..} currentMask c =
  let followMask = foldFollow 0 0
      foldFollow !acc !p
        | p >= gPosCount = acc
        | testBit currentMask p = foldFollow (acc .|. (gFollow V.! p)) (p + 1)
        | otherwise = foldFollow acc (p + 1)
      filterMatch !acc !p
        | p >= gPosCount = acc
        | testBit followMask p && (gMatchPos V.! p) c = filterMatch (setBit acc p) (p + 1)
        | otherwise = filterMatch acc (p + 1)
  in filterMatch 0 0

-- | Compute initial active positions that match the first character.
initMask :: GlushkovNFA -> Char -> Word64
initMask GlushkovNFA{..} c =
  let filterInit !acc !p
        | p >= gPosCount = acc
        | testBit gInitialStates p && (gMatchPos V.! p) c = filterInit (setBit acc p) (p + 1)
        | otherwise = filterInit acc (p + 1)
  in filterInit 0 0

-- | Execute Glushkov bit-parallel matching over a String input.
--
-- Returns True if the entire input matches the pattern.
matchGlushkov :: GlushkovNFA -> String -> Bool
matchGlushkov nfa@GlushkovNFA{..} = \case
  [] -> gNullable
  (c:cs) ->
    let active0 = initMask nfa c
        go !mask = \case
          [] -> (mask .&. gFinalStates) /= 0
          (ch:chs)
            | mask == 0 -> False
            | otherwise -> go (stepGlushkov nfa mask ch) chs
    in go active0 cs

-- | Execute Glushkov bit-parallel matching over a Data.Text input.
matchGlushkovText :: GlushkovNFA -> Text -> Bool
matchGlushkovText nfa txt = matchGlushkov nfa (T.unpack txt)
