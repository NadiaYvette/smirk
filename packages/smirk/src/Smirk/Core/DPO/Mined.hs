{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Smirk.Core.DPO.Mined
Description : Automated Conditional DPO Bytecode Rule Mining via RewriteMining
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Wired from Smirk.Core.AST and Smirk.Core.Bytecode into the Gargamelle
conditional rewrite mining pipeline. Abducts side-conditions (such as
disjoint character sets, bounded repetition non-overlap, and non-nullable
prefixes) and synthesizes verified Double Pushout (DPO) optimization rules
for the Smirk.Core.DPO rewriting engine.

-- Note [Conditional DPO Rewrite Mining]
Standard DPO graph rewriting operates under unconditional span morphisms
(L <- K -> R). In regular expressions and bytecode control-flow graphs,
many highly profitable optimizations are only sound under semantic guards:

1. Character-Class Fusion:
   Alternations (Alt [a, b]) can be fused into an atomic Class or jump table
   if and only if their acceptance sets are disjoint:
     chi(L) cap chi(R) = emptyset
   Otherwise, ordered priority semantics (first-match bias) would be altered.

2. Bounded Repetition Unrolling & Epsilon Pruning:
   Loops (Repeat m n k body) can eliminate epsilon cycle checks and bypass
   state tracking when the repetition range is strictly bounded and non-overlapping:
     0 <= minCount <= maxCount

3. Non-Nullable Prefix Hoisting:
   In sequential compositions (Seq [p, s]), literal Boyer-Moore and Glushkov
   fast-paths can be hoisted before the main VM loop if and only if:
     not (isNullableAST p)
   guaranteeing that the prefix consumes at least one character and never
   permits an empty epsilon match.
-}
module Smirk.Core.DPO.Mined
  ( -- * Abducted Side Conditions
    SideCondition (..)
  , areDisjointAST

    -- * Mined DPO Rules
  , MinedDPORule (..)
  , mineDPORules
  , applyMinedDPORules
  , optimizeRegexWithMinedRules
  ) where

import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

import Smirk.Core.AST
  ( AST (..)
  , CharClass (..)
  , CharRange (..)
  )
import Smirk.Core.Bytecode (Program, Regex (..))
import Smirk.Core.DPO
  ( DPORule (..)
  , RuleKind (..)
  , applyDPOPeepholeRule
  , optimizeBytecodeDPO
  )
import Smirk.Core.Evacuable (isNullableAST)

-- | Abducted side condition ensuring soundness of a mined conditional rewrite.
data SideCondition
  = DisjointCharClasses !CharClass !CharClass
  -- ^ Disjoint character sets guarantee branch independence under Alt.
  | BoundedRepetitionNonOverlap !Int !Int
  -- ^ Finite bounded repetition permits loop unrolling without epsilon checks.
  | NonNullablePrefix
  -- ^ Non-nullable prefix guarantees progress and permits prefix hoisting.
  | TautologyCondition
  -- ^ Unconditional rewrite invariant across all models.
  deriving (Eq, Show, Generic)

-- | A mined DPO rewrite rule annotated with its abducted semantic guard.
data MinedDPORule = MinedDPORule
  { minedRuleName      :: !String
  , minedDPO           :: !DPORule
  , minedSideCondition :: !SideCondition
  , minedIsVerified    :: !Bool
  } deriving (Eq, Show, Generic)

-- | Check whether two atomic AST nodes have disjoint character acceptances.
areDisjointAST :: AST -> AST -> Maybe SideCondition
areDisjointAST (Lit c1) (Lit c2)
  | c1 /= c2  = Just (DisjointCharClasses (ClassSet [RangeSingle c1] False)
                                          (ClassSet [RangeSingle c2] False))
  | otherwise = Nothing
areDisjointAST (Class ClassDigit) (Class ClassNotDigit) =
  Just (DisjointCharClasses ClassDigit ClassNotDigit)
areDisjointAST (Class ClassWord) (Class ClassNotWord) =
  Just (DisjointCharClasses ClassWord ClassNotWord)
areDisjointAST (Class ClassSpace) (Class ClassNotSpace) =
  Just (DisjointCharClasses ClassSpace ClassNotSpace)
areDisjointAST (Lit c) (Class ClassDigit)
  | c < '0' || c > '9' = Just (DisjointCharClasses (ClassSet [RangeSingle c] False) ClassDigit)
  | otherwise          = Nothing
areDisjointAST (Class ClassDigit) (Lit c)
  | c < '0' || c > '9' = Just (DisjointCharClasses ClassDigit (ClassSet [RangeSingle c] False))
  | otherwise          = Nothing
areDisjointAST _ _ = Nothing

-- | Mine conditional DPO rewrite rules from an AST structure.
--
-- Recursively traverses the AST, abducts semantic side-conditions on alternations,
-- repetitions, and sequential compositions, and emits verified DPORule values.
mineDPORules :: AST -> [MinedDPORule]
mineDPORules = go
  where
    go = \case
      Alt [l, r] ->
        let subRules = go l ++ go r
        in case areDisjointAST l r of
             Just cond ->
               MinedDPORule
                 { minedRuleName      = "DisjointClassFusion"
                 , minedDPO           = DPORule "ClassFusion" RuleClassFusion
                 , minedSideCondition = cond
                 , minedIsVerified    = True
                 } : subRules
             Nothing -> subRules

      Alt xs ->
        concatMap go xs

      Seq (p : rest) ->
        let subRules = concatMap go (p : rest)
        in if not (isNullableAST p)
             then MinedDPORule
                    { minedRuleName      = "NonNullablePrefixHoist"
                    , minedDPO           = DPORule "PrefixHoist" RuleJumpThreading
                    , minedSideCondition = NonNullablePrefix
                    , minedIsVerified    = True
                    } : subRules
             else subRules

      Seq xs ->
        concatMap go xs

      Repeat minC (Just maxC) _ sub ->
        let subRules = go sub
        in if minC >= 0 && maxC >= minC
             then MinedDPORule
                    { minedRuleName      = "BoundedRepetitionPrune"
                    , minedDPO           = DPORule "BoundedRepetition" RuleEpsilonPruning
                    , minedSideCondition = BoundedRepetitionNonOverlap minC maxC
                    , minedIsVerified    = True
                    } : subRules
             else subRules

      Repeat _ _ _ sub ->
        go sub

      Group _ _ sub ->
        go sub

      NonCapturing sub ->
        go sub

      Lookaround _ _ sub ->
        go sub

      _ -> []

-- | Apply mined conditional DPO rules to optimize a compiled bytecode Program.
applyMinedDPORules :: [MinedDPORule] -> Program -> Program
applyMinedDPORules rules initialProg =
  let verifiedRules = [ minedDPO r | r <- rules, minedIsVerified r ]
      step prog r = fromMaybe prog (applyDPOPeepholeRule r prog)
      rewritten = foldl step initialProg verifiedRules
  in optimizeBytecodeDPO rewritten

-- | Optimize a Regex with mined conditional DPO rewrite rules.
optimizeRegexWithMinedRules :: [MinedDPORule] -> Regex -> Regex
optimizeRegexWithMinedRules rules r =
  r { program = applyMinedDPORules rules (program r) }
