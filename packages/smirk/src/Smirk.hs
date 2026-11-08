{- |
Module      : Smirk
Description : Pure, effectful, resumable, step-budgeted PCRE interpreter for Haskell
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

@Smirk@ (;-P / ;-) is a pure, native Haskell PCRE regular expression engine
featuring step-bounded execution (ReDoS defense), pausable/resumable matching,
and @mono-traversable@ sequence polymorphism.

=== Key Architectural Pillars

1. __Step-Bounded Execution & ReDoS Defense:__ Every pattern evaluation can be
   constrained by a discrete 'Fuel' step budget ('StepBudget' or 'Unlimited'). If thread
   state evaluations exceed the threshold, evaluation halts safely and yields
   'OutOfFuel' along with a resumable 't:MatchContinuation'.

2. __Sequence Polymorphism:__ Pattern matching operates uniformly across any
   data type implementing 'RegexTarget', including strict 'Text', @ByteString@,
   'String', @Seq Char@, and @Vector Char@.

3. __Compile-Time QuasiQuoters:__ Static pattern validation via @[re|...|]@,
   stream editor substitution via @[ed|s/find/replace/g|]@, and shell/glob
   matching via @[glob|...|]@.

4. __Modular Effect Handlers:__ Algebraic effect bindings for @effectful@
   (@smirk-effectful@), @polysemy@ (@smirk-polysemy@), and @penelope@ (@smirk-penelope@).
-}
module Smirk
  ( -- * Core Types & API
    Regex (..)
  , compileRegex
  , compileRegexWithFlags
  , compileNonEmptyRegex
  , Flags (..)
  , defaultFlags
  , testMatch
  , execMatch
  , execMatchNotEmpty
  , execMatchWithFuel

    -- * Glob Matching
  , compileGlob
  , compileGlobWithOptions
  , compileWildcard
  , GlobOptions (..)
  , defaultGlobOptions
  , wildcardGlobOptions
  , matchGlob

    -- * QuasiQuoters & Editors
  , re
  , reNE
  , ed
  , glob
  , Editor (..)
  , applyEditor

    -- * AST Analysis & Evacuability
  , AST (..)
  , isNullableAST
  , astMinLength
  , astMaxLength
  , maxLookbehindHorizon
  , collectLookbehinds

    -- * Non-Empty Slices & Targets
  , NonEmptySlice (..)
  , mkNonEmptySlice
  , unsafeNonEmptySlice
  , NonEmptyTarget
  , ToNonEmptyTarget (..)
  , NonEmptyMatchResult (..)
  , toNonEmptyMatchResult

    -- * Compiler & Bytecode
  , Opcode (..)
  , Program
  , parsePCRE
  , parsePCREWithFlags
  , compileAST
  , compileToRegex

    -- * Execution & VM Modes
  , RegexTarget (..)
  , Fuel (..)
  , MatchMode (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , runMatchWithMode
  , runMatchAtWithMode
  , runMatchNotEmpty
  , resumeMatch
  , findAllMatches
  , findAllMatchesWithMode
  , findAllMatchesNotEmpty

    -- * Capture Registers & Polytropos Projections
  , CaptureSlots (..)
  , emptyCaptureSlots
  , getCaptureStart
  , getCaptureEnd
  , getCaptureSpan
  , toCaptureMap
  , toFullMatchSpan
  , toNamedCaptureSmallArray
  , toNamedCaptureRep
  , matchNamedCaptureRep
  , lookupNamedCaptureRep

    -- * Linear Continuation & Graded Modal Cursor Discipline
  , MatchOutcome (..)
  , mkMatchOutcome
  , outcomeToStatus
  , outcomeToMaybeResult
  , ResumeLinear (..)
  , resumeLinear
  , mkResumeLinear
  , LinearContinuation (..)
  , toLinearContinuation
  , fromLinearContinuation
  , resumeLinearCont
  , EvacuatedBytes (..)
  , GradedCursor (..)
  , mkGradedCursor
  , mkGradedCursorAt
  , cursorHorizon
  , advanceMinActive
  , minimumActiveOffset
  , cursorFromContinuation
  , safeEvacuationHorizon
  , evacuatePrefix
  , cursorRemainingLength
  , cursorSlice
  , cursorToRelativeOffset
  , cursorFromRelativeOffset
  ) where

import Data.Text (Text)

import Smirk.Core.AST
import Smirk.Core.Bytecode
import Smirk.Core.Compiler
import Smirk.Core.Cursor
import Smirk.Core.Evacuable
import Smirk.Core.Parser
import Smirk.Core.Quote
import Smirk.Core.Types
import Smirk.Engine.Sequence
import Smirk.Engine.VM
import Smirk.Glob

-- | Compile a PCRE regular expression string into a runnable 't:Regex'.
-- Returns a parse error description as 'Left' on failure.
compileRegex :: Text -> Either Text Regex
compileRegex = compileRegexWithFlags defaultFlags
{-# INLINE compileRegex #-}

-- | Compile a PCRE regular expression string with explicit compilation 't:Flags'.
compileRegexWithFlags :: Flags -> Text -> Either Text Regex
compileRegexWithFlags flags input = do
  ast <- parsePCREWithFlags flags input
  pure (compileToRegex input ast)

-- | Test if a target sequence matches a regular expression.
-- Returns 'True' if a match is found anywhere in the target sequence.
testMatch :: RegexTarget s => Regex -> s -> Bool
testMatch reg target = case runMatch reg target Unlimited of
  Matched _ -> True
  _         -> False

-- | Execute regex search returning the first 't:MatchResult', if any.
execMatch :: RegexTarget s => Regex -> s -> Maybe (MatchResult s)
execMatch reg target = case runMatch reg target Unlimited of
  Matched res -> Just res
  _           -> Nothing

-- | Execute regex search bounded by a specific step budget.
-- Returns 'OutOfFuel' with a continuation if the step threshold is exceeded.
execMatchWithFuel :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
execMatchWithFuel = runMatch
