{-# LANGUAGE OverloadedStrings #-}

module Smirk
  ( -- * Core Types & API
    Regex (..)
  , compileRegex
  , compileRegexWithFlags
  , Flags (..)
  , defaultFlags
  , testMatch
  , execMatch
  , execMatchWithFuel

    -- * QuasiQuoters & Editors
  , re
  , ed
  , Editor (..)
  , applyEditor

    -- * AST & Compiler
  , AST (..)
  , Opcode (..)
  , Program
  , parsePCRE
  , parsePCREWithFlags
  , compileAST
  , compileToRegex

    -- * Execution & VM
  , RegexTarget (..)
  , Fuel (..)
  , MatchResult (..)
  , MatchStatus (..)
  , MatchContinuation (..)
  , runMatch
  , runMatchAt
  , resumeMatch
  , findAllMatches

    -- * Algebraic Effects
  , module Smirk.Effect.Effectful
  ) where

import Data.Text (Text)

import Smirk.Core.AST
import Smirk.Core.Bytecode
import Smirk.Core.Compiler
import Smirk.Core.Parser
import Smirk.Core.Quote
import Smirk.Effect.Effectful
import Smirk.Engine.Sequence
import Smirk.Engine.VM

-- | Compile a PCRE regular expression string into a runnable 'Regex'.
compileRegex :: Text -> Either Text Regex
compileRegex = compileRegexWithFlags defaultFlags

-- | Compile a PCRE regular expression string with explicit compilation 'Flags'.
compileRegexWithFlags :: Flags -> Text -> Either Text Regex
compileRegexWithFlags flags input = do
  ast <- parsePCREWithFlags flags input
  pure (compileToRegex input ast)

-- | Test if a target sequence matches a regular expression.
testMatch :: RegexTarget s => Regex -> s -> Bool
testMatch reg target = case runMatch reg target Unlimited of
  Matched _ -> True
  _         -> False

-- | Execute regex search returning the first 'MatchResult', if any.
execMatch :: RegexTarget s => Regex -> s -> Maybe (MatchResult s)
execMatch reg target = case runMatch reg target Unlimited of
  Matched res -> Just res
  _           -> Nothing

-- | Execute regex search bounded by a specific step budget.
execMatchWithFuel :: RegexTarget s => Regex -> s -> Fuel -> MatchStatus s
execMatchWithFuel = runMatch
