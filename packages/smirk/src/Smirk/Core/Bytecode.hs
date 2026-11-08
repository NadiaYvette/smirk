{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Module      : Smirk.Core.Bytecode
Description : Virtual machine bytecode instruction set and compiled program structures
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Defines the low-level virtual machine bytecode instruction set ('Opcode')
used by the Pike-style non-backtracking / thread-stepped VM in 'Smirk.Engine.VM'.
Includes relative jumping, nondeterministic thread branching ('OpSplit'),
capture slot registers ('OpSaveStart', 'OpSaveEnd'), lookaround subprograms,
Lucene numeric natural intervals ('OpRangeNat' / 'IRangeNat'), and PCRE
callout hooks ('OpCallout' / 'ICallout').
-}
module Smirk.Core.Bytecode
  ( Opcode (..)
  , data IRangeNat
  , data ICallout
  , Program
  , Regex (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Generics (Generic)

import Smirk.Core.AST (Anchor, CharClass)

-- | Low-level bytecode instruction for the Smirk regex virtual machine.
data Opcode
  = OpChar !Char                                    -- ^ Match a single literal character
  | OpClass !CharClass                             -- ^ Match a character class or set
  | OpAnchor !Anchor                               -- ^ Assert a zero-width anchor condition
  | OpSplit !Int !Int                              -- ^ Fork execution: try relative offset 1, alternative at offset 2
  | OpJump !Int                                    -- ^ Unconditional relative jump by specified offset
  | OpSaveStart !Int                               -- ^ Save current input position as start of capture group $n$
  | OpSaveEnd !Int                                 -- ^ Save current input position as end of capture group $n$
  | OpAssertLookahead !(Vector Opcode) !Bool       -- ^ Zero-width lookahead assertion: subprogram and polarity
  | OpAssertLookbehind !(Vector Opcode) !Bool      -- ^ Zero-width lookbehind assertion: subprogram and polarity
  | OpMatchBackRef !Int                            -- ^ Match dynamic slice previously captured by group $n$
  | OpRangeNat !Word64 !Word64                     -- ^ Lucene-style decimal natural interval [min..max] [ref:note-lucene-interval-syntax]
  | OpCallout !Int !(Maybe Text)                   -- ^ PCRE Callout hook with callout index and optional tag [ref:note-pcre-callout-syntax]
  | OpMatch                                        -- ^ Successful match reached; terminate thread
  deriving (Eq, Show, Generic)

-- | Pattern synonym for Lucene-style decimal natural interval bytecode instruction.
-- Matches decimal numerals whose numerical value is within $[ \text{min}, \text{max} ]$
-- in $O(\text{digits})$ time with zero backtracking.
-- [ref:note-lucene-interval-syntax]
pattern IRangeNat :: Word64 -> Word64 -> Opcode
pattern IRangeNat lo hi = OpRangeNat lo hi

-- | Pattern synonym for PCRE callout bytecode instruction.
-- Represents a zero-width inspection hook carrying a numeric identifier and optional tag.
-- [ref:note-pcre-callout-syntax]
pattern ICallout :: Int -> Maybe Text -> Opcode
pattern ICallout num tag = OpCallout num tag

-- | Compiled bytecode sequence indexed as a contiguous vector.
type Program = Vector Opcode

-- | Complete compiled regular expression ready for VM execution.
data Regex = Regex
  { originalPattern :: !Text                      -- ^ Original PCRE source pattern string
  , program         :: !Program                   -- ^ Compiled bytecode instruction vector
  , groupNames      :: !(Map Text Int)            -- ^ Mapping from named capture groups to 1-based indices
  , maxGroupIndex   :: !Int                       -- ^ Highest capture group index allocated in the pattern
  } deriving (Eq, Show, Generic)
