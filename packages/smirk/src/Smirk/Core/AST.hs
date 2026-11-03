{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Smirk.Core.AST
Description : Abstract Syntax Tree representation of PCRE regular expressions
Copyright   : (c) Nadia Yvette Chambers, 2026
License     : Apache-2.0

Defines the typed Abstract Syntax Tree (AST) for PCRE regular expressions,
supporting literals, character classes, anchors, sequences, alternations,
bounded repetitions with greediness controls, named/numbered capture groups,
and lookaround assertions.
-}
module Smirk.Core.AST
  ( AST (..)
  , CharClass (..)
  , CharRange (..)
  , Anchor (..)
  , RepKind (..)
  , LookDir (..)
  , Flags (..)
  , defaultFlags
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Regex compilation and matching flags controlling parser and VM semantics.
data Flags = Flags
  { caseInsensitive :: !Bool -- ^ (?i) Case-insensitive character matching
  , dotAll          :: !Bool -- ^ (?s) Dot matches newline characters as well
  , multiline       :: !Bool -- ^ (?m) ^ and $ match at beginning and end of each line
  } deriving (Eq, Show, Generic)

-- | Default flags (standard PCRE mode: case-sensitive, no dotAll, no multiline).
defaultFlags :: Flags
defaultFlags = Flags
  { caseInsensitive = False
  , dotAll          = False
  , multiline       = False
  }

-- | Character range within a character set ([a-z], [0-9]).
data CharRange
  = RangeSingle !Char          -- ^ Single character: @a@
  | RangeBetween !Char !Char   -- ^ Inclusive character range: @a@ to @z@
  deriving (Eq, Show, Generic)

-- | Character classes and custom character sets.
data CharClass
  = ClassAny                      -- ^ @.@ (any character, subject to dotAll)
  | ClassDigit                    -- ^ @\\d@ (ASCII or Unicode decimal digit)
  | ClassNotDigit                 -- ^ @\\D@ (any character that is not a digit)
  | ClassWord                     -- ^ @\\w@ (word character: alphanumeric plus underscore)
  | ClassNotWord                  -- ^ @\\W@ (non-word character)
  | ClassSpace                    -- ^ @\\s@ (whitespace character)
  | ClassNotSpace                 -- ^ @\\S@ (non-whitespace character)
  | ClassSet ![CharRange] !Bool   -- ^ @[a-z0-9]@ or negated @[^a-z]@ (negated when True)
  deriving (Eq, Show, Generic)

-- | Position and boundary assertions that do not consume characters.
data Anchor
  = AnchorStart                   -- ^ @^@ (start of input or start of line in multiline mode)
  | AnchorEnd                     -- ^ @$@ (end of input or end of line in multiline mode)
  | AnchorWordBoundary            -- ^ @\\b@ (word boundary transition)
  | AnchorNotWordBoundary         -- ^ @\\B@ (non-word boundary transition)
  deriving (Eq, Show, Generic)

-- | Repetition greediness and backtracking strategy.
data RepKind
  = RepGreedy                     -- ^ Standard greedy repetition (match as much as possible)
  | RepLazy                       -- ^ Non-greedy / reluctant repetition (match as little as possible)
  | RepPossessive                 -- ^ Possessive repetition (do not backtrack over matched elements)
  deriving (Eq, Show, Generic)

-- | Lookaround assertion direction.
data LookDir
  = Lookahead                     -- ^ Lookahead assertion: inspecting suffix after current position
  | Lookbehind                    -- ^ Lookbehind assertion: inspecting prefix before current position
  deriving (Eq, Show, Generic)

-- | Typed PCRE Abstract Syntax Tree nodes.
data AST
  = Lit !Char                                    -- ^ Exact literal character match
  | Class !CharClass                             -- ^ Character class or set match
  | AnchorAssert !Anchor                         -- ^ Zero-width boundary anchor assertion
  | Seq ![AST]                                   -- ^ Sequential concatenation of sub-expressions
  | Alt ![AST]                                   -- ^ Ordered alternation between sub-expressions
  | Repeat !Int !(Maybe Int) !RepKind !AST       -- ^ Repetition with min count, optional max count, greediness kind, and child AST
  | Group !(Maybe Text) !(Maybe Int) !AST        -- ^ Capturing group with optional name, 1-based index, and inner AST
  | NonCapturing !AST                            -- ^ Non-capturing group: @(?:...)@
  | Lookaround !LookDir !Bool !AST               -- ^ Zero-width lookaround assertion: direction, positive/negative, and inner AST
  | BackRef !Int                                 -- ^ Numbered backreference: @\\1@
  | NamedBackRef !Text                           -- ^ Named backreference: @\\k<name>@
  | Empty                                        -- ^ Empty / epsilon AST node
  deriving (Eq, Show, Generic)
