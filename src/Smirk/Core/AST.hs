{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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

-- | Regex compilation and matching flags.
data Flags = Flags
  { caseInsensitive :: !Bool -- ^ (?i)
  , dotAll          :: !Bool -- ^ (?s) dot matches newline
  , multiline       :: !Bool -- ^ (?m) ^ and $ match line boundaries
  } deriving (Eq, Show, Generic)

-- | Default flags (standard PCRE mode).
defaultFlags :: Flags
defaultFlags = Flags
  { caseInsensitive = False
  , dotAll          = False
  , multiline       = False
  }

-- | Character range within a character set ([a-z], [0-9]).
data CharRange
  = RangeSingle !Char
  | RangeBetween !Char !Char
  deriving (Eq, Show, Generic)

-- | Character classes and sets.
data CharClass
  = ClassAny                      -- ^ . (any character, subject to dotAll)
  | ClassDigit                    -- ^ \d
  | ClassNotDigit                 -- ^ \D
  | ClassWord                     -- ^ \w
  | ClassNotWord                  -- ^ \W
  | ClassSpace                    -- ^ \s
  | ClassNotSpace                 -- ^ \S
  | ClassSet ![CharRange] !Bool   -- ^ [a-z0-9] or [^a-z] (negated if True)
  deriving (Eq, Show, Generic)

-- | Position and boundary assertions.
data Anchor
  = AnchorStart         -- ^ ^
  | AnchorEnd           -- ^ $
  | AnchorWordBoundary  -- ^ \b
  | AnchorNotWordBoundary -- ^ \B
  deriving (Eq, Show, Generic)

-- | Repetition greediness.
data RepKind
  = RepGreedy
  | RepLazy
  | RepPossessive
  deriving (Eq, Show, Generic)

-- | Lookaround direction.
data LookDir
  = Lookahead
  | Lookbehind
  deriving (Eq, Show, Generic)

-- | Typed PCRE Abstract Syntax Tree.
data AST
  = Lit !Char
  | Class !CharClass
  | AnchorAssert !Anchor
  | Seq ![AST]
  | Alt ![AST]
  | Repeat !Int !(Maybe Int) !RepKind !AST       -- ^ min, max, kind, inner
  | Group !(Maybe Text) !(Maybe Int) !AST        -- ^ optional name, assigned group index, inner
  | NonCapturing !AST                            -- ^ (?:...)
  | Lookaround !LookDir !Bool !AST               -- ^ dir, isPositive, inner
  | BackRef !Int                                 -- ^ \1
  | NamedBackRef !Text                           -- ^ \k<name>
  | Empty
  deriving (Eq, Show, Generic)
