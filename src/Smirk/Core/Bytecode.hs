{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Smirk.Core.Bytecode
  ( Opcode (..)
  , Program
  , Regex (..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Smirk.Core.AST (Anchor, CharClass)

-- | Bytecode instruction for the Smirk backtracking VM.
data Opcode
  = OpChar !Char
  | OpClass !CharClass
  | OpAnchor !Anchor
  | OpSplit !Int !Int
  | OpJump !Int
  | OpSaveStart !Int
  | OpSaveEnd !Int
  | OpAssertLookahead !(Vector Opcode) !Bool
  | OpAssertLookbehind !(Vector Opcode) !Bool
  | OpMatchBackRef !Int
  | OpMatch
  deriving (Eq, Show, Generic)

-- | Compiled bytecode sequence.
type Program = Vector Opcode

-- | Complete compiled regular expression ready for execution.
data Regex = Regex
  { originalPattern :: !Text
  , program         :: !Program
  , groupNames      :: !(Map Text Int)
  , maxGroupIndex   :: !Int
  } deriving (Eq, Show, Generic)
