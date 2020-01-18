module Gm.Machine where

import Data.Sequence (Seq)
import Syntax (Var(..))

data GmVal
  = Global Var
  | Value  Int
  | Arg    Int
  | Local  Int
  deriving (Eq, Show, Ord)

data GmCode
  = Push GmVal
  | Update Int
  | Alloc Int
  | Slide Int
  | Pop Int
  | Unwind
  | MkAp
  deriving (Eq, Show, Ord)

data GmSc = Supercomb Var Int (Seq GmCode)
  deriving (Eq, Show, Ord)
