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
  | Pop Int
  | Update Int
  | Alloc Int
  | Slide Int
  | Cond (Seq GmCode) (Seq GmCode)
  | Unwind
  | MkAp
  | Eval
  | Add | Sub | Mul | Div
  | Equ
  deriving (Eq, Show, Ord)

data GmSc = Supercomb Var Int (Seq GmCode)
  deriving (Eq, Show, Ord)
