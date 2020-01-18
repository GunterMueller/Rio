{-# LANGUAGE OverloadedLists #-}
module Gm.Compile (compileSc) where

import Data.Sequence (Seq)

import qualified Data.Map as Map

import Syntax (Var(..))

import Core.Lang
import Gm.Machine

compileSc :: SC p -> GmSc
compileSc (SC name args body) =
  Supercomb name (length args) (compileR body (Map.fromList (zip args (Arg <$> [0..]))))

type GmCompiler p = Term p -> Map.Map Var GmVal -> Seq GmCode

offset :: Int -> GmVal -> GmVal
offset _ (Global x) = Global x
offset _ (Value i) = Value i
offset k (Local x) = Local (x + k)
offset k (Arg x) = Arg (x + k)

compileR, compileC :: GmCompiler p
compileR e env = compileC e env <> [Update (length env), Pop (length env), Unwind]

compileC (Ref var) env =
  case Map.lookup var env of
    Just idx -> [Push idx]
    Nothing  -> [Push (Global var)]
compileC (Num i) _ = [Push (Value (fromInteger i))]

compileC (App e1 e2) env =
  compileC e1 env <> compileC e2 (offset 1 <$> env) <> [MkAp]

compileC (Let xi e) env = compileLet xi env <> compileC e env' <> [Slide (length xi)] where
  compileLet [] _ = []
  compileLet ((_, e):d) env =
    compileC e env <> compileLet d (offset 1 <$> env)
  env' = addLets xi env

compileC (Letrec xi e) env = [Alloc n] <> compileLet defs <> compileC e env' <> [Slide (length xi)] where
  compileLet []              = []
  compileLet ((k, (_, e)):d) = compileC e env' <> [Update (n - k)] <> compileLet d
  defs = zip [1..n] xi
  n = length xi
  env' = addLets xi env

addLets :: [(Var, Term p)] -> Map.Map Var GmVal -> Map.Map Var GmVal
addLets defs env =
  Map.fromList (zip (map fst defs) (Local <$> [n-1, n-2 .. 0])) <> (offset n <$> env)
    where n = length defs
