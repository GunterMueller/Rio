{-# LANGUAGE DataKinds #-}
module Core.Lower (lowerProg) where

import Control.Arrow

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Writer

import qualified Syntax as S
import Syntax (Var(..), Exp)
import Core.Lang

lowerProg :: S.Gamma -> Exp -> [SC p]
lowerProg (S.Gamma env) = execWriter . go (Map.keysSet env) where
  go supercomb (S.Let _ rec (nm, exp) rest) =
    if Set.null ((if rec == S.Rec then Set.delete nm else id) (S.free exp Set.\\ supercomb))
       then do
         makeTopSc nm exp
         go (Set.insert nm supercomb) rest
       else error ("Open top-level let " ++ show exp)

  go _ exp = do
    term <- lowerExp exp
    tell [SC (Var "_main") [] term]

makeTopSc :: Var -> Exp -> Writer [SC p] ()
makeTopSc name exp =
  do
    term <- lowerExp body
    tell [SC name args term]
  where (args, body) = splitLams exp

lowerExp :: Exp -> Writer [SC p] (Term p)
lowerExp (S.Lam spn arg body) = do
  let fvs = Set.toList (S.free body)
      name = Var $ show spn
  body <- lowerExp body
  tell [SC name (reverse (arg:fvs)) body]
  pure $ foldr App (Ref name) (Ref <$> reverse fvs)

lowerExp (S.Use _ v) = pure $ Ref v
lowerExp (S.Num _ v) = pure $ Num v
lowerExp exp@(S.Let _ flag _ _) =
  do let (bound, body) = splitLet flag exp
     bound <- traverse (\(v, _) -> (,) v <$> lowerExp exp) bound
     body <- lowerExp body
     pure $ mkLet bound body
  where
    mkLet = case flag of
      S.Rec -> Letrec
      _   -> Let

    splitLet flag exp@(S.Let _ this (x, v) b) =
      if this == flag then first ((x, v):) (splitLet flag b) else ([], exp)
    splitLet _    b = ([], b)

lowerExp (S.App _ f x) = App <$> lowerExp f <*> lowerExp x

splitLams :: Exp -> ([Var], Exp)
splitLams (S.Lam _ v e) =
  let (args, bd) = splitLams e in (v:args, bd)
splitLams e = ([], e)
