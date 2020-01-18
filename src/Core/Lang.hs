{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Core.Lang (Term(..), SC(..)) where

import Syntax (Var)

data Term p
  = Let [(Var, Term p)] (Term p) 
  | Letrec [(Var, Term p)] (Term p)
  | App (Term p) (Term p)
  | Ref Var
  | Num Integer
  deriving Show

data SC p = SC Var [Var] (Term p)
  deriving Show

