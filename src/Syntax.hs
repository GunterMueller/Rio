{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Syntax
  ( Exp(..)
  , Span(..)
  , Type(..)
  , Var(..)
  , Delta(..)
  , Typing(..)
  , Gamma(..)
  , Spanned(..)
  , Recursive(..)
  , typedAs, (!), (\-), insertGamma
  , expAnn, withTypeAnn
  , free, substitute
  , isTyCon
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Map (Map)
import Data.Set (Set)

import Data.List
import Data.Char

import GHC.Exts

import Text.Parsec.Pos
import Text.Printf

data Span = Span SourcePos SourcePos
  deriving (Eq, Ord)

instance Show Span where
  show (Span a b) = printf "%s:%d:%d-%d:%d"
    (sourceName a) (sourceLine a) (sourceColumn a) (sourceLine b) (sourceColumn b)

instance Semigroup Span where
  Span a _ <> Span _ b = Span a b

newtype Var = Var { varName :: String }
  deriving (Eq, Ord)
  deriving newtype IsString

instance Show Var where
  show (Var v) = v

data Recursive = Rec | NonRec
  deriving (Eq, Show, Ord)

data Exp
  = Lam Span Var Exp
  | App Span Exp Exp
  | Use Span Var
  | Let Span Recursive (Var, Exp) Exp
  | Num Span Integer
  deriving (Eq, Ord)

expAnn :: Exp -> Span
expAnn (Lam a _ _) = a
expAnn (App a _ _) = a
expAnn (Use a _) = a
expAnn (Let a _ _ _) = a
expAnn (Num a _) = a

instance Show Exp where
  show (Lam _ v e) = "fun " ++ show v ++ " -> " ++ show e
  show (App _ l r) = showFun l ++ " " ++ showArg r where
    showFun x@Lam{} = paren (show x)
    showFun x@Let{} = paren (show x)
    showFun x = show x

    showArg x@App{} = paren (show x)
    showArg x = showFun x
  show (Use _ v) = show v
  show (Num _ x) = show x
  show (Let _ rc (a, b) x) =
    "let " ++ rec ++ show a ++ " = " ++ show b ++ " in " ++ show x
      where rec | rc == Rec = "rec" | otherwise = ""

paren :: String -> String
paren x = "(" ++ x ++ ")"

newtype Delta = Delta { getDelta :: Map Var (Spanned Type) }
  deriving (Eq, Ord, Semigroup, Monoid)

data Spanned a = Spanned Span a

instance Show a => Show (Spanned a) where
  show (Spanned _ s) = show s

instance Eq a => Eq (Spanned a) where
  Spanned _ l == Spanned _ r = l == r

instance Ord a => Ord (Spanned a) where
  Spanned _ l `compare` Spanned _ r = l `compare` r

instance Show Delta where
  show (Delta xs)
    | Map.null xs = "{}"
    | otherwise
    = "{ "
   ++ intercalate ", "
        (map (\(a, b) -> show a ++ " :: " ++ show b) (Map.toList xs))
   ++ " }"

newtype Gamma = Gamma { getGamma :: Map Var Typing }
  deriving (Eq, Show, Ord, Semigroup, Monoid)

data Type
  = TyVar Var
  | Type :-> Type
  | TyCon Var
  deriving (Eq, Ord)

instance IsString Type where
  fromString [] = error "empty type"
  fromString xs@(x:_)
    | isUpper x = TyCon (fromString xs)
    | otherwise = TyVar (fromString xs)

infixr 1 :->

instance Show Type where
  show (TyVar (Var v)) = v
  show (TyCon (Var v)) = "\x1b[1;34m" ++ v ++ "\x1b[0m"
  show (l :-> t) = l' ++ " \x1b[1;35m->\x1b[0m " ++ show t where
    l' = case l of
      (:->){} -> paren (show l)
      _ -> show l

data Typing
  = Typing { typingAnn :: Maybe Span
           , typingEnv :: Delta
           , typingTy  :: Type }
  deriving (Eq, Ord)

instance Show Typing where
  show (Typing _ d t) = show d ++ " ??? " ++ show t

free :: Exp -> Set Var
free (Use _ v) = Set.singleton v
free (Let _ Rec (v, e) b) = Set.delete v (free e <> free b)
free (Let _ NonRec (v, e) b) = Set.delete v (free b) <> free e
free (Lam _ v b) = Set.delete v (free b)
free (App _ f x) = free f <> free x
free (Num _ _) = mempty

substitute :: Map Var Exp -> Exp -> Exp
substitute mp ex@(Use _ v)
  | Just x <- Map.lookup v mp = x
  | otherwise = ex

substitute mp (Let an rec (v, e) b) =
  Let an rec (v, substitute mp e) (substitute (Map.delete v mp) b)

substitute mp (Lam an v b) =
  Lam an v (substitute (Map.delete v mp) b)

substitute mp (App an f x) = App an (substitute mp f) (substitute mp x)
substitute _ x@(Num _ _) = x

withTypeAnn :: Span -> Typing -> Typing
withTypeAnn a t = t { typingAnn = Just a }

typedAs :: Var -> Span -> Type -> Delta
typedAs v s t = Delta (Map.singleton v (Spanned s t))

(!) :: Delta -> Var -> Maybe (Spanned Type)
Delta m ! v = Map.lookup v m

(\-) :: Delta -> Var -> Delta
Delta m \- v = Delta $ Map.delete v m

insertGamma :: Var -> Typing -> Gamma -> Gamma
insertGamma v t (Gamma g) = Gamma (Map.insert v t g)

isTyCon :: Type -> Bool
isTyCon TyCon{} = True
isTyCon _ = False
