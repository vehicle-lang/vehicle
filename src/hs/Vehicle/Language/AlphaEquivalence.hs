module Vehicle.Language.AlphaEquivalence
  ( alphaEq
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Foldable (Recursive(..))

import Vehicle.Prelude
import Vehicle.Language.AST (Name, HasVisibility(..), HasType(..), ExprF(..), Expr, Binder, Arg, Var, DeBruijnExpr, Literal, Builtin, Visibility, UniverseLevel, argExpr)

alphaEq :: HasProvenance ann => DeBruijnExpr ann -> DeBruijnExpr ann -> Bool
alphaEq e1 e2 = toAlpha e1 == toAlpha e2

data AlphaBinder = Binder Visibility AlphaExpr
  deriving (Eq)

data AlphaArg = Arg Visibility AlphaExpr
  deriving (Eq)

data AlphaExpr
  = Type UniverseLevel
  | Ann AlphaExpr AlphaExpr
  | App AlphaExpr (NonEmpty AlphaArg)
  | Pi AlphaBinder AlphaExpr
  | Builtin Builtin
  | Var Var
  | Let AlphaExpr AlphaBinder AlphaExpr
  | Lam AlphaBinder AlphaExpr
  | Literal Literal
  | Seq [AlphaExpr]
  | PrimDict AlphaExpr
  deriving (Eq)

class ToAlpha a where
  type Alpha a
  toAlpha :: HasProvenance ann => a ann -> Alpha a

instance ToAlpha (Binder Name Var) where
  type Alpha (Binder Name Var) = AlphaBinder
  toAlpha b = Binder (visibilityOf b) (toAlpha (typeOf b))

instance ToAlpha (Arg Name Var) where
  type Alpha (Arg Name Var) = AlphaArg
  toAlpha a = Arg (visibilityOf a) (toAlpha (argExpr a))

instance ToAlpha (Expr Name Var) where
  type Alpha (Expr Name Var) = AlphaExpr
  toAlpha = cata $ \case
    TypeF     l                   -> Type l
    HoleF     p _                 -> incompleteTermError p
    MetaF     ann _               -> incompleteTermError (provenanceOf ann)
    PrimDictF t                   -> PrimDict t
    LiteralF  _ l                 -> Literal l
    BuiltinF  _ op                -> Builtin op
    AnnF      _ e t               -> Ann e t
    AppF      _ fun args          -> App fun (fmap toAlpha args)
    PiF       _ binder result     -> Pi (toAlpha binder) result
    VarF      _ v                 -> Var v
    LetF      _ bound binder body -> Let bound (toAlpha binder) body
    LamF      _ binder body       -> Lam (toAlpha binder) body
    SeqF      _ xs                -> Seq xs

incompleteTermError :: Provenance -> a
incompleteTermError p = developerError $ "alphaEquivalence is not well defined on incomplete terms (at" <+> pretty p <+> ")"

