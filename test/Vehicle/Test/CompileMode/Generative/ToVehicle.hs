{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Vehicle.Test.CompileMode.Generative.ToVehicle
  ( convertKind
  , convertType
  , convertTerm
  ) where

import Data.Bifunctor (Bifunctor(..))
import Control.Monad.Reader (Reader(..), ask, runReader, withReader, local)

import Test.CompileMode.Generative.Generator

import Vehicle.Language.AST hiding (Expr)

convertKind :: Kind -> Expr
convertKind = \case
  Star      -> Type0
  k1 :=> k2 -> arrow (convertKind k1) (convertKind k2)

convertType :: Fin ty => Kind -> Type ty -> Expr
convertType k t = runReader (convertType' k t) (const 0)

convertTerm :: (Fin ty, Fin tm) => Type ty -> Term ty tm -> Expr
convertTerm t e = runReader (convertTerm' t e) (const 0, const 0)

type Expr = DBExpr ()

arrow :: Expr -> Expr -> Expr
arrow e1 = Pi () (Binder () Explicit Nothing e1)

expLam :: Expr -> Expr -> Expr
expLam e1 = Lam () (ExplicitBinder () Nothing e1)

impLam :: Expr -> Expr -> Expr
impLam e1 = Lam () (ImplicitBinder () Nothing e1)

expApp :: Expr -> Expr -> Expr
expApp e1 e2 = App () e1 [ExplicitArg () e2]

impApp :: Expr -> Expr -> Expr
impApp e1 e2 = App () e1 [ImplicitArg () e2]

forall :: Expr -> Expr -> Expr
forall e1 = Pi () (ImplicitBinder () Nothing e1)

raiseIndices :: (n -> DBIndex) -> (n -> DBIndex)
raiseIndices = fmap (+1)

extendIndices :: (n -> DBIndex) -> (S n -> DBIndex)
extendIndices = fromS 0 . raiseIndices

convertType' :: Fin ty => Kind -> Type ty -> Reader (ty -> DBIndex) Expr
convertType' Star TyNat =
  return $ BuiltinNumericType () Nat
convertType' Star (a :-> b) =
  arrow <$> convertType' Star a <*> convertType' Star b
convertType' Star (TyForall k a) =
  forall (convertKind k) <$> withReader extendIndices (convertType' Star a)
convertType' k (TyVar i) = do
  typeIndices <- ask
  return $ Var () (Bound (typeIndices i))
convertType' (k :=> k') (TyLam a) =
  expLam (convertKind k) <$> withReader extendIndices (convertType' k' a)
convertType' k' (TyApp a b k) =
  expApp <$> convertType' (k :=> k') a <*> convertType' k' b
convertType' _ _ = error "Malformed kind-type combo"

convertTerm' :: forall ty tm. (Fin ty, Fin tm)
             => Type ty -> Term ty tm -> Reader (ty -> DBIndex, tm -> DBIndex) Expr
convertTerm' a (TmVar n) = do
  (_, termIndices) <- ask
  return $ Var () (Bound (termIndices n))
convertTerm' (a :-> b) (TmLam x) = do
  a' <- withReader fst (convertType' Star a)
  x' <- withReader (bimap raiseIndices extendIndices) (convertTerm' b x)
  return $ expLam a' x'
convertTerm' b (TmApp x y (Normal a)) =
  expApp <$> convertTerm' (a :-> b) x <*> convertTerm' a y
convertTerm' (TyForall k a) (TMLAM x) =
  withReader (bimap extendIndices raiseIndices) $ do
    let k' = convertKind k
    a' <- withReader fst (convertType' k a)
    x' <- convertTerm' a x
    return $ impLam k' x'
convertTerm' _c (TMAPP x (Normal a) (Normal b) k) =
  impApp <$> convertTerm' (TyForall k a) x <*> withReader fst (convertType' k b)
convertTerm' _ _ = error "Malformed type-term combo"