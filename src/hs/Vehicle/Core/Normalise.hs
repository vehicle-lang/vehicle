{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Vehicle.Core.Normalise
  ( Norm
  , NormError (..)
  , MonadNorm
  , runNorm
  ) where

import Vehicle.Core.Type ( Tree (..), Expr, Type, Decl, Prog, K )
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..))
import Vehicle.Core.DeBruijn.Substitution as DeBruijn
import Control.Monad.Except (MonadError, Except, runExcept)
import Vehicle.Core.Abs (ExprName, Builtin)
import Control.Monad.Error.Class (throwError)

-- |Errors thrown during normalisation
data NormError = MissingDefFunType ExprName
    | MalformedLambdaError

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- | Run a function in 'MonadNorm'.
runNorm :: Except NormError a -> Either NormError a
runNorm = runExcept

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  norm :: MonadNorm m => vf -> m vf

instance Norm (Expr SortedDeBruijn (K Builtin) ann) where
  norm expr@(ELitInt _ _) = return expr
  norm expr@(ELitReal _ _) = return expr
  norm expr@(ECon _ _) = return expr
  norm expr@(EVar _ _) = return expr

  norm (ELitSeq ann exprs) = ELitSeq ann <$> traverse norm exprs
  norm (EAnn ann expr typ) = EAnn ann <$> norm expr <*> norm typ
  norm (ELam ann arg expr) = ELam ann arg <$> norm expr
  norm (ETyLam ann targ expr) = ETyLam ann targ <$> norm expr
  norm (ETyApp ann expr typ) = ETyApp ann <$> norm expr <*> norm typ

  norm (ELet _ _ letValue letBody) = do
    normalisedLetValue <- norm letValue
    let letBodyWithSubstitution = DeBruijn.subst 0 normalisedLetValue letBody
    norm letBodyWithSubstitution

  norm (EApp _ (ELam _ _ funcBody) arg) = do
    normalisedArg <- norm arg
    let substFuncBody = DeBruijn.subst 0 normalisedArg funcBody
    norm substFuncBody
  norm EApp {} = throwError MalformedLambdaError

instance Norm (Type SortedDeBruijn (K Builtin) ann) where
  norm expr@(TCon _ _) = return expr
  norm expr@(TLitDim _ _) = return expr
  norm expr@(TMeta _ _) = return expr
  norm expr@(TVar _ _) = return expr

  norm (TLitList ann typs) = TLitList ann <$> traverse norm typs
  norm (TForall ann arg body) = TForall ann arg <$> norm body

  norm (TApp _ (TForall _ _ forallBody) arg) = do
    normalisedArg <- norm arg
    let substForallBody = DeBruijn.subst 0 normalisedArg forallBody
    norm substForallBody
  norm TApp {} = throwError MalformedLambdaError

instance Norm (Decl SortedDeBruijn (K Builtin) ann) where
  norm (DeclNetw ann arg typ) = DeclNetw ann arg <$> norm typ
  norm (DeclData ann arg typ) = DeclData ann arg <$> norm typ
  norm (DefType ann arg args typ) = DefType ann arg args <$> norm typ
  norm (DefFun ann arg typ expr) = DefFun ann arg <$> norm typ <*> norm expr

instance Norm (Prog SortedDeBruijn (K Builtin) ann) where
  norm (Main ann decls)= Main ann <$> traverse norm decls