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

import Vehicle.Core.Type ( Tree (..), Expr, K )
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..))
import Vehicle.Core.DeBruijn.Substitution as DeBruijn
import Control.Monad.Except (MonadError, Except, runExcept)
import Vehicle.Core.Abs (ExprName, Builtin)
import Control.Monad.Error.Class (throwError)

-- * Normalisation monad

type NormExpr ann = Expr SortedDeBruijn (K Builtin) ann

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

instance Norm (NormExpr ann) where
  norm expr@(ELitInt _ _) = return expr
  norm expr@(ELitReal _ _) = return expr
  norm expr@(ECon _ _) = return expr
  norm expr@(EVar _ _) = return expr

  norm (ELitSeq ann exprs) = ELitSeq ann <$> traverse norm exprs
  norm (EAnn ann expr typ) = EAnn ann <$> norm expr <*> pure typ
  norm (ELam ann arg expr) = ELam ann arg <$> norm expr
  norm (ETyLam ann targ expr) = ETyLam ann targ <$> norm expr
  norm (ETyApp ann expr typ) = ETyApp ann <$> norm expr <*> pure typ

  norm (ELet _ _ letValue letBody) = do
    normalisedLetValue <- norm letValue
    let letBodyWithSubstitution = DeBruijn.subst 0 normalisedLetValue letBody
    norm letBodyWithSubstitution
  
  norm (EApp _ func arg) = do
    funcBody <- lamBody func
    normalisedArg <- norm arg
    let substFuncBody = DeBruijn.subst 0 normalisedArg funcBody
    norm substFuncBody

-- * Helper functions

lamBody :: (MonadNorm m) => NormExpr ann -> m (NormExpr ann)
lamBody (ELam _ _ e) = return e
lamBody _ = throwError MalformedLambdaError