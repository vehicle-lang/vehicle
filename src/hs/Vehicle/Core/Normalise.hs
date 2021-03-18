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

import Vehicle.Core.Type ( Sort (..), Expr (..), Sort, EArg (..) )
import Vehicle.Core.Instance.Recursive (ExprF (..))
import Control.Monad.Except (MonadError, Except, runExcept)
import Data.Map (Map, lookup, insert, notMember)
import Data.Functor.Foldable (fold)
import Vehicle.Core.Abs (ExprName, SortedBuiltin)
import Control.Monad.Error.Class (throwError)

-- * Normalisation monad

type DeBruijn = Int

newtype SortedDeBruijn (sort :: Sort) 
  = SortedDeBruijn DeBruijn

type NormExpr ann = Expr SortedDeBruijn SortedBuiltin ann

-- |Errors thrown during normalisation
data NormError = MissingDefFunType ExprName
    | UnboundVariableError DeBruijn
    | DuplicateVariableError DeBruijn
    | MalformedLambdaError

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- | Run a function in 'MonadNorm'.
runNorm :: Except NormError a -> Either NormError a
runNorm = runExcept

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  norm :: MonadNorm m => Map DeBruijn vf -> vf -> m vf

instance Norm (NormExpr ann) where
  norm ctxt = fold $ \case
    EVarF ann (SortedDeBruijn i) -> normVar ctxt ann i
    ELetF _ arg exp1 exp2 -> normLet ctxt (index arg) exp1 exp2
    EAppF _ exp1 exp2 -> normApp ctxt exp1 exp2

    ELitSeqF ann exprs -> ELitSeq ann <$> sequence exprs
    EAnnF ann expr typ -> EAnn ann <$> expr <*> pure typ
    ELamF ann arg expr -> ELam ann arg <$> expr
    ETyLamF ann targ expr -> ETyLam ann targ <$> expr
    ETyAppF ann expr typ -> ETyApp ann <$> expr <*> pure typ    

    ELitIntF ann val -> return $ ELitInt ann val
    ELitRealF ann val -> return $ ELitReal ann val
    EConF ann builtin -> return $ ECon ann builtin

normApp :: (MonadNorm m) => Map DeBruijn (NormExpr ann) -> 
            m (NormExpr ann) -> m (NormExpr ann) -> m (NormExpr ann) 
normApp ctxt mFn mArg = do
    fn <- mFn
    arg <- mArg
    varName <- lamVarName fn
    varBody <- lamBody fn
    updatedCtxt <- updateCtxt ctxt varName arg
    norm updatedCtxt varBody 

normLet :: (MonadNorm m) => Map DeBruijn (NormExpr ann) -> 
            DeBruijn -> m (NormExpr ann) -> m (NormExpr ann) -> m (NormExpr ann)
normLet ctxt name exp1 exp2 = do
  nexp1 <- exp1
  nexp2 <- exp2
  updatedCtxt <- updateCtxt ctxt name nexp1
  norm updatedCtxt nexp2 

normVar :: (MonadNorm m) => Map DeBruijn (NormExpr ann) ->
            ann 'EXPR (SortedDeBruijn 'EXPR) (SortedBuiltin 'EXPR) -> DeBruijn -> 
            m (NormExpr ann)
normVar ctxt ann i = case Data.Map.lookup i ctxt of 
  { Nothing -> return $ EVar ann (SortedDeBruijn i)
  ; Just expr -> return expr
  }

-- * Helper functions

index :: EArg SortedDeBruijn builtin ann -> DeBruijn
index (EArg _ (SortedDeBruijn i)) = i

lamVarName :: (MonadNorm m) => NormExpr ann -> m DeBruijn
lamVarName (ELam _ arg _) = return $ index arg
lamVarName _ = throwError $ MalformedLambdaError

lamBody :: (MonadNorm m) => NormExpr ann -> m (NormExpr ann)
lamBody (ELam _ _ e) = return e
lamBody _ = throwError MalformedLambdaError

updateCtxt :: (MonadNorm m) => Map DeBruijn (NormExpr ann) -> 
              DeBruijn -> NormExpr ann -> m (Map DeBruijn (NormExpr ann))
updateCtxt ctxt i expr = if notMember i ctxt
  then return $ insert i expr ctxt
  else throwError $ DuplicateVariableError i