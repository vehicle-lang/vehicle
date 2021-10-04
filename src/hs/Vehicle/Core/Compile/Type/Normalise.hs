module Vehicle.Core.Compile.Type.Normalise
  ( nf
  , whnf
  ) where

import Control.Monad.Reader (MonadReader(..), asks, runReaderT)
import Data.Map qualified as Map ( lookup )

import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.MetaSubstitution qualified as MetaSubst (lookup)

--------------------------------------------------------------------------------
-- Normalisation

-- This file only deals with App and Lam normalisation required during
-- type-checking. For full normalisation including builtins see the dedicated
-- `Vehicle.Core.Compile.Normalisation` module.

data Strategy
  = WeakHead
  | Strong

whnf :: (MonadMeta m) => DeclCtx -> CheckedExpr -> m CheckedExpr
whnf ctx e = runReaderT (norm e) (WeakHead, ctx)

nf :: (MonadMeta m) => DeclCtx -> CheckedExpr -> m CheckedExpr
nf ctx e = runReaderT (norm e) (Strong, ctx)

norm :: (MonadMeta m, MonadReader (Strategy, DeclCtx) m) => CheckedExpr -> m CheckedExpr
norm (App ann fun arg@(Arg p v argE)) = do
  normFun <- norm fun
  strategy <- asks fst
  case normFun of
    Lam _ _ body -> norm (argE `substInto` body)
    _            -> case strategy of
      WeakHead -> return (App ann normFun arg)
      Strong   -> App ann normFun . Arg p v <$> norm argE
norm (Meta p n) = do
  subst <- getMetaSubstitution
  case MetaSubst.lookup n subst of
    Nothing -> return (Meta p n)
    Just tm -> norm tm
norm e@(Var _ (Free ident)) = do
  ctx <- asks snd
  case Map.lookup ident ctx of
    Just (_, Just res) -> return res
    _                  -> return e
norm (Let _ bound _ body) = norm (bound `substInto` body)
norm (Ann _ body _)       = norm body
norm e                    = return e