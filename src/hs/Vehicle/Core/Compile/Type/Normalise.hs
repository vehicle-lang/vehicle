module Vehicle.Core.Compile.Type.Normalise
  ( nf
  , whnf
  , normaliseTypeClassConstraints
  ) where

import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..), runReaderT)

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
  = Weak
  | Strong

whnf :: (MonadMeta m) => CheckedExpr -> m CheckedExpr
whnf e = runReaderT (norm e) Weak

nf :: (MonadMeta m) => CheckedExpr -> m CheckedExpr
nf e = runReaderT (norm e) Strong

-- TODO: move this to elsewhere, we need to normalise types in the
-- typechecker when checking against them too.
norm :: (MonadMeta m, MonadReader Strategy m) => CheckedExpr -> m CheckedExpr
norm (App ann fun arg@(Arg p v argE)) = do
  normFun <- norm fun
  strategy <- ask
  case normFun of
    Lam _ _ body -> norm (argE `substInto` body)
    _            -> case strategy of
      Weak   -> return (App ann normFun arg)
      Strong -> App ann normFun . Arg p v <$> norm argE
norm (Meta p n) = do
  subst <- getMetaSubstitution
  case MetaSubst.lookup n subst of
    Nothing -> return (Meta p n)
    Just tm -> norm tm
-- TODO: expand out declared identifiers once the declCtx stores them
--  norm (Free nm) = ...
norm (Let _ bound _ body) = norm (bound `substInto` body)
norm (Ann _ body _)       = norm body
norm e                    = return e

normaliseTypeClassConstraints :: MonadMeta m => m ()
normaliseTypeClassConstraints = do
  MetaCtx{..} <- get
  normalisedConstraints <- traverse (\(m `Has` e) -> (m `Has`) <$> nf e) typeClassConstraints
  put $ MetaCtx{ typeClassConstraints = normalisedConstraints, ..}