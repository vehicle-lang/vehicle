module Vehicle.Core.Compile.Type.Normalise
  ( whnf
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Map qualified as Map ( lookup )
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.MetaSubstitution qualified as MetaSubst (lookup)

--------------------------------------------------------------------------------
-- Normalisation

-- This file only deals with App and Lam normalisation required during
-- type-checking. For full normalisation including builtins see the dedicated
-- `Vehicle.Core.Compile.Normalisation` module.

whnf :: (MonadMeta m) => DeclCtx -> CheckedExpr -> m CheckedExpr
whnf ctx e = runReaderT (norm e) ctx

norm :: (MonadMeta m, MonadReader DeclCtx m) => CheckedExpr -> m CheckedExpr
norm e@(App _ fun (Arg p _ argE :| args)) = do
  normFun  <- norm fun
  case normFun of
    Lam _ _ body -> do
      nfBody <- norm (argE `substInto` body)
      return $ normAppList p nfBody args
    _            -> return e
norm (Meta p n) = do
  subst <- getMetaSubstitution
  case MetaSubst.lookup n subst of
    Nothing -> return (Meta p n)
    Just tm -> norm tm
norm e@(Var _ (Free ident)) = do
  ctx <- ask
  case Map.lookup ident ctx of
    Just (_, Just res) -> return res
    _                  -> return e
norm (Let _ bound _ body) = norm (bound `substInto` body)
norm (Ann _ body _)       = norm body
norm e                    = return e