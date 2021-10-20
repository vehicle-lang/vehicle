module Vehicle.Language.Compile.Type.Normalise
  ( whnf
  ) where

import Control.Monad.Reader (MonadReader(..), runReaderT)
import Data.Map qualified as Map ( lookup )
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Language.AST
import Vehicle.Language.Compile.Type.Core
import Vehicle.Language.Compile.Type.Meta

--------------------------------------------------------------------------------
-- Normalisation

-- This file only deals with App and Lam normalisation required during
-- type-checking. For full normalisation including builtins see the dedicated
-- `Vehicle.Language.Compile.Normalisation` module.

whnf :: (MonadMeta m) => DeclCtx -> CheckedExpr -> m CheckedExpr
whnf ctx e = do
  subst <- getMetaSubstitution
  let e' = substMetas subst e
  runReaderT (norm e') ctx

norm :: (MonadMeta m, MonadReader DeclCtx m) => CheckedExpr -> m CheckedExpr
norm e@(App ann fun (Arg _ argE :| args)) = do
  normFun  <- norm fun
  case normFun of
    Lam _ _ body -> do
      nfBody <- norm (argE `substInto` body)
      return $ normAppList ann nfBody args
    _            -> return e
norm e@(Var _ (Free ident)) = do
  ctx <- ask
  case Map.lookup ident ctx of
    Just (_, Just res) -> return res
    _                  -> return e
norm (Let _ bound _ body) = norm (bound `substInto` body)
norm (Ann _ body _)       = norm body
norm e                    = return e