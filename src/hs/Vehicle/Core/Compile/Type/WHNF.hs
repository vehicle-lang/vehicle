

module Vehicle.Core.Compile.Type.WHNF where

import Data.IntMap qualified as IntMap

import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Meta

-- TODO: move this to elsewhere, we need to normalise types in the
-- typechecker when checking against them too.
whnf :: MonadMeta m => CheckedExpr -> m CheckedExpr
whnf (App ann fun arg@(Arg _ _ argE)) = do
  whnfFun <- whnf fun
  case whnfFun of
    Lam _ _ body -> whnf (argE `substInto` body)
    _            -> return (App ann whnfFun arg)
whnf (Meta p n) = do
  subst <- getMetaSubstitution
  case IntMap.lookup n subst of
    Nothing -> return (Meta p n)
    Just tm -> whnf tm
-- TODO: expand out declared identifiers once the declCtx stores them
--  whnf (Free nm) = ...
whnf (Let _ bound _ body) = whnf (bound `substInto` body)
whnf (Ann _ body _)       = whnf body
whnf e                    = return e