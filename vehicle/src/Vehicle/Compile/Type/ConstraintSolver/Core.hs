
module Vehicle.Compile.Type.ConstraintSolver.Core
  ( blockOn
  , unify
  , blockOnReductionBlockingMetasOrThrowError
  , malformedConstraintError
  ) where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (maybeToList)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Expr.Normalised (NormExpr (..), getMeta, isLiteral,
                                pattern VTensorType)

blockOn :: MonadCompile m => [MetaID] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

unify :: ConstraintContext -> NormExpr -> NormExpr -> WithContext Constraint
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)

-- This is really buggy at the moment. Tensor should not be here as its derived,
-- and all the other computational builtins should be.
getReductionBlockingMetas :: forall m . MonadLogger m => NormExpr -> m [MetaID]
getReductionBlockingMetas = \case
  VTensorType _ _ dims          -> do logDebug MaxDetail "Hit"; go dims
  VBuiltin _ TypeClassOp{} args -> do logDebug MaxDetail (prettyVerbose args); return $ maybeToList (getMeta (fst (findInstanceArg args)))
  _                             -> return []
  where
    go  :: NormExpr -> m [MetaID]
    go = \case
      VMeta _ m _ -> return [m]
      e | isLiteral e -> return []
        | otherwise   -> getReductionBlockingMetas e

blockOnReductionBlockingMetasOrThrowError :: MonadCompile m
                                          => [NormExpr]
                                          -> CompileError
                                          -> m ConstraintProgress
blockOnReductionBlockingMetasOrThrowError args err = do
  blockingMetas <- concat <$> traverse getReductionBlockingMetas args
  logDebug MaxDetail (pretty blockingMetas)
  if null blockingMetas
    then throwError err
    else blockOn blockingMetas

malformedConstraintError :: MonadCompile m => WithContext TypeClassConstraint -> m a
malformedConstraintError (WithContext c _ctx) = compilerDeveloperError $
  "Malformed type-class constraint:" <+> prettyVerbose c
