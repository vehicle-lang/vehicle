
module Vehicle.Compile.Type.ConstraintSolver.Core
  ( blockOn
  , unify
  , getMeta
  , blockOnReductionBlockingMetasOrThrowError
  , malformedConstraintError
  ) where

import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Language.Print (prettyVerbose)

blockOn :: MonadCompile m => [MetaID] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

unify :: ConstraintContext -> CheckedExpr -> CheckedExpr -> WithContext Constraint
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)

getMeta :: CheckedExpr -> Maybe MetaID
getMeta e = case exprHead e of
  Meta _ m -> Just m
  _        -> Nothing

getReductionBlockingArgs :: CheckedExpr -> [CheckedExpr]
getReductionBlockingArgs = \case
  TensorType _ _ dims              -> [dims]
  BuiltinExpr _ TypeClassOp{} args -> [ fst (findInstanceArg (NonEmpty.toList args)) ]
  _                                -> []

blockOnReductionBlockingMetasOrThrowError :: MonadCompile m
                                          => [CheckedExpr]
                                          -> CompileError
                                          -> m ConstraintProgress
blockOnReductionBlockingMetasOrThrowError args err = do
  let blockingArgs = concatMap getReductionBlockingArgs args
  let blockingMetas = mapMaybe getMeta blockingArgs
  if null blockingMetas
    then throwError err
    else blockOn blockingMetas

malformedConstraintError :: MonadCompile m => WithContext TypeClassConstraint -> m a
malformedConstraintError (WithContext c _ctx) = compilerDeveloperError $
  "Malformed type-class constraint:" <+> prettyVerbose c
