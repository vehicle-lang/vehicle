
module Vehicle.Compile.Type.ConstraintSolver.Core
  ( blockOn
  , unify
  , blockOnReductionBlockingMetasOrThrowError
  , malformedConstraintError
  ) where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (mapMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Normalise.NormExpr (NormExpr(..), pattern VTensorType, getMeta)

blockOn :: MonadCompile m => [MetaID] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

unify :: ConstraintContext -> NormExpr -> NormExpr -> WithContext Constraint
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)

getReductionBlockingArgs :: NormExpr -> [NormExpr]
getReductionBlockingArgs = \case
  VTensorType _ _ dims          -> [dims]
  VBuiltin _ TypeClassOp{} args -> [fst (findInstanceArg args)]
  _                             -> []

blockOnReductionBlockingMetasOrThrowError :: MonadCompile m
                                          => [NormExpr]
                                          -> CompileError
                                          -> m ConstraintProgress
blockOnReductionBlockingMetasOrThrowError args err = do
  let blockingArgs = concatMap getReductionBlockingArgs args
  logDebug MaxDetail $ prettyVerbose blockingArgs
  let blockingMetas = mapMaybe getMeta blockingArgs
  if null blockingMetas
    then throwError err
    else blockOn blockingMetas

malformedConstraintError :: MonadCompile m => WithContext TypeClassConstraint -> m a
malformedConstraintError (WithContext c _ctx) = compilerDeveloperError $
  "Malformed type-class constraint:" <+> prettyVerbose c
