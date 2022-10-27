
module Vehicle.Compile.Type.ConstraintSolver.Core where

import Control.Monad (MonadPlus (..))
import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.MetaSet qualified as MetaSet
import Vehicle.Language.Print (prettyVerbose)

malformedConstraintError :: MonadCompile m => Constraint -> m a
malformedConstraintError c = compilerDeveloperError $
  "Malformed constraint:" <+> prettyVerbose c

anyOf :: [a] -> (a -> Bool) ->  Bool
anyOf = flip any

allOf :: [a] -> (a -> Bool) ->  Bool
allOf = flip all

unless2 :: MonadPlus m => Bool -> a -> m a
unless2 p a = if not p then return a else mzero

tcArgError :: Constraint
           -> CheckedType
           -> TypeClassOp
           -> [Builtin]
           -> Int
           -> Int
           -> [CompileError]
tcArgError c arg op allowedTypes argIndex numberOfArgs = unless2 (isMeta arg)
    (FailedBuiltinConstraintArgument (constraintContext c) (TypeClassOp op) arg allowedTypes argIndex numberOfArgs)

tcResultError :: Constraint
              -> CheckedType
              -> TypeClassOp
              -> [Builtin]
              -> [CompileError]
tcResultError c result op allowedTypes =
  unless2 (isMeta result)
    (FailedBuiltinConstraintResult (constraintContext c) (TypeClassOp op) result allowedTypes)

positionalIntersection :: Eq a => [a] -> [a] -> [a]
positionalIntersection [] _       = []
positionalIntersection _ []       = []
positionalIntersection (x : xs) (y : ys)
 | x == y    = x : positionalIntersection xs ys
 | otherwise = positionalIntersection xs ys

blockOn :: MonadCompile m => [Meta] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

unify :: Constraint -> CheckedExpr -> CheckedExpr -> Constraint
unify c e1 e2 = UC (copyContext (constraintContext c)) $ Unify (e1, e2)

solved :: ConstraintProgress
solved = Progress mempty

getMeta :: CheckedExpr -> Maybe Meta
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
