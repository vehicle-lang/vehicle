module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    unify,
    solveTypeClassMeta,
    anyOf,
    allOf,
    createTC,
  )
where

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (MonadTypeChecker, TCM, copyContext, freshMetaIdAndExpr, solveMeta, trackSolvedMetas)
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..), NormalisableExpr)
import Vehicle.Expr.Normalised

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall types m constraint.
  (TCM types m, PrettyExternal (Contextualised constraint (ConstraintContext types))) =>
  m [Contextualised constraint (ConstraintContext types)] ->
  ([Contextualised constraint (ConstraintContext types)] -> m ()) ->
  (Contextualised constraint (ConstraintContext types) -> m ()) ->
  MetaSet ->
  m ()
runConstraintSolver getConstraints setConstraints attemptToSolveConstraint = loop 0
  where
    loop :: Int -> MetaSet -> m ()
    loop loopNumber recentMetasSolved = do
      unsolvedConstraints <- getConstraints

      if null unsolvedConstraints
        then return mempty
        else do
          let isUnblocked = not . constraintIsBlocked recentMetasSolved
          let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints

          if null unblockedConstraints
            then return mempty
            else do
              -- We have made useful progress so start a new pass
              setConstraints blockedConstraints

              solvedMetas <- trackSolvedMetas (Proxy @types) $ do
                forM_ unblockedConstraints $ \constraint -> do
                  logCompilerSection MaxDetail ("trying:" <+> prettyExternal constraint) $ do
                    attemptToSolveConstraint constraint

              loop (loopNumber + 1) solvedMetas

blockOn :: (MonadCompile m) => [MetaID] -> m (ConstraintProgress builtin)
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: (PrintableBuiltin types, MonadCompile m) => WithContext (TypeClassConstraint types) -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

unify ::
  (MonadTypeChecker types m) =>
  ConstraintContext types ->
  Value types ->
  Value types ->
  m (WithContext (Constraint types))
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) <$> copyContext ctx

{-
unifyWithPiType ::
  TCM types m =>
  ConstraintContext types ->
  Value types ->
  m (WithContext (Constraint types), Value types, Value types)
unifyWithPiType ctx expr = do
  let p = provenanceOf ctx
  let boundCtx = boundContext ctx
  binderType <- normalised <$> freshMetaExpr p (TypeUniverse p 0) boundCtx
  bodyType <- normalised <$> freshMetaExpr p (TypeUniverse p 0) boundCtx
  let binder = _
  eq <- unify ctx expr _
  return (eq, binderType, bodyType)
-}

createTC ::
  (TCM types m) =>
  ConstraintContext types ->
  types ->
  NonEmpty (VType types) ->
  m (NormalisableExpr types, WithContext (Constraint types))
createTC c tc argExprs = do
  let p = provenanceOf c
  ctx <- copyContext c
  let dbLevel = contextDBLevel c
  let nArgs = ExplicitArg p <$> argExprs
  newTypeClassExpr <- BuiltinExpr p (CType tc) <$> traverse (quote mempty dbLevel) nArgs
  (meta, metaExpr) <- freshMetaIdAndExpr p newTypeClassExpr (boundContext c)
  let newConstraint = TypeClassConstraint (Has meta tc (NonEmpty.toList argExprs))
  return (unnormalised metaExpr, WithContext newConstraint ctx)

solveTypeClassMeta :: (TCM types m) => ConstraintContext types -> MetaID -> Value types -> m ()
solveTypeClassMeta ctx meta solution = do
  quotedSolution <- quote mempty (contextDBLevel ctx) solution
  solveMeta meta quotedSolution (boundContext ctx)

anyOf :: [a] -> (a -> Bool) -> Bool
anyOf = flip any

allOf :: [a] -> (a -> Bool) -> Bool
allOf = flip all
