module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    unify,
    solveTypeClassMeta,
  )
where

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.List (partition)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (TCM, solveMeta, trackSolvedMetas)
import Vehicle.Expr.Normalised

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall builtin m constraint.
  (TCM builtin m, PrettyExternal (Contextualised constraint (ConstraintContext builtin))) =>
  m [Contextualised constraint (ConstraintContext builtin)] ->
  ([Contextualised constraint (ConstraintContext builtin)] -> m ()) ->
  (Contextualised constraint (ConstraintContext builtin) -> m ()) ->
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

              solvedMetas <- trackSolvedMetas (Proxy @builtin) $ do
                forM_ unblockedConstraints $ \constraint -> do
                  logCompilerSection MaxDetail ("trying:" <+> prettyExternal constraint) $ do
                    attemptToSolveConstraint constraint

              loop (loopNumber + 1) solvedMetas

blockOn :: MonadCompile m => [MetaID] -> m (ConstraintProgress builtin)
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: (PrintableBuiltin builtin, MonadCompile m) => WithContext (TypeClassConstraint builtin) -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

unify :: ConstraintContext builtin -> NormExpr builtin -> NormExpr builtin -> WithContext (Constraint builtin)
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)

solveTypeClassMeta :: TCM builtin m => ConstraintContext builtin -> MetaID -> NormExpr builtin -> m ()
solveTypeClassMeta ctx meta solution = do
  quotedSolution <- quote mempty (contextDBLevel ctx) solution
  solveMeta meta quotedSolution (boundContext ctx)
