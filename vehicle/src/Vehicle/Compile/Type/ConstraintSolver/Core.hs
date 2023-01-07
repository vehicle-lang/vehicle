module Vehicle.Compile.Type.ConstraintSolver.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    unify,
  )
where

import Control.Monad (forM_)
import Data.List (partition)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyVerbose, prettyVerbose)
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (TCM)
import Vehicle.Compile.Type.Monad.Class (getAndClearRecentlySolvedMetas)
import Vehicle.Expr.Normalised (NormExpr)

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall m constraint.
  (TCM m, PrettyVerbose (Contextualised constraint ConstraintContext)) =>
  m [Contextualised constraint ConstraintContext] ->
  ([Contextualised constraint ConstraintContext] -> m ()) ->
  (Contextualised constraint ConstraintContext -> m ()) ->
  MetaSet ->
  m MetaSet
runConstraintSolver getConstraints setConstraints attemptToSolveConstraint recentMetas = do
  solvedMetas <- loop 0 recentMetas
  logCompilerPassOutput ("metas-solved:" <+> pretty solvedMetas)
  return solvedMetas
  where
    loop :: Int -> MetaSet -> m MetaSet
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

              forM_ unblockedConstraints $ \constraint -> do
                logCompilerSection MaxDetail ("trying:" <+> prettyVerbose constraint) $
                  attemptToSolveConstraint constraint

              metasSolvedThisLoop <- getAndClearRecentlySolvedMetas
              metasSolvedInFutureLoops <- loop (loopNumber + 1) metasSolvedThisLoop
              return $ metasSolvedThisLoop <> metasSolvedInFutureLoops

blockOn :: MonadCompile m => [MetaID] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: MonadCompile m => WithContext TypeClassConstraint -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

unify :: ConstraintContext -> NormExpr -> NormExpr -> WithContext Constraint
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)
