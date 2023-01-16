module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    unify,
    solveTypeClassMeta,
    TypeClassProgress,
    InstanceSolver,
    TypeClassSolver,
    AuxiliaryTypeClassSolver,
    mkVAnnBoolType,
    mkVAnnRatType,
    mkVIndexType,
    mkVListType,
    mkVVecType,
  )
where

import Control.Monad (forM_)
import Data.List (partition)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (TCM, solveMeta)
import Vehicle.Compile.Type.Monad.Class (trackSolvedMetas)
import Vehicle.Expr.Normalised (NormExpr, NormType, Spine, pattern VConstructor)

-- | Function signature for constraints solved by instance search.
type InstanceSolver =
  forall m.
  TCM m =>
  ConstraintContext ->
  MetaID ->
  Spine ->
  m ()

type TypeClassProgress = Either MetaSet ([WithContext Constraint], NormExpr)

-- | Function signature for constraints solved by type class resolution.
-- This should eventually be refactored out so all are solved by instance
-- search.
type TypeClassSolver =
  forall m.
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m TypeClassProgress

-- | Function signature for auxiliary constraints solved by type class resolution.
type AuxiliaryTypeClassSolver =
  forall m.
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall m constraint.
  (TCM m, PrettyExternal (Contextualised constraint ConstraintContext)) =>
  m [Contextualised constraint ConstraintContext] ->
  ([Contextualised constraint ConstraintContext] -> m ()) ->
  (Contextualised constraint ConstraintContext -> m ()) ->
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

              solvedMetas <- trackSolvedMetas $ do
                forM_ unblockedConstraints $ \constraint -> do
                  logCompilerSection MaxDetail ("trying:" <+> prettyExternal constraint) $ do
                    attemptToSolveConstraint constraint

              loop (loopNumber + 1) solvedMetas

blockOn :: MonadCompile m => [MetaID] -> m ConstraintProgress
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: MonadCompile m => WithContext TypeClassConstraint -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

unify :: ConstraintContext -> NormExpr -> NormExpr -> WithContext Constraint
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) (copyContext ctx)

solveTypeClassMeta :: TCM m => ConstraintContext -> MetaID -> NormExpr -> m ()
solveTypeClassMeta ctx meta solution = do
  quotedSolution <- quote (contextDBLevel ctx) solution
  solveMeta meta quotedSolution (boundContext ctx)

mkVAnnBoolType :: Provenance -> NormExpr -> NormExpr -> NormExpr
mkVAnnBoolType p lin pol = VConstructor p Bool [IrrelevantImplicitArg p lin, IrrelevantImplicitArg p pol]

mkVAnnRatType :: Provenance -> NormExpr -> NormExpr
mkVAnnRatType p lin = VConstructor p Rat [IrrelevantImplicitArg p lin]

mkVIndexType :: Provenance -> NormExpr -> NormExpr
mkVIndexType p size = VConstructor p Index [ExplicitArg p size]

mkVListType :: Provenance -> NormType -> NormExpr
mkVListType p tElem = VConstructor p List [ExplicitArg p tElem]

mkVVecType :: Provenance -> NormType -> NormExpr -> NormExpr
mkVVecType p tElem dim = VConstructor p Vector [ExplicitArg p tElem, ExplicitArg p dim]
