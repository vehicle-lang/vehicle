module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    extractHeadFromInstanceCandidate,
    findInstanceGoalHead,
    parseInstanceGoal,
    unify,
    createInstanceUnification,
    createSubInstance,
    mkCandidate,
  )
where

import Control.Monad (forM_)
import Data.Data (Proxy (..))
import Data.List (partition)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (MonadTypeChecker, TCM, copyContext, freshMetaIdAndExpr, trackSolvedMetas)
import Vehicle.Data.DSL
import Vehicle.Data.Expr.Normalised

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

blockOn :: (MonadCompile m) => [MetaID] -> Maybe (m (ConstraintProgress builtin))
blockOn metas = Just $ do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: (PrintableBuiltin builtin, MonadCompile m) => WithContext (InstanceConstraint builtin) -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

-- | Create a new unification constraint, copying the context as appropriate.
unify ::
  (MonadTypeChecker builtin m) =>
  (ConstraintContext builtin, UnificationConstraintOrigin builtin) ->
  (WHNFValue builtin, WHNFValue builtin) ->
  m (WithContext (UnificationConstraint builtin))
unify (ctx, origin) (e1, e2) =
  WithContext (Unify origin e1 e2) <$> copyContext ctx

-- | Create a new unification constraint as a subgoal of an existing instance constraint.
createInstanceUnification ::
  (MonadTypeChecker builtin m) =>
  (ConstraintContext builtin, InstanceConstraintOrigin builtin) ->
  WHNFValue builtin ->
  WHNFValue builtin ->
  m (WithContext (Constraint builtin))
createInstanceUnification (ctx, origin) e1 e2 = do
  let unifyOrigin = CheckingInstanceType origin
  constraint <- unify (ctx, unifyOrigin) (e1, e2)
  return $ mapObject UnificationConstraint constraint

-- | Creates an instance constraint as a subgoal of an existing instance constraint.
createSubInstance ::
  (TCM builtin m) =>
  (ConstraintContext builtin, InstanceConstraintOrigin builtin) ->
  Relevance ->
  WHNFValue builtin ->
  m (Expr Ix builtin, WithContext (Constraint builtin))
createSubInstance (ctx, origin) r t = do
  let p = provenanceOf ctx
  newCtx <- copyContext ctx
  let dbLevel = contextDBLevel ctx
  let newTypeClassExpr = quote p dbLevel t
  (meta, metaExpr) <- freshMetaIdAndExpr p newTypeClassExpr (boundContext ctx)
  let newConstraint = InstanceConstraint (Resolve origin meta r t)
  return (unnormalised metaExpr, WithContext newConstraint newCtx)

extractHeadFromInstanceCandidate ::
  (PrintableBuiltin builtin) =>
  InstanceCandidate builtin ->
  (builtin, InstanceCandidate builtin)
extractHeadFromInstanceCandidate candidate@InstanceCandidate {..} = do
  case findInstanceGoalHead candidateExpr of
    Right b -> (b, candidate)
    Left subexpr -> do
      let candidateDoc = prettyVerbose subexpr
      let problemDoc = prettyVerbose subexpr
      developerError $
        "Invalid builtin instance candidate:"
          <+> candidateDoc
          <> line
          <> "Problematic subexpr:"
            <+> problemDoc

findInstanceGoalHead :: Expr Ix builtin -> Either (Expr Ix builtin) builtin
findInstanceGoalHead = \case
  Pi _ binder body
    | not (isExplicit binder) -> findInstanceGoalHead body
  App (Builtin _ b) _ -> Right b
  Builtin _ b -> Right b
  expr -> Left expr

parseInstanceGoal ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  WHNFValue builtin ->
  m (InstanceGoal builtin)
parseInstanceGoal e = go [] e
  where
    go :: Telescope Ix builtin -> WHNFValue builtin -> m (InstanceGoal builtin)
    go telescope = \case
      VPi binder _body
        | not (isExplicit binder) -> compilerDeveloperError "Instance goals with telescopes not yet supported"
      VBuiltin b spine -> return $ InstanceGoal telescope b spine
      _ -> compilerDeveloperError $ "Malformed instance goal" <+> prettyVerbose e

mkCandidate :: (DSLExpr builtin, DSLExpr builtin) -> InstanceCandidate builtin
mkCandidate (expr, solution) = do
  let p = mempty
  let expr' = fromDSL p expr
  let solution' = fromDSL p solution
  InstanceCandidate expr' solution'
