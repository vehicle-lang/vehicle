module Vehicle.Compile.Type.Constraint.Core
  ( runConstraintSolver,
    blockOn,
    malformedConstraintError,
    extractHeadFromInstanceCandidate,
    findInstanceGoalHead,
    parseInstanceGoal,
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
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad (MonadTypeChecker, TCM, copyContext, freshMetaIdAndExpr, solveMeta, trackSolvedMetas)
import Vehicle.Expr.DeBruijn (Ix)
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

blockOn :: (MonadCompile m) => [MetaID] -> m (ConstraintProgress builtin)
blockOn metas = do
  logDebug MaxDetail $ "stuck-on metas" <+> pretty metas
  return $ Stuck $ MetaSet.fromList metas

malformedConstraintError :: (PrintableBuiltin builtin, MonadCompile m) => WithContext (InstanceConstraint builtin) -> m a
malformedConstraintError c =
  compilerDeveloperError $ "Malformed type-class constraint:" <+> prettyVerbose c

unify ::
  (MonadTypeChecker builtin m) =>
  ConstraintContext builtin ->
  Value builtin ->
  Value builtin ->
  m (WithContext (Constraint builtin))
unify ctx e1 e2 = WithContext (UnificationConstraint $ Unify e1 e2) <$> copyContext ctx

{-
unifyWithPiType ::
  TCM builtin m =>
  ConstraintContext builtin ->
  Value builtin ->
  m (WithContext (Constraint builtin), Value builtin, Value builtin)
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
  (TCM builtin m) =>
  ConstraintContext builtin ->
  Value builtin ->
  m (Expr Ix builtin, WithContext (Constraint builtin))
createTC c t = do
  let p = provenanceOf c
  ctx <- copyContext c
  let dbLevel = contextDBLevel c
  newTypeClassExpr <- quote p dbLevel t
  (meta, metaExpr) <- freshMetaIdAndExpr p newTypeClassExpr (boundContext c)
  let newConstraint = InstanceConstraint (Has meta t)
  return (unnormalised metaExpr, WithContext newConstraint ctx)

solveTypeClassMeta :: (TCM builtin m) => ConstraintContext builtin -> MetaID -> Value builtin -> m ()
solveTypeClassMeta ctx meta solution = do
  quotedSolution <- quote mempty (contextDBLevel ctx) solution
  solveMeta meta quotedSolution (boundContext ctx)

extractHeadFromInstanceCandidate ::
  (PrintableBuiltin builtin) =>
  (Provenance -> InstanceCandidate builtin) ->
  (builtin, Provenance -> InstanceCandidate builtin)
extractHeadFromInstanceCandidate candidate = do
  let expr = candidateExpr (candidate mempty)
  case findInstanceGoalHead expr of
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
  App _ (Builtin _ b) _ -> Right b
  expr -> Left expr

parseInstanceGoal ::
  forall m builtin.
  (MonadCompile m) =>
  Value builtin ->
  m (Maybe (InstanceGoal builtin))
parseInstanceGoal = go []
  where
    go :: Telescope Ix builtin -> Value builtin -> m (Maybe (InstanceGoal builtin))
    go telescope = \case
      VPi binder _body
        | not (isExplicit binder) -> compilerDeveloperError "Instance goals with telescopes not yet supported"
      VBuiltin b spine -> return $ Just (InstanceGoal telescope b spine)
      _ -> return Nothing

anyOf :: [a] -> (a -> Bool) -> Bool
anyOf = flip any

allOf :: [a] -> (a -> Bool) -> Bool
allOf = flip all
