module Vehicle.Compile.Type.Monad
  ( MonadTypeChecker (..),
    TypeCheckerState,
    -- Top-level interface
    runTypeCheckerTInitially,
    runTypeCheckerTHypothetically,
    adoptHypotheticalState,
    -- Meta variables
    freshMetaExpr,
    freshSolutionMeta,
    getMetaType,
    getMetaCtx,
    getMetaProvenance,
    getUnsolvedMetas,
    solveMeta,
    getMetasLinkedToMetasIn,
    trackSolvedMetas,
    prettyMeta,
    substMetaVariables,
    -- Constraints
    runConstraintSolver,
    copyContext,
    createFreshInstanceConstraint,
    createFreshApplicationConstraint,
    createDerivedInstanceConstraint,
    getActiveConstraints,
    getActiveUnificationConstraints,
    getActiveInstanceConstraints,
    setInstanceConstraints,
    setUnificationConstraints,
    addUnificationConstraints,
    -- Other
    clearMetaCtx,
    logUnsolvedUnknowns,
    findFirstConstraint,
    checkAllConstraintsSolved,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error (CompileError (..), TypingError (..), compilerDeveloperError)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyExternal, prettyExternal, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Variable (MetaInfo (..), addMetaSolution)
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Data.Builtin.Interface.Print (PrintableBuiltin)
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Code.Value

runTypeCheckerTInitially ::
  (Monad m) =>
  FreeCtx builtin ->
  InstanceDatabase builtin ->
  TypeCheckerT builtin m a ->
  m a
runTypeCheckerTInitially freeCtx instanceCandidates e =
  fst <$> runTypeCheckerT freeCtx instanceCandidates emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerTHypothetically ::
  forall builtin m a.
  (MonadTypeChecker builtin m) =>
  TypeCheckerT builtin (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState builtin))
runTypeCheckerTHypothetically e = do
  callDepth <- getCallDepth
  freeCtx <- getFreeCtx (Proxy @builtin)
  instanceCandidates <- getInstanceCandidates
  state <- getTypeCheckerState
  result <- runExceptT $ runTypeCheckerT freeCtx instanceCandidates state e
  case result of
    Right value -> return $ Right value
    Left err -> case err of
      DevError {} -> throwError err
      _ -> do
        -- If we errored then reset the call depth so logging is not disrupted.
        setCallDepth callDepth
        return $ Left err

-- | Accepts the hypothetical outcome of the type-checker.
adoptHypotheticalState :: (MonadTypeChecker builtin m) => TypeCheckerState builtin -> m ()
adoptHypotheticalState = modifyTypeCheckerState . const

freshMetaExpr ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  BoundCtx (Type builtin) ->
  m (Expr builtin)
freshMetaExpr p t boundCtx = do
  let ctx = if useDependentMetas (Proxy @builtin) then boundCtx else mempty
  snd <$> freshMeta p t ctx

freshSolutionMeta ::
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  BoundCtx (Type builtin) ->
  m (MetaID, Expr builtin)
freshSolutionMeta = freshMeta

createFreshApplicationConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  BoundCtx (Type builtin) ->
  ArgInsertionProblem builtin ->
  MetaSet ->
  m (Expr builtin, Type builtin)
createFreshApplicationConstraint ctx problem blockingMetas = do
  let p = provenanceOf $ originalFun problem
  (finalTypeID, finalType) <- freshSolutionMeta p (TypeUniverse p 0) ctx
  (finalExprID, finalExpr) <- freshSolutionMeta p finalType ctx

  let constraint =
        InferArgs
          { exprSolution = finalExprID,
            typeSolution = finalTypeID,
            argInsertionProblem = problem
          }

  context <- createFreshConstraintCtx p ctx
  let blockedConstraint = WithContext constraint (blockCtxOn blockingMetas context)
  addApplicationConstraint blockedConstraint
  return (finalExpr, finalType)

-- | Adds an entirely new instance constraint (as opposed to one
-- derived from another constraint).
createFreshInstanceConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Bool ->
  BoundCtx (Type builtin) ->
  Provenance ->
  InstanceConstraintOrigin builtin ->
  Relevance ->
  Type builtin ->
  m (Expr builtin)
createFreshInstanceConstraint auxiliaryConstraint boundCtx p origin relevance tcExpr = do
  let env = boundContextToEnv boundCtx
  (metaID, metaExpr) <- freshSolutionMeta p tcExpr boundCtx

  context <- createFreshConstraintCtx p boundCtx
  nTCExpr <- normaliseInEnv (toNamedBoundCtx boundCtx) env tcExpr
  let goal = parseInstanceGoal nTCExpr
  let constraint = WithContext (Resolve origin metaID relevance goal) context

  if auxiliaryConstraint
    then addAuxiliaryInstanceConstraints [constraint]
    else addInstanceConstraints [constraint]

  return metaExpr

-- | Creates an instance constraint as a subgoal of an existing instance constraint.
createDerivedInstanceConstraint ::
  (MonadTypeChecker builtin m) =>
  (ConstraintContext builtin, InstanceConstraintOrigin builtin) ->
  Relevance ->
  Value builtin ->
  m (Expr builtin, WithContext (InstanceConstraint builtin))
createDerivedInstanceConstraint (ctx, origin) r t = do
  let p = provenanceOf ctx
  let dbLevel = contextDBLevel ctx
  let newTypeClassExpr = quote p dbLevel t
  (metaID, metaExpr) <- freshSolutionMeta p newTypeClassExpr (boundContextOf ctx)
  let newConstraint = Resolve origin metaID r $ parseInstanceGoal t

  newCtx <- copyContext ctx Nothing
  return (metaExpr, WithContext newConstraint newCtx)

parseInstanceGoal ::
  forall builtin.
  (PrintableBuiltin builtin) =>
  Value builtin ->
  InstanceGoal builtin
parseInstanceGoal originalValue = go [] originalValue
  where
    go :: Telescope builtin -> Value builtin -> InstanceGoal builtin
    go telescope = \case
      VPi binder _body
        | not (isExplicit binder) -> developerError "Instance goals with telescopes not yet supported"
      VBuiltin b spine -> InstanceGoal telescope b spine
      _ -> developerError $ "Malformed instance goal" <+> prettyVerbose originalValue

solveMeta ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  MetaID ->
  Expr builtin ->
  BoundCtx (Type builtin) ->
  m ()
solveMeta meta solution solutionCtx = do
  metaInfo <- getMetaInfo meta
  case metaSolution metaInfo of
    Just existing ->
      compilerDeveloperError $
        "meta-variable"
          <+> pretty meta
          <+> "already solved as"
          <+> line
          <> indent 2 (squotes (prettyVerbose (unnormalised existing)))
          <> line
          <> "but is being re-solved as"
            <+> line
          <> indent 2 (squotes (prettyVerbose solution))
          <> line
          <> "in context" <+> pretty (toNamedBoundCtx solutionCtx)
    Nothing -> do
      let abstractedSolution = abstractOverCtx (metaCtx metaInfo) solution
      let env = boundContextToEnv solutionCtx
      gluedSolution <- Glued abstractedSolution <$> normaliseInEnv (toNamedBoundCtx solutionCtx) env abstractedSolution

      logDebug MaxDetail $
        "solved"
          <+> pretty meta
          <+> "as"
          <+> prettyExternal (WithContext solution (toNamedBoundCtx solutionCtx))

      modifyTypeCheckerState $ \TypeCheckerState {..} ->
        TypeCheckerState
          { metaVariableCtx = addMetaSolution gluedSolution meta metaVariableCtx,
            solvedMetaState = registerSolvedMeta meta solvedMetaState,
            ..
          }

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall builtin m constraint.
  (MonadTypeChecker builtin m, PrettyExternal (Contextualised constraint (ConstraintContext builtin))) =>
  m [Contextualised constraint (ConstraintContext builtin)] ->
  ([Contextualised constraint (ConstraintContext builtin)] -> m ()) ->
  (Contextualised constraint (ConstraintContext builtin) -> m ()) ->
  Bool ->
  Proxy builtin ->
  m ()
runConstraintSolver getConstraints setConstraints attemptToSolveConstraint topLevel proxy = do
  unsolvedConstraints <- getConstraints
  if null unsolvedConstraints
    then logDebug MaxDetail "No constraints found"
    else do
      when topLevel $ logUnsolvedUnknowns proxy
      loop 0
  where
    loop :: Int -> m ()
    loop loopNumber = do
      unsolvedConstraints <- getConstraints
      if null unsolvedConstraints
        then return mempty
        else do
          isUnblocked <- getIsUnblockedFn

          case findFirstConstraint isUnblocked unsolvedConstraints of
            Nothing -> return mempty
            Just (unblockedConstraint, remainingConstraints) -> do
              -- We have made useful progress so start a new pass
              setConstraints remainingConstraints

              logCompilerSection MaxDetail ("trying:" <+> prettyExternal unblockedConstraint) $
                attemptToSolveConstraint unblockedConstraint

              loop (loopNumber + 1)

logUnsolvedUnknowns :: forall builtin m. (MonadTypeChecker builtin m) => Proxy builtin -> m ()
logUnsolvedUnknowns _proxy = do
  logDebugM MaxDetail $ do
    maybeDecl <- getCurrentDecl @builtin
    metaVarCtx <- getMetaVariableCtx @builtin
    updatedMetaVarCtx <- substMetaVariables metaVarCtx

    unsolvedConstraints <- getActiveConstraints @builtin

    isUnblocked <- getIsUnblockedFn
    let (unblockedConstraints, blockedConstraints) = partition isUnblocked unsolvedConstraints
    let constraintsDoc =
          "unsolved-blocked-constraints:"
            <> line
            <> indent 2 (prettyConstraints blockedConstraints)
            <> line
            <> "unsolved-unblocked-constraints:"
            <> line
            <> indent 2 (prettyConstraints unblockedConstraints)
            <> line

    let declDoc = case maybeDecl of
          Nothing -> ""
          Just decl ->
            "current-decl:"
              <> line
              <> indent 2 (prettyExternal decl)
              <> line

    let (solvedMetas, unsolvedMetas) = MetaMap.partition (isJust . metaSolution) updatedMetaVarCtx

    return $
      "solved-metas:"
        <> line
        <> indent 2 (prettyVerbose solvedMetas)
        <> line
        <> "unsolved-metas:"
        <> line
        <> indent 2 (prettyVerbose unsolvedMetas)
        <> line
        <> constraintsDoc
        <> declDoc

prettyConstraints :: (PrintableBuiltin builtin) => [WithContext (Constraint builtin)] -> Doc a
prettyConstraints constraints = do
  let sortedConstraints = sortOn (constraintID . contextOf) constraints
  let pairs = fmap prettyExternal sortedConstraints
  prettySetLike pairs

-- | Find the first constraint satisfying `p` appending all the constraints that don't satisfy it to
-- the end of the list, so we don't search through them again immediately next time.
findFirstConstraint :: forall a. (a -> Bool) -> [a] -> Maybe (a, [a])
findFirstConstraint p xs = (\(found, seen, unseen) -> (found, unseen <> seen)) <$> go xs
  where
    go :: [a] -> Maybe (a, [a], [a])
    go = \case
      [] -> Nothing
      c : cs
        | p c -> Just (c, [], cs)
        | otherwise -> fmap (\(found, seen, unseen) -> (found, c : seen, unseen)) (go cs)

checkAllConstraintsSolved ::
  (MonadTypeChecker builtin m) =>
  Proxy builtin ->
  m [Contextualised constraint (ConstraintContext builtin)] ->
  (constraint -> Constraint builtin) ->
  m ()
checkAllConstraintsSolved _ getConstraints toConstraint = do
  constraints <- getConstraints
  case constraints of
    [] -> return ()
    (c : cs) -> do
      let failedConstraints = mapObject toConstraint <$> (c :| cs)
      throwError $ TypingError $ UnsolvedConstraints failedConstraints
