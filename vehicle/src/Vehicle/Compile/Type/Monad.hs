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
    addInstanceToInstanceDatabase,
    TelescopeType (..),
    instantiateTelescope,
    -- Other
    clearMetaCtx,
    logUnsolvedUnknowns,
    findFirstConstraint,
    checkAllConstraintsSolved,
    forceThunkWithMetas,
    forceApplicationWithMetas,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.List (partition, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (CompileError (..), TypingError (..), compilerDeveloperError)
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Quote (unnormalise)
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
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.ModuleInterface
import Vehicle.Data.Variable.Bound.Context.Generic

runTypeCheckerTInitially ::
  (Monad m, TypableBuiltin builtin) =>
  InstanceDatabase builtin ->
  ImportedModuleContext builtin ->
  TypeCheckerT builtin m a ->
  m (a, ModuleTypingInterface builtin, FreeCtx builtin)
runTypeCheckerTInitially builtinInstances importedCtx e = do
  let state = emptyTypeCheckerState builtinInstances importedCtx
  (result, internalState) <- runTypeCheckerT state e
  return (result, currentModuleInterface internalState, currentFreeCtx internalState)

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerTHypothetically ::
  forall builtin m a.
  (MonadTypeChecker builtin m) =>
  TypeCheckerT builtin (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState builtin))
runTypeCheckerTHypothetically e = do
  callDepth <- getCallDepth
  state <- getTypeCheckerState
  result <- runExceptT $ runTypeCheckerT state e
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
  (MonadTypeChecker builtin m, TypableBuiltin builtin) =>
  Provenance ->
  Type builtin ->
  Relevance ->
  BoundCtx (Type builtin) ->
  m (Expr builtin)
freshMetaExpr p t relevance boundCtx = do
  let ctx = if useDependentMetas (Proxy @builtin) then boundCtx else mempty
  snd <$> freshMeta p t relevance ctx

freshSolutionMeta ::
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  Relevance ->
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
  (finalTypeID, finalType) <- freshSolutionMeta p (TypeUniverse p 0) Relevant ctx
  (finalExprID, finalExpr) <- freshSolutionMeta p finalType Relevant ctx

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
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin) =>
  Bool ->
  BoundCtx (Type builtin) ->
  Provenance ->
  InstanceConstraintOrigin builtin ->
  Relevance ->
  Type builtin ->
  m (Expr builtin)
createFreshInstanceConstraint auxiliaryConstraint boundCtx p origin relevance tcExpr = do
  let env = boundContextToEnv boundCtx
  (metaID, metaExpr) <- freshSolutionMeta p tcExpr relevance boundCtx

  context <- createFreshConstraintCtx p boundCtx
  let nTCExpr = Unforced env tcExpr
  goal <- parseInstanceGoal boundCtx nTCExpr
  let constraint = Resolve origin metaID relevance Nothing goal
  let constraintWithCtx = WithContext constraint context

  if auxiliaryConstraint
    then addAuxiliaryInstanceConstraints [constraintWithCtx]
    else addInstanceConstraints [constraintWithCtx]

  return metaExpr

-- | Creates an instance constraint as a subgoal of an existing instance constraint.
createDerivedInstanceConstraint ::
  (MonadTypeChecker builtin m) =>
  (ConstraintContext builtin, InstanceConstraintOrigin builtin) ->
  Relevance ->
  ThunkWithMetas builtin ->
  m (Expr builtin, WithContext (InstanceConstraint builtin))
createDerivedInstanceConstraint (ctx, origin) relevance t = do
  let p = provenanceOf ctx
  let dbLevel = contextDBLevel ctx
  let newTypeClassExpr = unnormalise dbLevel t
  (metaID, metaExpr) <- freshSolutionMeta p newTypeClassExpr relevance (boundContextOf ctx)
  goal <- parseInstanceGoal (boundContextOf ctx) t
  let newConstraint = Resolve origin metaID relevance Nothing goal

  newCtx <- copyContext ctx Nothing
  return (metaExpr, WithContext newConstraint newCtx)

parseInstanceGoal ::
  forall builtin m.
  (MonadTypeChecker builtin m, PrintableBuiltin builtin) =>
  BoundCtx (Type builtin) ->
  ThunkWithMetas builtin ->
  m (InstanceGoal builtin)
parseInstanceGoal ctx originalValue = go [] originalValue
  where
    go :: Telescope builtin -> ThunkWithMetas builtin -> m (InstanceGoal builtin)
    go telescope value = do
      (forcedValue, _) <- forceThunkWithMetas (toNamedBoundCtx ctx) value
      case forcedValue of
        VBuiltin b spine -> return $ InstanceGoal telescope (Right b) spine
        VFreeVar b spine -> return $ InstanceGoal telescope (Left b) spine
        VPi binder _body | not (isExplicit binder) -> developerError "Instance goals with telescopes not yet supported"
        _ -> developerError $ "Malformed instance goal" <+> prettyVerbose originalValue

addInstanceToInstanceDatabase ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Decl builtin ->
  Maybe InstancePriority ->
  m ()
addInstanceToInstanceDatabase decl priority =
  case decl of
    DefFunction _ _ _ t e -> do
      let candidate = InstanceCandidate t e priority
      instanceHead <- findValidInstanceHead (identifierOf decl, provenanceOf decl) candidate
      modifyTypeCheckerState $ \state ->
        state
          { currentModuleInterface =
              (currentModuleInterface state)
                { instanceDatabase =
                    insertInstanceIntoDatabase
                      instanceHead
                      candidate
                      (instanceDatabase $ currentModuleInterface state)
                }
          }
    _ -> developerError "Malformed instance declaration"

findValidInstanceHead ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  DeclProvenance ->
  InstanceCandidate builtin ->
  m (InstanceHead builtin)
findValidInstanceHead declProv candidate = do
  let expr = candidateExpr candidate
  case findInstanceGoalHead expr of
    Left _err -> throwError $ TypingError $ InvalidInstanceHead declProv expr
    Right instanceHead -> case instanceHead of
      Left typeClassIdent -> do
        typeClassDecl <- getDecl (Proxy @builtin) typeClassIdent
        unless (isTypeClassDecl typeClassDecl) $ do
          throwError $ TypingError $ NonTypeClassInstanceHead (Proxy @builtin) declProv typeClassIdent
        return instanceHead
      Right _builtin -> return instanceHead

solveMeta ::
  forall builtin m.
  (MonadTypeChecker builtin m, NormalisableBuiltin builtin) =>
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
      let normAbstractedSolution = Unforced env abstractedSolution
      let gluedSolution = Glued abstractedSolution normAbstractedSolution

      logDebug MaxDetail $
        "solved"
          <+> pretty meta
          <+> "as"
          <+> prettyExternal (WithContext solution (toNamedBoundCtx solutionCtx))

      modifyTypeCheckerDeclState $ \state ->
        state
          { metaVariableCtx = addMetaSolution gluedSolution meta (metaVariableCtx state),
            solvedMetaState = registerSolvedMeta meta (solvedMetaState state)
          }

-- | Attempts to solve as many constraints as possible. Takes in
-- the set of meta-variables solved since the solver was last run and outputs
-- the set of meta-variables solved during this run.
runConstraintSolver ::
  forall builtin m constraint.
  (MonadTypeChecker builtin m, TypableBuiltin builtin, PrettyExternal (Contextualised constraint (ConstraintContext builtin))) =>
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

logUnsolvedUnknowns :: forall builtin m. (MonadTypeChecker builtin m, NormalisableBuiltin builtin) => Proxy builtin -> m ()
logUnsolvedUnknowns _proxy = do
  logDebugM MaxDetail $ do
    maybeDecl <- getCurrentDecl @builtin
    metaVarCtx <- getMetaVariableCtx @builtin
    updatedMetaVarCtx <- substMetaVariables @builtin metaVarCtx

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
  (MonadTypeChecker builtin m, Eq builtin, NormalisableBuiltin builtin) =>
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

data TelescopeType
  = InstanceTelescope
  | RecordTelescope

instantiateTelescope ::
  (MonadTypeChecker builtin m, TypableBuiltin builtin) =>
  TelescopeType ->
  (Relevance -> Type builtin -> m (Expr builtin)) ->
  BoundCtx (Type builtin) ->
  (Type builtin, Expr builtin) ->
  m (Type builtin, Expr builtin, [Arg builtin])
instantiateTelescope telescopeType createFreshInstance boundCtx = \case
  (Pi _ piBinder exprBody, Lam _ _solutionBinder solutionBody) -> do
    let binderProvenance = provenanceOf piBinder
    let binderType = typeOf piBinder
    let binderRelevance = relevanceOf piBinder
    newArg <- case visibilityOf piBinder of
      Explicit {} -> case telescopeType of
        InstanceTelescope -> compilerDeveloperError "Should not have an explicit argument in instance goal telescope"
        RecordTelescope -> freshMetaExpr binderProvenance binderType binderRelevance boundCtx
      Implicit {} ->
        freshMetaExpr binderProvenance binderType binderRelevance boundCtx
      Instance {} -> do
        createFreshInstance (relevanceOf piBinder) binderType
    let exprBodyResult = newArg `substDBInto` exprBody
    let solutionBodyResult = newArg `substDBInto` solutionBody
    (typ', body', args) <- instantiateTelescope telescopeType createFreshInstance boundCtx (exprBodyResult, solutionBodyResult)
    return (typ', body', argFromBinder piBinder newArg : args)
  (typ, body) -> return (typ, body, [])
