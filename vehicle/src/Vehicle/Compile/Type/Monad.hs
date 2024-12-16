module Vehicle.Compile.Type.Monad
  ( MonadTypeChecker (..),
    TypeCheckerState,
    -- Top-level interface
    runTypeCheckerTInitially,
    runTypeCheckerTHypothetically,
    adoptHypotheticalState,
    -- Meta variables
    freshMetaExpr,
    freshMetaIdAndExpr,
    getMetaType,
    getMetaCtx,
    getMetaProvenance,
    getUnsolvedMetas,
    solveMeta,
    extendBoundCtxOfMeta,
    removeMetaDependencies,
    getMetasLinkedToMetasIn,
    trackSolvedMetas,
    prettyMeta,
    prettyMetas,
    substMetas,
    -- Constraints
    copyContext,
    createFreshUnificationConstraint,
    createFreshInstanceConstraint,
    createFreshApplicationConstraint,
    getActiveConstraints,
    getActiveUnificationConstraints,
    getActiveInstanceConstraints,
    setInstanceConstraints,
    setUnificationConstraints,
    addUnificationConstraints,
    -- Other
    clearMetaCtx,
    glueNBE,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error (CompileError (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Builtin (TypableBuiltin (..))
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
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
  state <- getMetaState
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
adoptHypotheticalState = modifyMetaCtx . const

freshMetaIdAndExpr ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  BoundCtx (Type builtin) ->
  m (MetaID, GluedExpr builtin)
freshMetaIdAndExpr p t boundCtx = do
  let ctx = if useDependentMetas (Proxy @builtin) then boundCtx else mempty
  freshMeta p t ctx

freshMetaExpr ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  Type builtin ->
  BoundCtx (Type builtin) ->
  m (GluedExpr builtin)
freshMetaExpr p t boundCtx = snd <$> freshMetaIdAndExpr p t boundCtx

-- | Adds an entirely new unification constraint (as opposed to one
-- derived from another constraint).
createFreshUnificationConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  Provenance ->
  BoundCtx (Type builtin) ->
  UnificationConstraintOrigin builtin ->
  Type builtin ->
  Type builtin ->
  m ()
createFreshUnificationConstraint p ctx origin expectedType actualType = do
  let env = boundContextToEnv ctx
  normExpectedType <- normaliseInEnv env expectedType
  normActualType <- normaliseInEnv env actualType
  logDebug MaxDetail $ "Hit1" <+> prettyVerbose normExpectedType
  logDebug MaxDetail $ "Hit2" <+> prettyVerbose normActualType
  context <- createFreshConstraintCtx p p ctx
  let unification = Unify origin normExpectedType normActualType
  let constraint = WithContext unification context

  addUnificationConstraints [constraint]

createFreshApplicationConstraint ::
  forall builtin m.
  (MonadTypeChecker builtin m) =>
  BoundCtx (Type builtin) ->
  ArgInsertionProblem builtin ->
  MetaSet ->
  m (Expr builtin, Type builtin)
createFreshApplicationConstraint ctx problem blockingMetas = do
  let p = provenanceOf $ originalFun problem
  (typeMeta, finalType) <- freshMetaIdAndExpr p (TypeUniverse p 0) ctx
  (exprMeta, finalExpr) <- freshMetaIdAndExpr p (unnormalised finalType) ctx

  let constraint =
        InferArgs
          { exprSolutionMeta = exprMeta,
            typeSolutionMeta = typeMeta,
            argInsertionProblem = problem
          }

  context <- createFreshConstraintCtx p p ctx
  let blockedConstraint = WithContext constraint (blockCtxOn blockingMetas context)
  addApplicationConstraint blockedConstraint
  return (unnormalised finalExpr, unnormalised finalType)

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
  m (GluedExpr builtin)
createFreshInstanceConstraint auxiliaryConstraint boundCtx p origin relevance tcExpr = do
  let env = boundContextToEnv boundCtx
  (meta, metaExpr) <- freshMetaIdAndExpr p tcExpr boundCtx

  let originProvenance = provenanceOf tcExpr
  context <- createFreshConstraintCtx originProvenance p boundCtx
  nTCExpr <- normaliseInEnv env tcExpr
  let constraint = WithContext (Resolve origin meta relevance nTCExpr) context

  if auxiliaryConstraint
    then addAuxiliaryInstanceConstraints [constraint]
    else addInstanceConstraints [constraint]

  return metaExpr
