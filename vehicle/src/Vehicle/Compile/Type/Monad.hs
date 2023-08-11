module Vehicle.Compile.Type.Monad
  ( TCM,
    MonadTypeChecker (..),
    TypeCheckerState,
    TypableBuiltin (..),
    -- Top-level interface
    runTypeChecker,
    runTypeCheckerHypothetically,
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
    -- Constraints
    copyContext,
    createFreshUnificationConstraint,
    createFreshInstanceConstraint,
    getActiveConstraints,
    getActiveUnificationConstraints,
    getActiveInstanceConstraints,
    setInstanceConstraints,
    setUnificationConstraints,
    addConstraints,
    addUnificationConstraints,
    -- Other
    clearMetaCtx,
    getBinderNameOrFreshName,
    getDeclType,
    instantiateArgForNonExplicitBinder,
    glueNBE,
  )
where

import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (CompileError (..), compilerDeveloperError)
import Vehicle.Compile.Normalise.Monad (MonadNorm)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Expr.DeBruijn (Ix)
import Vehicle.Expr.Normalised

-- | The type-checking monad.
type TCM builtin m =
  ( MonadTypeChecker builtin m,
    MonadNorm builtin m,
    TypableBuiltin builtin
  )

runTypeChecker ::
  (Monad m) =>
  TypingDeclCtx builtin ->
  InstanceCandidateDatabase builtin ->
  TypeCheckerT builtin m a ->
  m a
runTypeChecker declCtx instanceCandidates e =
  fst <$> runTypeCheckerT declCtx instanceCandidates emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically ::
  (TCM builtin m) =>
  TypeCheckerT builtin (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState builtin))
runTypeCheckerHypothetically e = do
  callDepth <- getCallDepth
  declCtx <- getDeclContext
  instanceCandidates <- getInstanceCandidates
  state <- getMetaState
  result <- runExceptT $ runTypeCheckerT declCtx instanceCandidates state e
  case result of
    Right value -> return $ Right value
    Left err -> case err of
      DevError {} -> throwError err
      _ -> do
        -- If we errored then reset the call depth so logging is not disrupted.
        setCallDepth callDepth
        return $ Left err

-- | Accepts the hypothetical outcome of the type-checker.
adoptHypotheticalState :: (TCM builtin m) => TypeCheckerState builtin -> m ()
adoptHypotheticalState = modifyMetaCtx . const

freshMetaIdAndExpr ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Type Ix builtin ->
  TypingBoundCtx builtin ->
  m (MetaID, GluedExpr builtin)
freshMetaIdAndExpr p t boundCtx = do
  let ctx = if useDependentMetas (Proxy @builtin) then boundCtx else mempty
  freshMeta p t ctx

freshMetaExpr ::
  forall builtin m.
  (TCM builtin m) =>
  Provenance ->
  Type Ix builtin ->
  TypingBoundCtx builtin ->
  m (GluedExpr builtin)
freshMetaExpr p t boundCtx = snd <$> freshMetaIdAndExpr p t boundCtx

-- | Adds an entirely new type-class constraint (as opposed to one
-- derived from another constraint).
createFreshInstanceConstraint ::
  forall builtin m.
  (TCM builtin m) =>
  TypingBoundCtx builtin ->
  (Expr Ix builtin, [Arg Ix builtin], Type Ix builtin) ->
  Relevance ->
  Type Ix builtin ->
  m (GluedExpr builtin)
createFreshInstanceConstraint boundCtx (fun, funArgs, funType) relevance tcExpr = do
  let origin =
        InstanceConstraintOrigin
          { checkedInstanceOp = fun,
            checkedInstanceOpArgs = funArgs,
            checkedInstanceOpType = funType,
            checkedInstanceType = tcExpr
          }

  let env = typingBoundContextToEnv boundCtx

  let p = provenanceOf fun
  (meta, metaExpr) <- freshMetaIdAndExpr p tcExpr boundCtx

  let originProvenance = provenanceOf tcExpr
  cid <- generateFreshConstraintID (Proxy @builtin)
  let context = ConstraintContext cid originProvenance p unknownBlockingStatus boundCtx
  nTCExpr <- whnf env tcExpr
  let constraint = WithContext (Resolve origin meta relevance nTCExpr) context

  addInstanceConstraints [constraint]

  return metaExpr

instantiateArgForNonExplicitBinder ::
  (TCM builtin m) =>
  TypingBoundCtx builtin ->
  Provenance ->
  (Expr Ix builtin, [Arg Ix builtin], Type Ix builtin) ->
  Binder Ix builtin ->
  m (GluedArg builtin)
instantiateArgForNonExplicitBinder boundCtx p origin binder = do
  let binderType = typeOf binder
  checkedExpr <- case visibilityOf binder of
    Explicit {} -> compilerDeveloperError "Should not be instantiating Arg for explicit Binder"
    Implicit {} -> freshMetaExpr p binderType boundCtx
    Instance {} -> createFreshInstanceConstraint boundCtx origin (relevanceOf binder) binderType
  return $ Arg p (markInserted $ visibilityOf binder) (relevanceOf binder) checkedExpr
