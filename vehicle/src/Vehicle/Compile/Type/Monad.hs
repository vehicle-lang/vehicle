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
    createFreshTypeClassConstraint,
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
    debugError,
  )
where

import Control.Monad (when)
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

runTypeChecker :: (Monad m) => TypingDeclCtx builtin -> TypeCheckerT builtin m a -> m a
runTypeChecker declCtx e = fst <$> runTypeCheckerT declCtx emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically ::
  (TCM builtin m) =>
  TypeCheckerT builtin (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState builtin))
runTypeCheckerHypothetically e = do
  callDepth <- getCallDepth
  declCtx <- getDeclContext
  state <- getMetaState
  result <- runExceptT $ runTypeCheckerT declCtx state e
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
createFreshTypeClassConstraint ::
  forall builtin m.
  (TCM builtin m) =>
  TypingBoundCtx builtin ->
  (Expr Ix builtin, [Arg Ix builtin]) ->
  Relevance ->
  Type Ix builtin ->
  m (GluedExpr builtin)
createFreshTypeClassConstraint boundCtx (fun, funArgs) relevance tcExpr = do
  let env = typingBoundContextToEnv boundCtx

  let p = provenanceOf fun
  (meta, metaExpr) <- freshMetaIdAndExpr p tcExpr boundCtx

  let origin = CheckingTypeClass fun funArgs tcExpr
  let originProvenance = provenanceOf tcExpr
  cid <- generateFreshConstraintID (Proxy @builtin)
  let context = ConstraintContext cid originProvenance origin p unknownBlockingStatus boundCtx
  nTCExpr <- whnf env tcExpr
  let constraint = WithContext (Has meta relevance nTCExpr) context

  addInstanceConstraints [constraint]

  return metaExpr

instantiateArgForNonExplicitBinder ::
  (TCM builtin m) =>
  TypingBoundCtx builtin ->
  Provenance ->
  (Expr Ix builtin, [Arg Ix builtin]) ->
  Binder Ix builtin ->
  m (GluedArg builtin)
instantiateArgForNonExplicitBinder boundCtx p origin binder = do
  let binderType = typeOf binder
  checkedExpr <- case visibilityOf binder of
    Explicit {} -> compilerDeveloperError "Should not be instantiating Arg for explicit Binder"
    Implicit {} -> freshMetaExpr p binderType boundCtx
    Instance {} -> createFreshTypeClassConstraint boundCtx origin (relevanceOf binder) binderType
  return $ Arg p (markInserted $ visibilityOf binder) (relevanceOf binder) checkedExpr

debugError :: forall builtin m. (TCM builtin m) => Proxy builtin -> Int -> m ()
debugError _ limit = do
  idd <- generateFreshConstraintID (Proxy @builtin)
  when (idd > limit) $ compilerDeveloperError "Hi"
