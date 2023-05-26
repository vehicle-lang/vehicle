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
    getActiveTypeClassConstraints,
    setTypeClassConstraints,
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
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error (CompileError (..), compilerDeveloperError)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

-- | The type-checking monad.
type TCM types m =
  ( MonadTypeChecker types m,
    MonadNorm types m,
    TypableBuiltin types
  )

runTypeChecker :: (Monad m) => TypingDeclCtx types -> TypeCheckerT types m a -> m a
runTypeChecker declCtx e = fst <$> runTypeCheckerT declCtx emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically ::
  (TCM types m) =>
  TypeCheckerT types (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState types))
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
adoptHypotheticalState :: (TCM types m) => TypeCheckerState types -> m ()
adoptHypotheticalState = modifyMetaCtx . const

freshMetaIdAndExpr ::
  forall types m.
  (TCM types m) =>
  Provenance ->
  NormalisableType types ->
  TypingBoundCtx types ->
  m (MetaID, GluedExpr types)
freshMetaIdAndExpr p t boundCtx = do
  let ctx = if useDependentMetas (Proxy @types) then boundCtx else mempty
  freshMeta p t ctx

freshMetaExpr ::
  forall types m.
  (TCM types m) =>
  Provenance ->
  NormalisableType types ->
  TypingBoundCtx types ->
  m (GluedExpr types)
freshMetaExpr p t boundCtx = snd <$> freshMetaIdAndExpr p t boundCtx

-- | Adds an entirely new type-class constraint (as opposed to one
-- derived from another constraint).
createFreshTypeClassConstraint ::
  forall types m.
  (TCM types m) =>
  TypingBoundCtx types ->
  (NormalisableExpr types, [NormalisableArg types]) ->
  NormalisableType types ->
  m (GluedExpr types)
createFreshTypeClassConstraint boundCtx (fun, funArgs) tcExpr = do
  (tc, tcArgs) <- case tcExpr of
    BuiltinExpr _ (CType tc) args -> return (tc, NonEmpty.toList args)
    _ ->
      compilerDeveloperError $
        "Malformed type class constraint" <+> prettyVerbose tcExpr

  let env = typingBoundContextToEnv boundCtx
  nArgs <- traverse (traverse (whnf env)) tcArgs

  let p = provenanceOf fun
  (meta, metaExpr) <- freshMetaIdAndExpr p tcExpr boundCtx

  let origin = CheckingTypeClass fun funArgs tc tcArgs
  let originProvenance = provenanceOf tcExpr
  cid <- generateFreshConstraintID (Proxy @types)
  let context = ConstraintContext cid originProvenance origin p unknownBlockingStatus boundCtx
  let constraint = WithContext (Has meta tc (fmap argExpr nArgs)) context

  addTypeClassConstraints [constraint]

  return metaExpr

instantiateArgForNonExplicitBinder ::
  (TCM types m) =>
  TypingBoundCtx types ->
  Provenance ->
  (NormalisableExpr types, [NormalisableArg types]) ->
  NormalisableBinder types ->
  m (GluedArg types)
instantiateArgForNonExplicitBinder boundCtx p origin binder = do
  let binderType = typeOf binder
  checkedExpr <- case visibilityOf binder of
    Explicit {} -> compilerDeveloperError "Should not be instantiating Arg for explicit Binder"
    Implicit {} -> freshMetaExpr p binderType boundCtx
    Instance {} -> createFreshTypeClassConstraint boundCtx origin binderType
  return $ Arg p (markInserted $ visibilityOf binder) (relevanceOf binder) checkedExpr

debugError :: forall types m. (TCM types m) => Proxy types -> Int -> m ()
debugError _ limit = do
  idd <- generateFreshConstraintID (Proxy @types)
  when (idd > limit) $ compilerDeveloperError "Hi"
