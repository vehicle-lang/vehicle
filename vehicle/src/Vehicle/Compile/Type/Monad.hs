module Vehicle.Compile.Type.Monad
  ( TCM,
    MonadTypeChecker (..),
    TypeCheckerState,
    -- Top-level interface
    runTypeChecker,
    runTypeCheckerHypothetically,
    adoptHypotheticalState,
    -- Meta variables
    freshPolarityMeta,
    freshLinearityMeta,
    freshExprMeta,
    freshTypeClassPlacementMeta,
    getMetaType,
    getMetaCtx,
    getMetaProvenance,
    getUnsolvedMetas,
    solveMeta,
    extendBoundCtxOfMeta,
    filterMetasByTypes,
    removeMetaDependencies,
    getMetasLinkedToMetasIn,
    trackSolvedMetas,
    prettyMeta,
    prettyMetas,
    -- Constraints
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
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Compile.Type.Subsystem (TypableBuiltin)
import Vehicle.Compile.Type.VariableContext (TypingDeclCtx)

-- | The type-checking monad.
type TCM builtin m =
  ( MonadTypeChecker builtin m,
    MonadNorm builtin m,
    TypableBuiltin builtin
  )

runTypeChecker :: Monad m => TypingDeclCtx builtin -> TypeCheckerT builtin m a -> m a
runTypeChecker declCtx e = fst <$> runTypeCheckerT declCtx emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically ::
  TCM builtin m =>
  TypeCheckerT builtin (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState builtin))
runTypeCheckerHypothetically e = do
  callDepth <- getCallDepth
  declCtx <- getDeclContext
  state <- getMetaState
  result <- runExceptT $ runTypeCheckerT declCtx state e
  case result of
    Right value -> return $ Right value
    Left err -> do
      -- If we errored then reset the call depth so logging is not disrupted.
      setCallDepth callDepth
      return $ Left err

-- | Accepts the hypothetical outcome of the type-checker.
adoptHypotheticalState :: TCM builtin m => TypeCheckerState builtin -> m ()
adoptHypotheticalState = modifyMetaCtx . const
