module Vehicle.Compile.Type.Monad
  ( module Vehicle.Compile.Type.Monad.Class,
    TCM,
    runTypeChecker,
    runTypeCheckerHypothetically,
    adoptHypotheticalState,
  )
where

import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Compile.Type.VariableContext (TypingDeclCtx)

-- | The type-checking monad.
type TCM m = MonadTypeChecker m

runTypeChecker :: Monad m => TypingDeclCtx -> TypeCheckerT m a -> m a
runTypeChecker declCtx e = fst <$> runTypeCheckerT declCtx emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically :: TCM m => TypeCheckerT m a -> m (a, TypeCheckerState)
runTypeCheckerHypothetically e = do
  declCtx <- getDeclContext
  state <- getMetaCtx
  runTypeCheckerT declCtx state e

-- | Accepts the hypothetical outcome of the type-checker.
adoptHypotheticalState :: TCM m => TypeCheckerState -> m ()
adoptHypotheticalState = modifyMetaCtx . const
