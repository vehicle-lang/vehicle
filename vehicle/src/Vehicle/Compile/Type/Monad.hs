module Vehicle.Compile.Type.Monad
  ( module Vehicle.Compile.Type.Monad.Class,
    TCM,
    runTypeChecker,
    runTypeCheckerHypothetically,
    adoptHypotheticalState,
  )
where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Vehicle.Compile.Error (CompileError)
import Vehicle.Compile.Prelude (MonadLogger (..))
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.Monad.Instance
import Vehicle.Compile.Type.VariableContext (TypingDeclCtx)

-- | The type-checking monad.
type TCM m = MonadTypeChecker m

runTypeChecker :: Monad m => TypingDeclCtx -> TypeCheckerT m a -> m a
runTypeChecker declCtx e = fst <$> runTypeCheckerT declCtx emptyTypeCheckerState e

-- | Runs a hypothetical computation in the type-checker,
-- returning the resulting state of the type-checker.
runTypeCheckerHypothetically ::
  TCM m =>
  TypeCheckerT (ExceptT CompileError m) a ->
  m (Either CompileError (a, TypeCheckerState))
runTypeCheckerHypothetically e = do
  callDepth <- getCallDepth
  declCtx <- getDeclContext
  state <- getMetaCtx
  result <- runExceptT $ runTypeCheckerT declCtx state e
  case result of
    Right value -> return $ Right value
    Left err -> do
      -- If we errored then reset the call depth so logging is not disrupted.
      setCallDepth callDepth
      return $ Left err

-- | Accepts the hypothetical outcome of the type-checker.
adoptHypotheticalState :: TCM m => TypeCheckerState -> m ()
adoptHypotheticalState = modifyMetaCtx . const
