{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Context.Tensor.Instance
  ( TensorBoundContextT,
    runFreshTensorBoundContextT,
    mapTensorBoundContextT,
  )
where

import Control.Monad (void)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), MonadTrans (..), StateT (..), evalStateT, gets, mapStateT, modify)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkModality (..))
import Vehicle.Data.Variable.Bound.Context.Name.Class (MonadNameContext (..), MonadReadableNameContext (..), getBinderDepth)
import Vehicle.Data.Variable.Bound.Context.Tensor.Class
import Vehicle.Data.Variable.Bound.Context.Tensor.Core
import Vehicle.Data.Variable.Bound.Level (NestedSliceVariable (..), SliceVariable (..))
import Vehicle.Prelude.IO qualified as VIO

--------------------------------------------------------------------------------
-- Tensor variables

newtype TensorBoundContextT m a = TensorBoundContextT
  { unTensorBoundContextT :: StateT NestedTensorVariableCtx m a
  }
  deriving (Functor, Applicative, Monad)

-- | Runs a computation in the tensor context monad allowing you to keep track
-- of all variables including tensor variables and their children.
runFreshTensorBoundContextT :: (Monad m) => TensorBoundContextT m a -> m a
runFreshTensorBoundContextT f = evalStateT (unTensorBoundContextT f) emptyNestedCtx

instance MonadTrans TensorBoundContextT where
  lift = TensorBoundContextT . lift

mapTensorBoundContextT ::
  (m (a, NestedTensorVariableCtx) -> n (b, NestedTensorVariableCtx)) ->
  TensorBoundContextT m a ->
  TensorBoundContextT n b
mapTensorBoundContextT f m =
  TensorBoundContextT (mapStateT f (unTensorBoundContextT m))

--------------------------------------------------------------------------------
-- Other monad preservation

instance (MonadIO m) => MonadIO (TensorBoundContextT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (TensorBoundContextT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

instance (MonadLogger m) => MonadLogger (TensorBoundContextT m) where
  setCallDepth = TensorBoundContextT . setCallDepth
  getCallDepth = TensorBoundContextT getCallDepth
  incrCallDepth = TensorBoundContextT incrCallDepth
  decrCallDepth = TensorBoundContextT decrCallDepth
  getDebugLevel = TensorBoundContextT getDebugLevel
  logMessage = TensorBoundContextT . logMessage
  logWarning = TensorBoundContextT . logWarning
  runCompilerPass = mapTensorBoundContextT . runCompilerPass
  runCompileDecl = mapTensorBoundContextT . runCompileDecl

instance (MonadError e m) => MonadError e (TensorBoundContextT m) where
  throwError = lift . throwError
  catchError m f =
    TensorBoundContextT $
      catchError (unTensorBoundContextT m) (unTensorBoundContextT . f)

instance (MonadState s m) => MonadState s (TensorBoundContextT m) where
  get = lift get
  put = lift . put

instance (MonadReader s m) => MonadReader s (TensorBoundContextT m) where
  ask = lift ask
  local = mapTensorBoundContextT . local

instance (MonadSupply s m) => MonadSupply s (TensorBoundContextT m) where
  demand = lift demand

instance (Monad m) => MonadNameContext (TensorBoundContextT m) where
  addNameToContext = addNonTensorBinderToContext

instance (Monad m) => MonadTensorBoundContext (TensorBoundContextT m) where
  addNonTensorBinderToContext binder action =
    TensorBoundContextT $ do
      localState (appendNonTensorVariableToNestedCtx $ void binder) $
        unTensorBoundContextT action

  addTensorBinderToContextLocally knownShapePrefix binder action =
    TensorBoundContextT $ do
      localState (appendTensorVariableToNestedCtx (void binder) (UniModal knownShapePrefix)) $
        unTensorBoundContextT action

  addTensorBinderToContextPermenantly p name shape = do
    lv <- getBinderDepth
    TensorBoundContextT $ do
      modify (appendTensorVariableToNestedCtx (mkExplicitBinder () (Just (p, name))) shape)
      return $ NestedSliceVariable shape (SliceVariable lv)

instance (Monad m) => MonadReadableNameContext (TensorBoundContextT m) where
  getNameContext = TensorBoundContextT $ gets $ fmap Just . nestedVariableCtxNames

instance (Monad m) => MonadReadableTensorBoundContext (TensorBoundContextT m) where
  getNestedVariableCtx = TensorBoundContextT get
