{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( TypeCheckerT,
    runTypeCheckerT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State
  ( MonadState (..),
    StateT (..),
    gets,
    mapStateT,
    modify,
  )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.VariableContext

--------------------------------------------------------------------------------
-- Implementation

type TypeCheckerTInternals m =
  ReaderT TypingDeclCtx (StateT TypeCheckerState m)

clearFreshNamesInternal :: Monad m => TypeCheckerTInternals m ()
clearFreshNamesInternal =
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = 0, ..})

getFreshNameInternal :: Monad m => CheckedType -> TypeCheckerTInternals m Name
getFreshNameInternal _typ = do
  nameID <- gets freshNameState
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = nameID + 1, ..})
  return $ layoutAsText $ "_x" <> pretty nameID

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT :: Monad m => TypingDeclCtx -> TypeCheckerState -> TypeCheckerT m a -> m (a, TypeCheckerState)
runTypeCheckerT declCtx metaCtx (TypeCheckerT e) =
  runStateT (runReaderT e declCtx) metaCtx

mapTypeCheckerT ::
  (m (a, TypeCheckerState) -> n (b, TypeCheckerState)) ->
  TypeCheckerT m a ->
  TypeCheckerT n b
mapTypeCheckerT f m = TypeCheckerT (mapReaderT (mapStateT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance MonadCompile m => MonadTypeChecker (TypeCheckerT m) where
  getDeclContext = TypeCheckerT ask
  addDeclContext d s = TypeCheckerT $ local (addToDeclCtx d) (unTypeCheckerT s)
  getMetaCtx = TypeCheckerT get
  modifyMetaCtx f = TypeCheckerT $ modify f
  getFreshName typ = TypeCheckerT $ getFreshNameInternal typ
  clearFreshNames = TypeCheckerT clearFreshNamesInternal

instance MonadTrans TypeCheckerT where
  lift = TypeCheckerT . lift . lift

instance MonadError e m => MonadError e (TypeCheckerT m) where
  throwError = lift . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance MonadLogger m => MonadLogger (TypeCheckerT m) where
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance MonadReader r m => MonadReader r (TypeCheckerT m) where
  ask = lift ask
  local = mapTypeCheckerT . local
