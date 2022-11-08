{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( module Vehicle.Compile.Type.Monad.Class
  , runTypeCheckerT
  , toNormalisationDeclContext
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State (MonadState (..), StateT (..), evalStateT, gets,
                            mapStateT, modify)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Trans (MonadTrans)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.VariableContext
import Vehicle.Compile.Type.Monad.Class

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT m a = TypeCheckerT
  { unTypeCheckerT :: ReaderT TypingDeclCtx (StateT TypingMetaCtx m) a
  } deriving (Functor, Applicative, Monad)

runTypeCheckerT :: Monad m => TypeCheckerT m a -> m a
runTypeCheckerT (TypeCheckerT e) = evalStateT (runReaderT e mempty) emptyMetaCtx

mapTypeCheckerT :: (m (a, TypingMetaCtx) -> n (b, TypingMetaCtx))
                -> TypeCheckerT m a
                -> TypeCheckerT n b
mapTypeCheckerT f m = TypeCheckerT (mapReaderT (mapStateT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance MonadCompile m => MonadTypeChecker (TypeCheckerT m) where
  getDeclContext = TypeCheckerT ask
  addDeclContext d s = TypeCheckerT $ local (addToDeclCtx d) (unTypeCheckerT s)
  getMetaCtx = TypeCheckerT get
  getsMetaCtx f = TypeCheckerT $ gets f
  putMetaCtx x = TypeCheckerT $ put x
  modifyMetaCtx f = TypeCheckerT $ modify f

instance MonadTrans TypeCheckerT where
  lift = TypeCheckerT . lift . lift

instance MonadError e m => MonadError e (TypeCheckerT m) where
  throwError = lift . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance MonadLogger m => MonadLogger (TypeCheckerT m) where
  getCallDepth  = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage    = lift . logMessage

instance MonadReader r m => MonadReader r (TypeCheckerT m) where
  ask = lift ask
  local = mapTypeCheckerT . local
