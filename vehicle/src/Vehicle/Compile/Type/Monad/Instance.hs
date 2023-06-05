{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( TypeCheckerT,
    runTypeCheckerT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
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
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Expr.DeBruijn (Ix)

--------------------------------------------------------------------------------
-- Implementation

type TypeCheckerTInternals types m =
  ReaderT (TypingDeclCtx types) (StateT (TypeCheckerState types) m)

clearFreshNamesInternal :: (Monad m) => TypeCheckerTInternals builtin m ()
clearFreshNamesInternal =
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = 0, ..})

getFreshNameInternal :: (Monad m) => Type Ix builtin -> TypeCheckerTInternals builtin2 m Name
getFreshNameInternal _typ = do
  nameID <- gets freshNameState
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = nameID + 1, ..})
  return $ layoutAsText $ "_x" <> pretty nameID

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT types m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals types m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT :: (Monad m) => TypingDeclCtx types -> TypeCheckerState types -> TypeCheckerT types m a -> m (a, TypeCheckerState types)
runTypeCheckerT declCtx metaCtx (TypeCheckerT e) =
  runStateT (runReaderT e declCtx) metaCtx

mapTypeCheckerT ::
  (m (a, TypeCheckerState types) -> n (b, TypeCheckerState types)) ->
  TypeCheckerT types m a ->
  TypeCheckerT types n b
mapTypeCheckerT f m = TypeCheckerT (mapReaderT (mapStateT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance (PrintableBuiltin types, MonadCompile m) => MonadNorm types (TypeCheckerT types m) where
  getDeclSubstitution = TypeCheckerT $ asks typingDeclCtxToNormDeclCtx

  getMetaSubstitution = TypeCheckerT (gets currentSubstitution)

instance (PrintableBuiltin types, MonadCompile m) => MonadTypeChecker types (TypeCheckerT types m) where
  getDeclContext = TypeCheckerT ask
  addDeclContext d s = TypeCheckerT $ local (addToTypingDeclCtx d) (unTypeCheckerT s)
  getMetaState = TypeCheckerT get
  modifyMetaCtx f = TypeCheckerT $ modify f
  getFreshName typ = TypeCheckerT $ getFreshNameInternal typ
  clearFreshNames _ = TypeCheckerT clearFreshNamesInternal

--------------------------------------------------------------------------------
-- Monad inheritance laws that TypeCheckerT satisfies

instance MonadTrans (TypeCheckerT types) where
  lift = TypeCheckerT . lift . lift

instance (MonadError e m) => MonadError e (TypeCheckerT types m) where
  throwError = lift . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance (MonadLogger m) => MonadLogger (TypeCheckerT types m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (MonadReader r m) => MonadReader r (TypeCheckerT types m) where
  ask = lift ask
  local = mapTypeCheckerT . local
