{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( TypeCheckerT,
    runTypeCheckerT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict
  ( MonadState (..),
    StateT (..),
    gets,
    mapStateT,
    modify',
  )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT)
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Variable.Free.Context

--------------------------------------------------------------------------------
-- Implementation

type TypeCheckerTInternals builtin m =
  StateT (TypeCheckerState builtin) (ExceptT (TypingError builtin) m)

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT builtin m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals builtin m a
  }
  deriving newtype (Functor, Applicative, Monad)

runTypeCheckerT ::
  (Monad m) =>
  TypeCheckerState builtin ->
  TypeCheckerT builtin m a ->
  m (Either (TypingError builtin) (a, TypeCheckerState builtin))
runTypeCheckerT initialState (TypeCheckerT action) =
  runExceptT $ runStateT action initialState

mapTypeCheckerT ::
  ( m (Either (TypingError builtin) (a, TypeCheckerState builtin)) ->
    n (Either (TypingError builtin) (b, TypeCheckerState builtin))
  ) ->
  TypeCheckerT builtin m a ->
  TypeCheckerT builtin n b
mapTypeCheckerT f m = TypeCheckerT (mapStateT (mapExceptT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance (PrintableBuiltin builtin, MonadLogger m) => MonadFreeContext builtin (TypeCheckerT builtin m) where
  addDeclEntryToContext decl action = TypeCheckerT $ do
    modify' $ \typeCheckerState ->
      typeCheckerState {currentFreeCtx = Map.insert (identifierOf decl) decl (currentFreeCtx typeCheckerState)}
    unTypeCheckerT action

  getFreeCtx _proxy = TypeCheckerT $ gets currentFreeCtx

  getDeclEntry _proxy ident = TypeCheckerT $ do
    gets (lookupInFreeCtx ident . currentFreeCtx)

instance (TypableBuiltin builtin, MonadLogger m) => MonadTypeChecker builtin (TypeCheckerT builtin m) where
  getTypeCheckerState = TypeCheckerT get
  modifyTypeCheckerState f = TypeCheckerT $ modify' f

--------------------------------------------------------------------------------
-- Monad inheritance laws that TypeCheckerT satisfies

instance MonadTrans (TypeCheckerT builtin) where
  lift = TypeCheckerT . lift . lift

instance (Monad m) => MonadError (TypingError builtin) (TypeCheckerT builtin m) where
  throwError = TypeCheckerT . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance (MonadLogger m) => MonadLogger (TypeCheckerT builtin m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage
  logWarning = lift . logWarning
  runCompilerPass = mapTypeCheckerT . runCompilerPass
  runCompileDecl = mapTypeCheckerT . runCompileDecl

instance (MonadReader r m) => MonadReader r (TypeCheckerT builtin m) where
  ask = lift ask
  local = mapTypeCheckerT . local
