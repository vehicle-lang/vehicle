{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( TypeCheckerT,
    runTypeCheckerT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State
  ( MonadState (..),
    StateT (..),
    gets,
    mapStateT,
    modify,
  )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
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
  StateT (TypeCheckerState builtin) m

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT builtin m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals builtin m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT ::
  (Monad m) =>
  TypeCheckerState builtin ->
  TypeCheckerT builtin m a ->
  m (a, TypeCheckerState builtin)
runTypeCheckerT initialState (TypeCheckerT action) =
  runStateT action initialState

mapTypeCheckerT ::
  (m (a, TypeCheckerState builtin) -> n (b, TypeCheckerState builtin)) ->
  TypeCheckerT builtin m a ->
  TypeCheckerT builtin n b
mapTypeCheckerT f m = TypeCheckerT (mapStateT f (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance (PrintableBuiltin builtin, MonadCompile m) => MonadFreeContext builtin (TypeCheckerT builtin m) where
  addDeclEntryToContext decl action = TypeCheckerT $ do
    modify $ \typeCheckerState ->
      typeCheckerState {currentFreeEnv = Map.insert (identifierOf decl) decl (currentFreeEnv typeCheckerState)}
    unTypeCheckerT action

  getFreeCtx _proxy = TypeCheckerT $ gets currentFreeEnv

  getDeclEntry _proxy ident = TypeCheckerT $ do
    gets (lookupInFreeCtx ident . currentFreeEnv)

instance (TypableBuiltin builtin, MonadCompile m) => MonadTypeChecker builtin (TypeCheckerT builtin m) where
  getTypeCheckerState = TypeCheckerT get
  modifyTypeCheckerState f = TypeCheckerT $ modify f

--------------------------------------------------------------------------------
-- Monad inheritance laws that TypeCheckerT satisfies

instance MonadTrans (TypeCheckerT builtin) where
  lift = TypeCheckerT . lift

instance (MonadError e m) => MonadError e (TypeCheckerT builtin m) where
  throwError = lift . throwError
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
