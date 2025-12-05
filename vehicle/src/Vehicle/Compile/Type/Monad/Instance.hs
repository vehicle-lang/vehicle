{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( TypeCheckerT,
    runTypeCheckerT,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State
  ( MonadState (..),
    StateT (..),
    gets,
    mapStateT,
    modify,
  )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Hashable (Hashable)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface.Normalise (NormalisableBuiltin)
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Variable.Free.Context

--------------------------------------------------------------------------------
-- Implementation

type TypeCheckerTInternals builtin m =
  FreeContextT
    builtin
    (StateT (TypeCheckerState builtin) m)

clearFreshNamesInternal :: (Monad m) => TypeCheckerTInternals builtin m ()
clearFreshNamesInternal =
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = 0, ..})

getFreshNameInternal :: (Monad m) => Type builtin -> TypeCheckerTInternals builtin2 m Name
getFreshNameInternal _typ = do
  nameID <- gets freshNameState
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = nameID + 1, ..})
  return $ layoutAsText $ "_x" <> pretty nameID

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT builtin m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals builtin m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT ::
  (Monad m) =>
  FreeCtx builtin ->
  TypeCheckerState builtin ->
  TypeCheckerT builtin m a ->
  m (a, TypeCheckerState builtin)
runTypeCheckerT freeCtx metaCtx (TypeCheckerT e) =
  runStateT (runFreeContextT freeCtx e) metaCtx

mapTypeCheckerT ::
  (m (a, TypeCheckerState builtin) -> n (b, TypeCheckerState builtin)) ->
  TypeCheckerT builtin m a ->
  TypeCheckerT builtin n b
mapTypeCheckerT f m = TypeCheckerT (mapFreeContextT (mapStateT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance (PrintableBuiltin builtin, MonadCompile m) => MonadFreeContext builtin (TypeCheckerT builtin m) where
  addDeclEntryToContext entry = TypeCheckerT . addDeclEntryToContext entry . unTypeCheckerT
  getFreeCtx = TypeCheckerT . getFreeCtx
  getDeclEntry proxy = TypeCheckerT . getDeclEntry proxy

instance (Eq builtin, Hashable builtin, PrintableBuiltin builtin, NormalisableBuiltin builtin, TypableBuiltin builtin, MonadCompile m) => MonadTypeChecker builtin (TypeCheckerT builtin m) where
  getTypeCheckerState = TypeCheckerT get
  modifyTypeCheckerState f = TypeCheckerT $ modify f
  getFreshName typ = TypeCheckerT $ getFreshNameInternal typ
  clearFreshNames _ = TypeCheckerT clearFreshNamesInternal

--------------------------------------------------------------------------------
-- Monad inheritance laws that TypeCheckerT satisfies

instance MonadTrans (TypeCheckerT builtin) where
  lift = TypeCheckerT . lift . lift

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
  enterCompilerPass = lift . enterCompilerPass
  exitCompilerPass = lift exitCompilerPass

instance (MonadReader r m) => MonadReader r (TypeCheckerT builtin m) where
  ask = lift ask
  local = mapTypeCheckerT . local
