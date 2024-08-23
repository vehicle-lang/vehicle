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
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Interface (BuiltinHasStandardData)

--------------------------------------------------------------------------------
-- Implementation

type TypeCheckerTInternals builtin m =
  FreeContextT
    builtin
    ( ReaderT
        (InstanceCandidateDatabase builtin)
        (StateT (TypeCheckerState builtin) m)
    )

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

newtype TypeCheckerT builtin m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals builtin m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT ::
  (Monad m) =>
  FreeCtx builtin ->
  InstanceCandidateDatabase builtin ->
  TypeCheckerState builtin ->
  TypeCheckerT builtin m a ->
  m (a, TypeCheckerState builtin)
runTypeCheckerT freeCtx instanceDatabase metaCtx (TypeCheckerT e) =
  runStateT (runReaderT (runFreeContextT freeCtx e) instanceDatabase) metaCtx

mapTypeCheckerT ::
  (m (a, TypeCheckerState builtin) -> n (b, TypeCheckerState builtin)) ->
  TypeCheckerT builtin m a ->
  TypeCheckerT builtin n b
mapTypeCheckerT f m = TypeCheckerT (mapFreeContextT (mapReaderT (mapStateT f)) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance (PrintableBuiltin builtin, BuiltinHasStandardData builtin, MonadCompile m) => MonadFreeContext builtin (TypeCheckerT builtin m) where
  addDeclEntryToContext entry = TypeCheckerT . addDeclEntryToContext entry . unTypeCheckerT
  getFreeCtx = TypeCheckerT . getFreeCtx
  hideStdLibDecls p f = TypeCheckerT . hideStdLibDecls p f . unTypeCheckerT
  getHiddenStdLibDecl p = TypeCheckerT . getHiddenStdLibDecl p

instance (PrintableBuiltin builtin, BuiltinHasStandardData builtin, NormalisableBuiltin builtin, MonadCompile m) => MonadTypeChecker builtin (TypeCheckerT builtin m) where
  getMetaState = TypeCheckerT get
  modifyMetaCtx f = TypeCheckerT $ modify f
  getFreshName typ = TypeCheckerT $ getFreshNameInternal typ
  clearFreshNames _ = TypeCheckerT clearFreshNamesInternal
  getInstanceCandidates = TypeCheckerT ask

--------------------------------------------------------------------------------
-- Monad inheritance laws that TypeCheckerT satisfies

instance MonadTrans (TypeCheckerT builtin) where
  lift = TypeCheckerT . lift . lift . lift

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

instance (MonadReader r m) => MonadReader r (TypeCheckerT builtin m) where
  ask = lift ask
  local = mapTypeCheckerT . local
