{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Context.Free.Instance where

import Control.Monad qualified as Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Control.Monad.State
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Vehicle.Compile.Context.Free.Class
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Error (MonadCompile, lookupInFreeCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Prelude.IO qualified as VIO

--------------------------------------------------------------------------------
-- Free variable context monad instantiation

newtype FreeContextT builtin m a = FreeContextT
  { unFreeContextT :: ReaderT (FreeCtx builtin, FreeCtx builtin) m a
  }
  deriving (Functor, Applicative, Monad)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreeContextT :: (Monad m) => FreeCtx builtin -> FreeContextT builtin m a -> m a
runFreeContextT ctx (FreeContextT contextFn) = runReaderT contextFn (ctx, mempty)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreshFreeContextT :: (Monad m) => Proxy builtin -> FreeContextT builtin m a -> m a
runFreshFreeContextT _ = runFreeContextT mempty

instance MonadTrans (FreeContextT builtin) where
  lift = FreeContextT . lift

mapFreeContextT ::
  (m a -> n b) ->
  FreeContextT builtin m a ->
  FreeContextT builtin n b
mapFreeContextT f m = FreeContextT (mapReaderT f (unFreeContextT m))

--------------------------------------------------------------------------------
-- Other monad preservation

instance (MonadLogger m) => MonadLogger (FreeContextT builtin m) where
  setCallDepth = FreeContextT . setCallDepth
  getCallDepth = FreeContextT getCallDepth
  incrCallDepth = FreeContextT incrCallDepth
  decrCallDepth = FreeContextT decrCallDepth
  getDebugLevel = FreeContextT getDebugLevel
  logMessage = FreeContextT . logMessage
  logWarning = FreeContextT . logWarning

instance (MonadError e m) => MonadError e (FreeContextT builtin m) where
  throwError = lift . throwError
  catchError m f = FreeContextT (catchError (unFreeContextT m) (unFreeContextT . f))

instance (MonadState s m) => MonadState s (FreeContextT builtin m) where
  get = lift get
  put = lift . put

instance (MonadReader s m) => MonadReader s (FreeContextT builtin m) where
  ask = lift ask
  local = mapFreeContextT . local

instance (MonadIO m) => MonadIO (FreeContextT builtin m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (FreeContextT builtin m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

--------------------------------------------------------------------------------
-- Context monad preservation

instance
  (PrintableBuiltin builtin, MonadCompile m) =>
  MonadFreeContext builtin (FreeContextT builtin m)
  where
  addDeclEntryToContext entry@(decl, _) cont = FreeContextT $ do
    let updateCtx = Map.insert (identifierOf decl) entry
    local (first updateCtx) (unFreeContextT cont)

  getFreeCtx _ =
    FreeContextT $
      asks fst

  hideStdLibDecls _ fns x =
    FreeContextT $
      local hideDecls (unFreeContextT x)
    where
      hideDecls (decls, hiddenDecls) = do
        Monad.unless (Map.null hiddenDecls) $ do
          developerError "Hiding stdlib-decls twice not allowed"
        let fnIdents = Set.map identifierOf fns
        let unhiddenDecls = alterKeys fnIdents (bimap convertToPostulate convertToPostulate) decls
        (unhiddenDecls, decls)

  getHiddenStdLibDecl _ fn = FreeContextT $ do
    hiddenDecls <- asks snd
    lookupInFreeCtx "lookupStdlibDef" (identifierOf fn) hiddenDecls
