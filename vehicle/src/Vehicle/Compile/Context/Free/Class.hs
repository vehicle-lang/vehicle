module Vehicle.Compile.Context.Free.Class where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Data.Vector.Internal.Check (HasCallStack)
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (PrintableBuiltin builtin, MonadLogger m) => MonadFreeContext builtin m where
  -- | Adds a new decl to the free variable context.
  addDeclEntryToContext :: FreeCtxEntry builtin -> m a -> m a

  -- | Returns the current free variable context
  getFreeCtx :: Proxy builtin -> m (FreeCtx builtin)

instance (Monoid w, MonadFreeContext builtin m) => MonadFreeContext builtin (WriterT w m) where
  addDeclEntryToContext = mapWriterT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ReaderT w m) where
  addDeclEntryToContext = mapReaderT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (StateT w m) where
  addDeclEntryToContext = mapStateT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (BoundContextT builtin2 m) where
  addDeclEntryToContext = mapBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (IdentityT m) where
  addDeclEntryToContext = mapIdentityT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (SupplyT s m) where
  addDeclEntryToContext = mapSupplyT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ExceptT s m) where
  addDeclEntryToContext = mapExceptT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (MaybeT m) where
  addDeclEntryToContext = mapMaybeT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx

--------------------------------------------------------------------------------
-- Operations

getDeclEntry ::
  (MonadLogger m, MonadFreeContext builtin m, HasCallStack) =>
  Proxy builtin ->
  Identifier ->
  m (FreeCtxEntry builtin)
getDeclEntry proxy ident = do
  ctx <- getFreeCtx proxy
  return $ lookupInFreeCtx ident ctx

getDeclType ::
  (MonadLogger m, MonadFreeContext builtin m, HasCallStack) =>
  Proxy builtin ->
  Identifier ->
  m (Type builtin)
getDeclType proxy ident =
  typeOf . fst <$> getDeclEntry proxy ident

getDecl ::
  (MonadLogger m, MonadFreeContext builtin m, HasCallStack) =>
  Proxy builtin ->
  Identifier ->
  m (VDecl builtin)
getDecl proxy ident =
  snd <$> getDeclEntry proxy ident

getFreeEnv ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  m (FreeEnv builtin)
getFreeEnv = do
  ctx <- getFreeCtx (Proxy @builtin)
  return $ fmap snd ctx
