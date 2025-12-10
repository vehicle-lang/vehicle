module Vehicle.Data.Variable.Free.Context.Class where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Generic
import Vehicle.Data.Variable.Bound.Context.Name.Instance
import Vehicle.Data.Variable.Bound.Context.Tensor.Instance

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (PrintableBuiltin builtin, MonadLogger m) => MonadFreeContext builtin m where
  -- | Adds a new decl to the free variable context.
  addDeclEntryToContext :: FreeCtxEntry builtin -> m a -> m a

  -- | Returns the current free variable context (may be expensive, so should use `getDeclEntry` in preference)
  getFreeCtx :: Proxy builtin -> m (FreeCtx builtin)

  -- | Lookup the decl for a particular identifier entry
  getDeclEntry :: Proxy builtin -> Identifier -> m (FreeCtxEntry builtin)

instance (Monoid w, MonadFreeContext builtin m) => MonadFreeContext builtin (WriterT w m) where
  addDeclEntryToContext = mapWriterT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ReaderT w m) where
  addDeclEntryToContext = mapReaderT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (StateT w m) where
  addDeclEntryToContext = mapStateT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (BoundContextT builtin2 m) where
  addDeclEntryToContext = mapBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (TensorBoundContextT m) where
  addDeclEntryToContext = mapTensorBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (NameBoundContextT m) where
  addDeclEntryToContext = mapNameBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (IdentityT m) where
  addDeclEntryToContext = mapIdentityT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (SupplyT s m) where
  addDeclEntryToContext = mapSupplyT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ExceptT s m) where
  addDeclEntryToContext = mapExceptT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (MaybeT m) where
  addDeclEntryToContext = mapMaybeT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy

--------------------------------------------------------------------------------
-- Operations

getFreeEnv ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  m (FreeEnv builtin)
getFreeEnv = do
  ctx <- getFreeCtx (Proxy @builtin)
  return ctx

lookupIdentValue :: forall builtin m. (MonadFreeContext builtin m) => Identifier -> m (Value builtin)
lookupIdentValue ident = do
  decl <- getDeclEntry (Proxy @builtin) ident
  return $ case bodyOf decl of
    Just value -> value
    _ -> VFreeVar ident []
