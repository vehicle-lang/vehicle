module Vehicle.Compile.Context.Free.Class where

import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Error (MonadCompile, lookupInFreeCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Data.BuiltinInterface (HasStandardData)
import Vehicle.Data.NormalisedExpr (WHNFDecl)

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (PrintableBuiltin builtin, HasStandardData builtin, MonadCompile m) => MonadFreeContext builtin m where
  addDeclEntryToContext :: (WHNFDecl builtin, Type Ix builtin) -> m a -> m a
  getFreeCtx :: Proxy builtin -> m (GenericFreeCtx (WHNFDecl builtin, Type Ix builtin))
  locallyAdjustCtx :: Proxy builtin -> (FreeCtx builtin -> FreeCtx builtin) -> m a -> m a

instance (Monoid w, MonadFreeContext builtin m) => MonadFreeContext builtin (WriterT w m) where
  addDeclEntryToContext = mapWriterT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapWriterT . locallyAdjustCtx p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ReaderT w m) where
  addDeclEntryToContext = mapReaderT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapReaderT . locallyAdjustCtx p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (StateT w m) where
  addDeclEntryToContext = mapStateT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapStateT . locallyAdjustCtx p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (BoundContextT builtin m) where
  addDeclEntryToContext = mapBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapBoundContextT . locallyAdjustCtx p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (IdentityT m) where
  addDeclEntryToContext = mapIdentityT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapIdentityT . locallyAdjustCtx p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (SupplyT s m) where
  addDeclEntryToContext = mapSupplyT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  locallyAdjustCtx p = mapSupplyT . locallyAdjustCtx p

--------------------------------------------------------------------------------
-- Operations

getDeclEntry ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (WHNFDecl builtin, Type Ix builtin)
getDeclEntry proxy compilerPass ident = do
  ctx <- getFreeCtx proxy
  lookupInFreeCtx compilerPass ident ctx

getDecl ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (WHNFDecl builtin)
getDecl proxy compilerPass ident =
  fst <$> getDeclEntry proxy compilerPass ident

getDeclType ::
  (MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (Type Ix builtin)
getDeclType proxy compilerPass ident =
  snd <$> getDeclEntry proxy compilerPass ident
