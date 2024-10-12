module Vehicle.Compile.Context.Free.Class where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Identity (IdentityT, mapIdentityT)
import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT (..), mapStateT)
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Data.Set (Set)
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Error (MonadCompile, lookupInFreeCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Builtin
import Vehicle.Data.Code.Value
import Vehicle.Libraries.StandardLibrary.Definitions

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (PrintableBuiltin builtin, MonadLogger m) => MonadFreeContext builtin m where
  -- | Adds a new decl to the free variable context.
  addDeclEntryToContext :: FreeCtxEntry builtin -> m a -> m a

  -- | Returns the current free variable context (with masked definitions excluded)
  getFreeCtx :: Proxy builtin -> m (FreeCtx builtin)

  -- | Temporarily hides the given standard library function so that it is not returned as part of `getFreeCtx`.
  -- Useful if you want to stop certain standard library functions from reducing.
  hideStdLibDecls :: Proxy builtin -> Set StdLibFunction -> m a -> m a

  -- | Returns the free context of all the currently hidden declarations.
  getHiddenStdLibDecl :: Proxy builtin -> StdLibFunction -> m (FreeCtxEntry builtin)

instance (Monoid w, MonadFreeContext builtin m) => MonadFreeContext builtin (WriterT w m) where
  addDeclEntryToContext = mapWriterT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapWriterT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ReaderT w m) where
  addDeclEntryToContext = mapReaderT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapReaderT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (StateT w m) where
  addDeclEntryToContext = mapStateT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapStateT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (BoundContextT builtin2 m) where
  addDeclEntryToContext = mapBoundContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapBoundContextT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (IdentityT m) where
  addDeclEntryToContext = mapIdentityT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapIdentityT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (SupplyT s m) where
  addDeclEntryToContext = mapSupplyT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapSupplyT . hideStdLibDecls p

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ExceptT s m) where
  addDeclEntryToContext = mapExceptT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getHiddenStdLibDecl p = lift . getHiddenStdLibDecl p
  hideStdLibDecls p = mapExceptT . hideStdLibDecls p

--------------------------------------------------------------------------------
-- Operations

getDeclEntry ::
  (MonadCompile m, MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (FreeCtxEntry builtin)
getDeclEntry proxy compilerPass ident = do
  ctx <- getFreeCtx proxy
  lookupInFreeCtx compilerPass ident ctx

getDeclType ::
  (MonadCompile m, MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (Type builtin)
getDeclType proxy compilerPass ident =
  typeOf . fst <$> getDeclEntry proxy compilerPass ident

getDecl ::
  (MonadCompile m, MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (WHNFDecl builtin)
getDecl proxy compilerPass ident =
  snd <$> getDeclEntry proxy compilerPass ident

getFreeEnv ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  m (WHNFFreeEnv builtin)
getFreeEnv = do
  ctx <- getFreeCtx (Proxy @builtin)
  return $ fmap snd ctx
