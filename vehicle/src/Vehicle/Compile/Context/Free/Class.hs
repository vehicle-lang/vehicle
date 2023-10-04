module Vehicle.Compile.Context.Free.Class where

import Control.Monad.Reader (ReaderT (..), mapReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Error (MonadCompile, lookupInFreeCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Data.BuiltinInterface (HasStandardData)

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (PrintableBuiltin builtin, HasStandardData builtin, MonadCompile m) => MonadFreeContext builtin m where
  addDeclToContext :: Decl Ix builtin -> m a -> m a
  getFreeCtx :: Proxy builtin -> m (FreeCtx builtin)

instance (Monoid w, MonadFreeContext builtin m) => MonadFreeContext builtin (WriterT w m) where
  addDeclToContext = mapWriterT . addDeclToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (ReaderT w m) where
  addDeclToContext = mapReaderT . addDeclToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (StateT w m) where
  addDeclToContext = mapStateT . addDeclToContext
  getFreeCtx = lift . getFreeCtx

instance (MonadFreeContext builtin m) => MonadFreeContext builtin (BoundContextT builtin m) where
  addDeclToContext = mapBoundContextT . addDeclToContext
  getFreeCtx = lift . getFreeCtx

--------------------------------------------------------------------------------
-- Operations

getDecl ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (Decl Ix builtin)
getDecl _ compilerPass ident =
  lookupInFreeCtx compilerPass ident =<< getFreeCtx (Proxy @builtin)

getDeclType ::
  (MonadFreeContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (Type Ix builtin)
getDeclType proxy compilerPass ident = do
  decl <- getDecl proxy compilerPass ident
  return $ typeOf decl
