module Vehicle.Data.Variable.Bound.Context.Name.Class where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Writer
import Control.Monad.Writer.Strict qualified as Strict
import GHC.Stack (HasCallStack)
import Vehicle.Data.MaybeTrivial (MaybeTrivialT, mapMaybeTrivialT)
import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Index (Ix)
import Vehicle.Data.Variable.Bound.Level (Lv)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Readable name context

class (Monad m) => MonadReadableNameContext m where
  getNameContext :: (MonadReadableNameContext m) => m NamedBoundCtx

instance (Monoid w, MonadReadableNameContext m) => MonadReadableNameContext (WriterT w m) where
  getNameContext = lift getNameContext

instance (Monoid w, MonadReadableNameContext m) => MonadReadableNameContext (Strict.WriterT w m) where
  getNameContext = lift getNameContext

instance (MonadReadableNameContext m) => MonadReadableNameContext (ReaderT w m) where
  getNameContext = lift getNameContext

instance (MonadReadableNameContext m) => MonadReadableNameContext (ExceptT e m) where
  getNameContext = lift getNameContext

instance (MonadReadableNameContext m) => MonadReadableNameContext (SupplyT e m) where
  getNameContext = lift getNameContext

instance (MonadReadableNameContext m) => MonadReadableNameContext (StateT e m) where
  getNameContext = lift getNameContext

instance (MonadReadableNameContext m) => MonadReadableNameContext (MaybeTrivialT m) where
  getNameContext = lift getNameContext

getBinderDepth :: (MonadReadableNameContext m) => m Lv
getBinderDepth = boundCtxLv <$> getNameContext

ixToProperName :: (MonadReadableNameContext m, HasCallStack) => Provenance -> Ix -> m Name
ixToProperName p ix = do
  ctx <- getNameContext
  case lookupIx ctx ix of
    Nothing -> varOutOfBounds "DeBruijn index" p ix ctx
    Just Nothing -> return "_"
    Just (Just name) -> return name

lvToProperName :: (MonadReadableNameContext m, HasCallStack) => Provenance -> Lv -> m Name
lvToProperName p lv = do
  ctx <- getNameContext
  case lookupLv ctx lv of
    Nothing -> varOutOfBounds "DeBruijn level" p lv ctx
    Just Nothing -> return "_"
    Just (Just name) -> return name

-- | Throw an |IndexOutOfBounds| error using an arbitrary var.
varOutOfBounds ::
  (MonadReadableNameContext m, Pretty var, HasCallStack) =>
  Doc a ->
  Provenance ->
  var ->
  NamedBoundCtx ->
  m a
varOutOfBounds varType p var ctx =
  developerError $
    "During descoping found"
      <+> varType
      <+> pretty var
      <+> "greater than the size"
      <+> quotePretty (length ctx)
      <+> "of the current context"
      <+> pretty ctx
      <+> parens (pretty p)

--------------------------------------------------------------------------------
-- Writable name context

class (MonadReadableNameContext m) => MonadNameContext m where
  addNameToContext :: GenericBinder expr -> m a -> m a

instance (Monoid w, MonadNameContext m) => MonadNameContext (WriterT w m) where
  addNameToContext = mapWriterT . addNameToContext

instance (Monoid w, MonadNameContext m) => MonadNameContext (Strict.WriterT w m) where
  addNameToContext = Strict.mapWriterT . addNameToContext

instance (MonadNameContext m) => MonadNameContext (ReaderT w m) where
  addNameToContext = mapReaderT . addNameToContext

instance (MonadNameContext m) => MonadNameContext (ExceptT e m) where
  addNameToContext = mapExceptT . addNameToContext

instance (MonadNameContext m) => MonadNameContext (SupplyT e m) where
  addNameToContext = mapSupplyT . addNameToContext

instance (MonadNameContext m) => MonadNameContext (StateT e m) where
  addNameToContext = mapStateT . addNameToContext

instance (MonadNameContext m) => MonadNameContext (MaybeTrivialT m) where
  addNameToContext = mapMaybeTrivialT . addNameToContext
