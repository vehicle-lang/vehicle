module Vehicle.Data.Variable.Bound.Context.Class where

import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (Monad m) => MonadBoundContext expr m where
  addBinderToContext :: GenericBinder expr -> m a -> m a
  getBoundCtx :: Proxy expr -> m (BoundCtx expr)

instance (Monoid w, MonadBoundContext expr m) => MonadBoundContext expr (WriterT w m) where
  addBinderToContext = mapWriterT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

instance (MonadBoundContext expr m) => MonadBoundContext expr (ReaderT w m) where
  addBinderToContext = mapReaderT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

instance (MonadBoundContext expr m) => MonadBoundContext expr (ExceptT e m) where
  addBinderToContext = mapExceptT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

--------------------------------------------------------------------------------
-- Operations

addBindersToContext ::
  (MonadBoundContext expr m) =>
  [GenericBinder expr] ->
  m a ->
  m a
addBindersToContext binders fn = foldr addBinderToContext fn binders

getCurrentLv ::
  (MonadBoundContext expr m) =>
  Proxy expr ->
  m Lv
getCurrentLv p = boundCtxLv <$> getBoundCtx p

--------------------------------------------------------------------------------
-- Fresh names

-- | State for generating fresh names.
type FreshNameState = Int

freshName :: Int -> Name
freshName i = "_x" <> layoutAsText (pretty i)

-- TODO not currently sound.
getFreshName ::
  forall expr m.
  (MonadBoundContext expr m) =>
  expr ->
  m Name
getFreshName _t = do
  boundCtx <- getBoundCtx (Proxy @expr)
  return $ freshName (length boundCtx)

getBinderNameOrFreshName :: (MonadBoundContext expr m) => Maybe Name -> expr -> m Name
getBinderNameOrFreshName piName typ = case piName of
  Just x -> return x
  Nothing -> getFreshName typ

getNamedBoundCtx :: (MonadBoundContext expr m) => Proxy expr -> m NamedBoundCtx
getNamedBoundCtx p = toNamedBoundCtx <$> getBoundCtx p
