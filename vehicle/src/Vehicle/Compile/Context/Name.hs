module Vehicle.Compile.Context.Name where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Control.Monad (void)
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Bound.Class
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Context.Bound.Instance (BoundContext, BoundContextT, runBoundContext, runBoundContextT, runFreshBoundContext, runFreshBoundContextT)
import Vehicle.Data.DeBruijn (Ix, Lv)
import Vehicle.Prelude

type MonadNameContext m = MonadBoundContext () m

type NameContextT m = BoundContextT () m

runNameContext :: BoundCtx () -> BoundContext () a -> a
runNameContext = runBoundContext

runNameContextT :: (Monad m) => BoundCtx () -> NameContextT m a -> m a
runNameContextT = runBoundContextT

runFreshNameContext :: BoundContext () a -> a
runFreshNameContext = runFreshBoundContext (Proxy @())

runFreshNameContextT :: (Monad m) => NameContextT m a -> m a
runFreshNameContextT = runFreshBoundContextT (Proxy @())

addNameToContext :: (MonadNameContext m) => GenericBinder expr -> m a -> m a
addNameToContext binder = addBinderToContext (void binder)

getBinderContext :: (MonadNameContext m) => m (BoundCtx ())
getBinderContext = getBoundCtx (Proxy @())

getNameContext :: (MonadNameContext m) => m NamedBoundCtx
getNameContext = getNamedBoundCtx (Proxy @())

getBinderDepth :: (MonadNameContext m) => m Lv
getBinderDepth = getCurrentLv (Proxy @())

ixToProperName :: (MonadNameContext m) => Provenance -> Ix -> m Name
ixToProperName p ix = do
  ctx <- getNameContext
  case lookupIx ctx ix of
    Nothing -> varOutOfBounds "DeBruijn index" p ix (length ctx)
    Just Nothing -> return "_"
    Just (Just name) -> return name

lvToProperName :: (MonadNameContext m) => Provenance -> Lv -> m Name
lvToProperName p lv = do
  ctx <- getNameContext
  case lookupLv ctx lv of
    Nothing -> varOutOfBounds "DeBruijn level" p lv (length ctx)
    Just Nothing -> return "_"
    Just (Just name) -> return name

-- | Throw an |IndexOutOfBounds| error using an arbitrary var.
varOutOfBounds :: (MonadNameContext m, Pretty var) => Doc a -> Provenance -> var -> Int -> m a
varOutOfBounds varType p var ctxSize =
  developerError $
    "During descoping found"
      <+> varType
      <+> pretty var
      <+> "greater than current context size"
      <+> pretty ctxSize
      <+> parens (pretty p)
