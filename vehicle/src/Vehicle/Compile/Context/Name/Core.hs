module Vehicle.Compile.Context.Name.Core where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Control.Monad (void)
import Data.Proxy (Proxy (..))
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Context.Bound.Class
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Context.Bound.Instance (BoundContext, BoundContextT, runBoundContext, runBoundContextT, runFreshBoundContext, runFreshBoundContextT)
import Vehicle.Data.DeBruijn (Ix, Lv)
import Vehicle.Prelude

type MonadNameContext m = MonadBoundContext () m

type NameContextT m = BoundContextT () m

runNameContext :: NamedBoundCtx -> BoundContext () a -> a
runNameContext ctx = do
  let binderCtx = fmap (mkExplicitBinder ()) ctx
  runBoundContext binderCtx

runNameContextT :: (Monad m) => NamedBoundCtx -> NameContextT m a -> m a
runNameContextT ctx = do
  let binderCtx = fmap (mkExplicitBinder ()) ctx
  runBoundContextT binderCtx

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

ixToProperName :: (MonadNameContext m, HasCallStack) => Provenance -> Ix -> m Name
ixToProperName p ix = do
  ctx <- getNameContext
  case lookupIx ctx ix of
    Nothing -> varOutOfBounds "DeBruijn index" p ix ctx
    Just Nothing -> return "_"
    Just (Just name) -> return name

lvToProperName :: (MonadNameContext m, HasCallStack) => Provenance -> Lv -> m Name
lvToProperName p lv = do
  ctx <- getNameContext
  case lookupLv ctx lv of
    Nothing -> varOutOfBounds "DeBruijn level" p lv ctx
    Just Nothing -> return "_"
    Just (Just name) -> return name

-- | Throw an |IndexOutOfBounds| error using an arbitrary var.
varOutOfBounds ::
  (MonadNameContext m, Pretty var, HasCallStack) =>
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
