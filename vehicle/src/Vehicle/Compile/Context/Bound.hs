module Vehicle.Compile.Context.Bound
  ( module X,
    prettyFriendlyInCtx,
    getBoundVarByIx,
    getBoundVarByLv,
    unnormalise,
    piBinderToLamBinder,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Context.Bound.Class as X
import Vehicle.Compile.Context.Bound.Core as X
import Vehicle.Compile.Context.Bound.Instance as X
import Vehicle.Compile.Error (MonadCompile, lookupIxInBoundCtx, lookupLvInBoundCtx)
import Vehicle.Compile.Normalise.Quote qualified as Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyFriendly, prettyFriendly)
import Vehicle.Data.Code.Value (Value)

prettyFriendlyInCtx ::
  (MonadBoundContext expr m, MonadLogger m, PrettyFriendly (Contextualised a NamedBoundCtx)) =>
  Proxy expr ->
  a ->
  m (Doc b)
prettyFriendlyInCtx p value = do
  ctx <- getNamedBoundCtx p
  return $ prettyFriendly (WithContext value ctx)

getBoundVarByIx ::
  forall expr m.
  (MonadBoundContext expr m, MonadCompile m) =>
  Proxy expr ->
  CompilerPass ->
  Ix ->
  m (GenericBinder expr)
getBoundVarByIx _ compilerPass ix =
  lookupIxInBoundCtx compilerPass ix <$> getBoundCtx (Proxy @expr)

getBoundVarByLv ::
  forall expr m.
  (MonadBoundContext expr m, MonadCompile m) =>
  Proxy expr ->
  CompilerPass ->
  Lv ->
  m (GenericBinder expr)
getBoundVarByLv _ compilerPass lv =
  lookupLvInBoundCtx compilerPass lv <$> getBoundCtx (Proxy @expr)

unnormalise ::
  forall expr m.
  (MonadBoundContext expr m, Show expr) =>
  Value expr ->
  m (Expr expr)
unnormalise e = do
  lv <- getCurrentLv (Proxy @expr)
  return $ Quote.unnormalise lv e

piBinderToLamBinder ::
  (MonadBoundContext (Expr builtin) m) =>
  Binder builtin ->
  m (Binder builtin)
piBinderToLamBinder binder@(Binder p _ v r t) = do
  binderName <- case nameOf binder of
    Just name -> return name
    Nothing -> getFreshName (typeOf binder)

  let displayForm = BinderDisplayForm (OnlyName binderName) True
  return $ Binder p displayForm v r t
