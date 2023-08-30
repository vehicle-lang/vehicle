module Vehicle.Compile.Context.Bound.Class where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Data (Proxy (..))
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Error (MonadCompile, lookupIxInBoundCtx, lookupLvInBoundCtx)
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Normalise.Quote qualified as Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (Monad m, NormalisableBuiltin builtin) => MonadBoundContext builtin m where
  addBinderToContext :: Binder Ix builtin -> m a -> m a
  getBoundCtx :: Proxy builtin -> m (BoundCtx builtin)

instance (Monoid w, MonadBoundContext builtin m) => MonadBoundContext builtin (WriterT w m) where
  addBinderToContext = mapWriterT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

instance (MonadBoundContext builtin m) => MonadBoundContext builtin (ReaderT w m) where
  addBinderToContext = mapReaderT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

instance (MonadBoundContext builtin m) => MonadBoundContext builtin (StateT w m) where
  addBinderToContext = mapStateT . addBinderToContext
  getBoundCtx = lift . getBoundCtx

--------------------------------------------------------------------------------
-- Operations

addBindersToContext ::
  (MonadBoundContext builtin m) =>
  [Binder Ix builtin] ->
  m a ->
  m a
addBindersToContext binders fn = foldr addBinderToContext fn binders

getBoundVarByIx ::
  forall builtin m.
  (MonadBoundContext builtin m, MonadCompile m) =>
  Proxy builtin ->
  CompilerPass ->
  Ix ->
  m (Binder Ix builtin)
getBoundVarByIx _ compilerPass ix =
  lookupIxInBoundCtx compilerPass ix =<< getBoundCtx (Proxy @builtin)

getBoundVarByLv ::
  forall builtin m.
  (MonadBoundContext builtin m, MonadCompile m) =>
  Proxy builtin ->
  CompilerPass ->
  Lv ->
  m (Binder Ix builtin)
getBoundVarByLv _ compilerPass lv =
  lookupLvInBoundCtx compilerPass lv =<< getBoundCtx (Proxy @builtin)

unnormalise ::
  forall builtin m.
  (MonadBoundContext builtin m) =>
  Value builtin ->
  m (Expr Ix builtin)
unnormalise e = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  return $ Quote.unnormalise (Lv $ length boundCtx) e

--------------------------------------------------------------------------------
-- Fresh names

-- | State for generating fresh names.
type FreshNameState = Int

-- TODO not currently sound.
getFreshName ::
  forall builtin m.
  (MonadBoundContext builtin m) =>
  Expr Ix builtin ->
  m Name
getFreshName _t = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  return $ "_x" <> layoutAsText (pretty (length boundCtx))

getBinderNameOrFreshName :: (MonadBoundContext builtin m) => Maybe Name -> Type Ix builtin -> m Name
getBinderNameOrFreshName piName typ = case piName of
  Just x -> return x
  Nothing -> getFreshName typ

{-
getFreshNameInternal :: (Monad m) => Type Ix builtin -> TypeCheckerTInternals builtin2 m Name
getFreshNameInternal _typ = do
  nameID <- gets freshNameState
  modify (\TypeCheckerState {..} -> TypeCheckerState {freshNameState = nameID + 1, ..})
  return $ layoutAsText $ "_x" <> pretty nameID
-}

piBinderToLamBinder ::
  (MonadBoundContext builtin m) =>
  Binder Ix builtin ->
  m (Binder Ix builtin)
piBinderToLamBinder binder@(Binder p _ v r t) = do
  binderName <- case nameOf binder of
    Just name -> return name
    Nothing -> getFreshName (typeOf binder)

  let displayForm = BinderDisplayForm (OnlyName binderName) True
  return $ Binder p displayForm v r t
