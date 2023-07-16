{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Prelude.MonadContext where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Vehicle.Compile.Error (MonadCompile, lookupInDeclCtx, lookupIxInBoundCtx, lookupLvInBoundCtx)
import Vehicle.Compile.Normalise.Builtin (Normalisable)
import Vehicle.Compile.Normalise.NBE (defaultEvalOptions, eval, runNormT)
import Vehicle.Compile.Normalise.Quote qualified as Quote (unnormalise)
import Vehicle.Compile.Prelude hiding (getBoundCtx)
import Vehicle.Compile.Type.Core (TypingDeclCtxEntry (..), typingBoundContextToEnv)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Context monad class

type FullDeclCtx builtin = DeclCtx (GluedDecl builtin)

type FullBoundCtx builtin = BoundCtx (Binder Ix builtin)

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (Normalisable builtin, MonadCompile m) => MonadContext builtin m where
  addDeclToContext :: Decl Ix builtin -> m a -> m a
  addBinderToContext :: Binder Ix builtin -> m a -> m a
  getDeclCtx :: Proxy builtin -> m (FullDeclCtx builtin)
  getBoundCtx :: Proxy builtin -> m (FullBoundCtx builtin)

addBindersToContext ::
  (MonadContext builtin m) =>
  [Binder Ix builtin] ->
  m a ->
  m a
addBindersToContext binders fn = foldr addBinderToContext fn binders

getDecl ::
  forall builtin m.
  (MonadContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Identifier ->
  m (GluedDecl builtin)
getDecl _ compilerPass ident =
  lookupInDeclCtx compilerPass ident =<< getDeclCtx (Proxy @builtin)

getBoundVarByIx ::
  forall builtin m.
  (MonadContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Ix ->
  m (Binder Ix builtin)
getBoundVarByIx _ compilerPass ix =
  lookupIxInBoundCtx compilerPass ix =<< getBoundCtx (Proxy @builtin)

getBoundVarByLv ::
  forall builtin m.
  (MonadContext builtin m) =>
  Proxy builtin ->
  CompilerPass ->
  Lv ->
  m (Binder Ix builtin)
getBoundVarByLv _ compilerPass lv =
  lookupLvInBoundCtx compilerPass lv =<< getBoundCtx (Proxy @builtin)

normalise ::
  forall builtin m.
  (MonadContext builtin m) =>
  Expr Ix builtin ->
  m (Value builtin)
normalise e = do
  declCtx <- getDeclCtx (Proxy @builtin)
  boundCtx <- getBoundCtx (Proxy @builtin)
  let normDeclCtx = flip fmap declCtx $ \d ->
        TypingDeclCtxEntry
          { declAnns = annotationsOf d,
            declType = typeOf d,
            declBody = normalised <$> bodyOf d
          }

  let boundEnv = typingBoundContextToEnv boundCtx
  runNormT defaultEvalOptions normDeclCtx mempty (eval boundEnv e)

unnormalise ::
  forall builtin m.
  (MonadContext builtin m) =>
  Value builtin ->
  m (Expr Ix builtin)
unnormalise e = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  return $ Quote.unnormalise (Lv $ length boundCtx) e

--------------------------------------------------------------------------------
-- Fresh names

-- TODO not currently sound, unify with `freshNameState` in TypeCheckerMonad.
getFreshName ::
  forall builtin m.
  (MonadContext builtin m) =>
  Expr Ix builtin ->
  m Name
getFreshName _t = do
  boundCtx <- getBoundCtx (Proxy @builtin)
  return $ "_x" <> layoutAsText (pretty (length boundCtx))

piBinderToLamBinder :: (MonadContext builtin m) => Binder Ix builtin -> m (Binder Ix builtin)
piBinderToLamBinder binder@(Binder p _ v r t) = do
  binderName <- case nameOf binder of
    Just name -> return name
    Nothing -> getFreshName (typeOf binder)

  let displayForm = BinderDisplayForm (OnlyName binderName) True
  return $ Binder p displayForm v r t

--------------------------------------------------------------------------------
-- Context monad instance

newtype ContextT builtin m a = ContextT
  { uncontextT :: ReaderT (FullDeclCtx builtin, FullBoundCtx builtin) m a
  }
  deriving (Functor, Applicative, Monad)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runContextT :: (Monad m) => ContextT builtin m a -> m a
runContextT (ContextT contextFn) = runReaderT contextFn mempty

instance MonadTrans (ContextT builtin) where
  lift = ContextT . lift

instance (MonadLogger m) => MonadLogger (ContextT builtin m) where
  setCallDepth = ContextT . setCallDepth
  getCallDepth = ContextT getCallDepth
  incrCallDepth = ContextT incrCallDepth
  decrCallDepth = ContextT decrCallDepth
  getDebugLevel = ContextT getDebugLevel
  logMessage = ContextT . logMessage

instance (MonadError e m) => MonadError e (ContextT builtin m) where
  throwError = lift . throwError
  catchError m f = ContextT (catchError (uncontextT m) (uncontextT . f))

instance (Normalisable builtin, MonadCompile m) => MonadContext builtin (ContextT builtin m) where
  addDeclToContext decl cont = do
    gluedDecl <- traverse (\e -> Glued e <$> normalise e) decl
    ContextT $ do
      let updateCtx = first (Map.insert (identifierOf decl) gluedDecl)
      local updateCtx (uncontextT cont)

  addBinderToContext binder cont = ContextT $ do
    let updateCtx = second (binder :)
    local updateCtx (uncontextT cont)

  getDeclCtx _ = ContextT $ asks fst

  getBoundCtx _ = ContextT $ asks snd
