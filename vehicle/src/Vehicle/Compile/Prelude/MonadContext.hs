{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Prelude.MonadContext where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy)
import Data.Map qualified as Map
import Vehicle.Compile.Error (MonadCompile, lookupInDeclCtx, lookupIxInBoundCtx, lookupLvInBoundCtx)
import Vehicle.Compile.Normalise.Builtin (Normalisable)
import Vehicle.Compile.Normalise.NBE (defaultEvalOptions, eval, runNormT)
import Vehicle.Compile.Normalise.Quote qualified as Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core (TypingDeclCtxEntry (..), typingBoundContextToEnv)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Context monad class

-- | A monad that is used to store the current context at a given point in a
-- program, i.e. what declarations and bound variables are in scope.
class (Normalisable builtin, MonadCompile m) => MonadContext builtin m where
  addDeclToContext :: Decl Ix builtin -> m a -> m a
  addBinderToContext :: Binder Ix builtin -> m a -> m a
  getDecl :: Proxy builtin -> CompilerPass -> Identifier -> m (GluedDecl builtin)
  getBoundVarByIx :: Proxy builtin -> CompilerPass -> Ix -> m (Binder Ix builtin)
  getBoundVarByLv :: Proxy builtin -> CompilerPass -> Lv -> m (Binder Ix builtin)
  normalise :: Expr Ix builtin -> m (Value builtin)
  unnormalise :: Value builtin -> m (Expr Ix builtin)

addBindersToContext ::
  (MonadContext builtin m) =>
  [Binder Ix builtin] ->
  m a ->
  m a
addBindersToContext binders fn = foldr addBinderToContext fn binders

--------------------------------------------------------------------------------
-- Context monad instance

type FullDeclCtx builtin = DeclCtx (GluedDecl builtin)

type FullBoundCtx builtin = BoundCtx (Binder Ix builtin)

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
  getDecl _ compilerPass ident = ContextT $ do
    declCtx <- asks fst
    lookupInDeclCtx compilerPass ident declCtx

  addDeclToContext decl cont = do
    gluedDecl <- traverse (\e -> Glued e <$> normalise e) decl
    ContextT $ do
      let updateCtx = first (Map.insert (identifierOf decl) gluedDecl)
      local updateCtx (uncontextT cont)

  getBoundVarByIx _ compilerPass ix = ContextT $ do
    boundCtx <- asks snd
    lookupIxInBoundCtx compilerPass ix boundCtx

  getBoundVarByLv _ compilerPass lv = ContextT $ do
    boundCtx <- asks snd
    lookupLvInBoundCtx compilerPass lv boundCtx

  addBinderToContext binder cont = ContextT $ do
    let updateCtx = second (binder :)
    local updateCtx (uncontextT cont)

  normalise e = ContextT $ do
    (declCtx, boundCtx) <- ask
    let normDeclCtx = flip fmap declCtx $ \d ->
          TypingDeclCtxEntry
            { declAnns = annotationsOf d,
              declType = typeOf d,
              declBody = normalised <$> bodyOf d
            }

    let boundEnv = typingBoundContextToEnv boundCtx
    runNormT defaultEvalOptions normDeclCtx mempty (eval boundEnv e)

  unnormalise e = ContextT $ do
    boundCtx <- asks snd
    return $ Quote.unnormalise (Lv $ length boundCtx) e
