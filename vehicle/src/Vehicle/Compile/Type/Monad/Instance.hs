{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Type.Monad.Instance
  ( module Vehicle.Compile.Type.Monad.Class,
    runTypeCheckerT,
    toNormalisationDeclContext,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State
  ( MonadState (..),
    StateT (..),
    evalStateT,
    gets,
    mapStateT,
    modify,
  )
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (Bifunctor (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Compile.Type.VariableContext

--------------------------------------------------------------------------------
-- Implementation

-- | State for generating fresh names.
type FreshNameState = Int

type TypeCheckerTInternals m =
  ReaderT TypingDeclCtx (StateT (TypingMetaCtx, FreshNameState) m)

getFreshNameInternal :: Monad m => CheckedType -> TypeCheckerTInternals m Name
getFreshNameInternal _typ = do
  (metaCtx, nameID) <- get
  put (metaCtx, nameID + 1)
  return $ layoutAsText $ "_x" <> pretty nameID

--------------------------------------------------------------------------------
-- The type-checking monad

newtype TypeCheckerT m a = TypeCheckerT
  { unTypeCheckerT :: TypeCheckerTInternals m a
  }
  deriving (Functor, Applicative, Monad)

runTypeCheckerT :: Monad m => TypingDeclCtx -> TypeCheckerT m a -> m a
runTypeCheckerT declCtx (TypeCheckerT e) =
  evalStateT (runReaderT e declCtx) (emptyTypingMetaCtx, 0)

mapTypeCheckerT ::
  (m (a, (TypingMetaCtx, FreshNameState)) -> n (b, (TypingMetaCtx, FreshNameState))) ->
  TypeCheckerT m a ->
  TypeCheckerT n b
mapTypeCheckerT f m = TypeCheckerT (mapReaderT (mapStateT f) (unTypeCheckerT m))

--------------------------------------------------------------------------------
-- Instances that TypeCheckerT satisfies

instance MonadCompile m => MonadTypeChecker (TypeCheckerT m) where
  getDeclContext = TypeCheckerT ask
  addDeclContext d s = TypeCheckerT $ local (addToDeclCtx d) (unTypeCheckerT s)
  getMetaCtx = TypeCheckerT $ gets fst
  modifyMetaCtx f = TypeCheckerT $ modify (first f)
  getFreshName typ = TypeCheckerT $ getFreshNameInternal typ
  clearFreshNames = TypeCheckerT $ modify (second (const 0))

instance MonadTrans TypeCheckerT where
  lift = TypeCheckerT . lift . lift

instance MonadError e m => MonadError e (TypeCheckerT m) where
  throwError = lift . throwError
  catchError m f = TypeCheckerT (catchError (unTypeCheckerT m) (unTypeCheckerT . f))

instance MonadLogger m => MonadLogger (TypeCheckerT m) where
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance MonadReader r m => MonadReader r (TypeCheckerT m) where
  ask = lift ask
  local = mapTypeCheckerT . local
