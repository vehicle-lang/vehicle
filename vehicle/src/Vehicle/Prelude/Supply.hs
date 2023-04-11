{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Supply
  ( MonadSupply (..),
    SupplyT,
    runSupplyT,
    Supply,
    runSupply,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader (..), ReaderT)
import Control.Monad.State (MonadState (..), StateT, evalStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT)

class (Monad m) => MonadSupply s m where
  demand :: m s

newtype SupplyT s m a = SupplyT
  { unsupplyT :: StateT [s] m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans)

runSupplyT :: (Monad m) => SupplyT s m a -> [s] -> m a
runSupplyT (SupplyT m) = evalStateT m

type Supply s a = SupplyT s Identity a

runSupply :: Supply s a -> [s] -> a
runSupply m s = runIdentity $ runSupplyT m s

instance (Monad m) => MonadSupply s (SupplyT s m) where
  demand = SupplyT $ do
    supply <- get
    case supply of
      [] -> error "runSupplyT was not provided with an infinite list"
      x : xs -> do put xs; return x

instance (MonadSupply t m) => MonadSupply t (StateT s m) where
  demand = lift demand

instance (MonadSupply t m) => MonadSupply t (ReaderT s m) where
  demand = lift demand

instance (Monoid w, MonadSupply t m) => MonadSupply t (WriterT w m) where
  demand = lift demand

instance (MonadSupply t m) => MonadSupply t (ExceptT e m) where
  demand = lift demand

instance (MonadError e m) => MonadError e (SupplyT s m) where
  throwError = lift . throwError
  catchError m f = SupplyT (catchError (unsupplyT m) (unsupplyT . f))

instance (MonadReader e m) => MonadReader e (SupplyT s m) where
  ask = lift ask
  local f x = SupplyT $ local f $ unsupplyT x

instance (MonadState e m) => MonadState e (SupplyT s m) where
  get = lift get
  put = lift . put
