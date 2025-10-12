module Vehicle.Data.MaybeTrivial where

import Control.DeepSeq (NFData)
import Control.Monad (ap)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Prelude (MonadStdIO (..), Pretty (..))
import Vehicle.Prelude.Supply

--------------------------------------------------------------------------------
-- Triviality

-- | A single individual query for a verifier. Is either a trivial query or
-- holds arbitrary data.
data MaybeTrivial a
  = Trivial !Bool
  | NonTrivial !a
  deriving (Show, Generic, Foldable, Traversable)

instance (NFData a) => NFData (MaybeTrivial a)

instance (ToJSON a) => ToJSON (MaybeTrivial a)

instance (FromJSON a) => FromJSON (MaybeTrivial a)

instance Functor MaybeTrivial where
  fmap f = \case
    Trivial s -> Trivial s
    NonTrivial s -> NonTrivial (f s)

instance (Pretty a) => Pretty (MaybeTrivial a) where
  pretty = \case
    Trivial True -> "True"
    Trivial False -> "False"
    NonTrivial a -> pretty a

trivialElim :: (Bool -> b) -> (a -> b) -> MaybeTrivial a -> b
trivialElim f g = \case
  Trivial b -> f b
  NonTrivial x -> g x

bindMaybeTrivial :: MaybeTrivial a -> (a -> MaybeTrivial b) -> MaybeTrivial b
bindMaybeTrivial (NonTrivial x) f = f x
bindMaybeTrivial (Trivial b) _ = Trivial b

flattenTrivial :: MaybeTrivial (MaybeTrivial a) -> MaybeTrivial a
flattenTrivial x = bindMaybeTrivial x id

maybeTrivialToEither :: MaybeTrivial a -> Either Bool a
maybeTrivialToEither = \case
  Trivial b -> Left b
  NonTrivial l -> Right l

isNonTrivial :: MaybeTrivial a -> Bool
isNonTrivial = \case
  Trivial {} -> False
  NonTrivial {} -> True

orTrivial :: (a -> a -> a) -> MaybeTrivial a -> MaybeTrivial a -> MaybeTrivial a
orTrivial f x y = case (x, y) of
  (Trivial False, _) -> y
  (_, Trivial False) -> x
  (Trivial True, _) -> Trivial True
  (_, Trivial True) -> Trivial True
  (NonTrivial a, NonTrivial b) -> NonTrivial $ f a b

andTrivial :: (a -> a -> a) -> MaybeTrivial a -> MaybeTrivial a -> MaybeTrivial a
andTrivial f x y = case (x, y) of
  (Trivial False, _) -> Trivial False
  (_, Trivial False) -> Trivial False
  (Trivial True, _) -> y
  (_, Trivial True) -> x
  (NonTrivial a, NonTrivial b) -> NonTrivial $ f a b

--------------------------------------------------------------------------------
-- Triviality typeclass

-- | IMPORTANT: this monad should not be used in contexts where the results are
-- either conjuncted or disjuncted together, as this monad will simply return
-- the trivial result rather than combine them together appropriately!
class (Monad m) => MonadMaybeTrivial m where
  trivial :: Bool -> m a
  nonTrivial :: a -> m a

--------------------------------------------------------------------------------
-- Triviality monad

newtype MaybeTrivialT m a = MaybeTrivialT
  { -- | IMPORTANT: this monad should not be used in contexts where the results are
    -- either conjuncted or disjuncted together, as this monad will simply return
    -- the trivial result rather than combine them together appropriately!
    runMaybeTrivialT :: m (MaybeTrivial a)
  }
  deriving (Functor)

instance (Monad m) => Applicative (MaybeTrivialT m) where
  pure = MaybeTrivialT . return . NonTrivial
  (<*>) = ap

instance (Monad m) => Monad (MaybeTrivialT m) where
  action >>= f = MaybeTrivialT $ do
    result <- runMaybeTrivialT action
    case result of
      Trivial b -> return (Trivial b)
      NonTrivial a -> runMaybeTrivialT (f a)

instance MonadTrans MaybeTrivialT where
  lift = MaybeTrivialT . fmap NonTrivial

instance (Monad m) => MonadMaybeTrivial (MaybeTrivialT m) where
  trivial = MaybeTrivialT . pure . Trivial
  nonTrivial = MaybeTrivialT . pure . NonTrivial

instance (MonadError e m) => MonadError e (MaybeTrivialT m) where
  throwError = lift . throwError
  catchError m f = MaybeTrivialT (catchError (runMaybeTrivialT m) (runMaybeTrivialT . f))

instance (MonadIO m) => MonadIO (MaybeTrivialT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (MaybeTrivialT m) where
  writeStdout = lift . writeStdout
  writeStderr = lift . writeStderr
  writeStdoutLn = lift . writeStdoutLn
  writeStderrLn = lift . writeStderrLn

instance (MonadSupply t m) => MonadSupply t (MaybeTrivialT m) where
  demand = lift demand
