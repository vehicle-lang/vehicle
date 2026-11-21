module Vehicle.Data.MaybeTrivial where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Prelude (Doc, Pretty (..))

--------------------------------------------------------------------------------
-- Triviality

-- | A single individual query for a solver. Is either a trivial query or
-- holds arbitrary data.
data MaybeTrivial a
  = Trivial !Bool
  | NonTrivial !a
  deriving stock (Show, Generic, Foldable, Traversable)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Functor MaybeTrivial where
  fmap f = \case
    Trivial s -> Trivial s
    NonTrivial s -> NonTrivial (f s)

prettyMaybeTrivial :: (a -> Doc b) -> MaybeTrivial a -> Doc b
prettyMaybeTrivial f = \case
  Trivial True -> "True"
  Trivial False -> "False"
  NonTrivial a -> f a

instance (Pretty a) => Pretty (MaybeTrivial a) where
  pretty = prettyMaybeTrivial pretty

trivialElim :: (Bool -> b) -> (a -> b) -> MaybeTrivial a -> b
trivialElim f g = \case
  Trivial b -> f b
  NonTrivial x -> g x

bindMaybeTrivial :: MaybeTrivial a -> (a -> MaybeTrivial b) -> MaybeTrivial b
bindMaybeTrivial (NonTrivial x) f = f x
bindMaybeTrivial (Trivial b) _ = Trivial b

-- | Monadic version of `bindMaybeTrivial`: runs the continuation only if the
-- result of the first computation is non-trivial, otherwise short-circuits
-- with the trivial result.
--
-- IMPORTANT: this should not be used in contexts where the results are
-- either conjuncted or disjuncted together, as it simply returns the trivial
-- result rather than combining them together appropriately!
bindNonTrivial :: (Monad m) => m (MaybeTrivial a) -> (a -> m (MaybeTrivial b)) -> m (MaybeTrivial b)
bindNonTrivial action f = do
  result <- action
  case result of
    Trivial b -> return (Trivial b)
    NonTrivial a -> f a
{-# INLINE bindNonTrivial #-}

infixl 1 `bindNonTrivial`

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

orTrivialM :: (Monad m) => (a -> a -> m a) -> MaybeTrivial a -> MaybeTrivial a -> m (MaybeTrivial a)
orTrivialM f x y = case (x, y) of
  (Trivial False, _) -> return y
  (_, Trivial False) -> return x
  (Trivial True, _) -> return $ Trivial True
  (_, Trivial True) -> return $ Trivial True
  (NonTrivial a, NonTrivial b) -> NonTrivial <$> f a b

orTrivial :: (a -> a -> a) -> MaybeTrivial a -> MaybeTrivial a -> MaybeTrivial a
orTrivial f x y = runIdentity (orTrivialM (\u v -> return $ f u v) x y)

andTrivialM :: (Monad m) => (a -> a -> m a) -> MaybeTrivial a -> MaybeTrivial a -> m (MaybeTrivial a)
andTrivialM f x y = case (x, y) of
  (Trivial False, _) -> return $ Trivial False
  (_, Trivial False) -> return $ Trivial False
  (Trivial True, _) -> return y
  (_, Trivial True) -> return x
  (NonTrivial a, NonTrivial b) -> NonTrivial <$> f a b

andTrivial :: (a -> a -> a) -> MaybeTrivial a -> MaybeTrivial a -> MaybeTrivial a
andTrivial f x y = runIdentity (andTrivialM (\u v -> return $ f u v) x y)
