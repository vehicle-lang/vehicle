{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Expr.Boolean where

import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import Vehicle.Prelude (Pretty (..), (<+>))

--------------------------------------------------------------------------------
-- BooleanExpr

data BooleanExpr a
  = Conjunct (BooleanExpr a) (BooleanExpr a)
  | Disjunct (BooleanExpr a) (BooleanExpr a)
  | Query a
  deriving (Show, Functor, Foldable, Generic)

instance (ToJSON a) => ToJSON (BooleanExpr a)

instance (FromJSON a) => FromJSON (BooleanExpr a)

instance (Pretty a) => Pretty (BooleanExpr a) where
  pretty = \case
    Query x -> pretty x
    Disjunct x y -> "Or[" <> pretty x <+> pretty y <> "]"
    Conjunct x y -> "And[" <> pretty x <+> pretty y <> "]"

flatten :: BooleanExpr a -> [a]
flatten = \case
  Query a -> [a]
  Disjunct e1 e2 -> flatten e1 <> flatten e2
  Conjunct e1 e2 -> flatten e1 <> flatten e2

evaluate :: (a -> Bool) -> BooleanExpr a -> Bool
evaluate f = \case
  Query v -> f v
  Disjunct x y -> evaluate f x || evaluate f y
  Conjunct x y -> evaluate f x && evaluate f y

--------------------------------------------------------------------------------
-- Triviality

-- | A single individual query for a verifier. Is either a trivial query or
-- holds arbitrary data.
data MaybeTrivial a
  = Trivial Bool
  | NonTrivial a
  deriving (Show, Generic, Foldable, Traversable)

instance (ToJSON a) => ToJSON (MaybeTrivial a)

instance (FromJSON a) => FromJSON (MaybeTrivial a)

instance Functor MaybeTrivial where
  fmap f = \case
    Trivial s -> Trivial s
    NonTrivial s -> NonTrivial (f s)

instance (Pretty a) => Pretty (MaybeTrivial a) where
  pretty = \case
    Trivial True -> "TriviallyTrue"
    Trivial False -> "TriviallyFalse"
    NonTrivial a -> pretty a

maybeTrivialToEither :: MaybeTrivial a -> Either Bool a
maybeTrivialToEither = \case
  Trivial b -> Left b
  NonTrivial l -> Right l

isNonTrivial :: MaybeTrivial a -> Bool
isNonTrivial = \case
  Trivial {} -> False
  NonTrivial {} -> True

orTrivial :: MaybeTrivial a -> MaybeTrivial a -> (a -> a -> a) -> MaybeTrivial a
orTrivial x y f = case (x, y) of
  (Trivial False, _) -> y
  (_, Trivial False) -> x
  (Trivial True, _) -> Trivial True
  (_, Trivial True) -> Trivial True
  (NonTrivial a, NonTrivial b) -> NonTrivial $ f a b

andTrivial :: MaybeTrivial a -> MaybeTrivial a -> (a -> a -> a) -> MaybeTrivial a
andTrivial x y f = case (x, y) of
  (Trivial False, _) -> Trivial False
  (_, Trivial False) -> Trivial False
  (Trivial True, _) -> y
  (_, Trivial True) -> x
  (NonTrivial a, NonTrivial b) -> NonTrivial $ f a b

eliminateTrivialConjunctions :: ConjunctAll (MaybeTrivial a) -> MaybeTrivial (ConjunctAll a)
eliminateTrivialConjunctions conjunction = do
  let conjuncts = NonEmpty.toList (unConjunctAll conjunction)
  let (trivial, nonTrivial) = partitionEithers (fmap maybeTrivialToEither conjuncts)
  let triviallyFalse = not (and trivial)
  if triviallyFalse
    then Trivial False
    else case nonTrivial of
      [] -> Trivial True
      x : xs -> NonTrivial $ ConjunctAll (x :| xs)

--------------------------------------------------------------------------------
-- Disjunctions

newtype DisjunctAll a = DisjunctAll
  { unDisjunctAll :: NonEmpty a
  }
  deriving (Show, Generic, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

instance (ToJSON a) => ToJSON (DisjunctAll a)

instance (FromJSON a) => FromJSON (DisjunctAll a)

eliminateTrivialDisjunctions :: DisjunctAll (MaybeTrivial a) -> MaybeTrivial (DisjunctAll a)
eliminateTrivialDisjunctions disjunction = do
  let disjuncts = NonEmpty.toList (unDisjunctAll disjunction)
  let (trivial, nonTrivial) = partitionEithers (fmap maybeTrivialToEither disjuncts)
  let triviallyTrue = or trivial
  if triviallyTrue
    then Trivial True
    else case nonTrivial of
      [] -> Trivial False
      x : xs -> NonTrivial $ DisjunctAll (x :| xs)

--------------------------------------------------------------------------------
-- Conjunctions

newtype ConjunctAll a = ConjunctAll
  { unConjunctAll :: NonEmpty a
  }
  deriving (Show, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

conjunctsToList :: ConjunctAll a -> [a]
conjunctsToList = NonEmpty.toList . unConjunctAll
