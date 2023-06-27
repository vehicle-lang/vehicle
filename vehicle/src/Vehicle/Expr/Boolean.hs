{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Expr.Boolean where

import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import Vehicle.Prelude (Doc, Pretty (..), indent, line, prependList)

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
-- BooleanExpr

data BooleanExpr a
  = Conjunct (BooleanExpr a) (BooleanExpr a)
  | Disjunct (BooleanExpr a) (BooleanExpr a)
  | Query a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (ToJSON a) => ToJSON (BooleanExpr a)

instance (FromJSON a) => FromJSON (BooleanExpr a)

instance (Pretty a) => Pretty (BooleanExpr a) where
  pretty = prettyBoolExpr pretty

prettyBoolExpr :: (a -> Doc b) -> (BooleanExpr a -> Doc b)
prettyBoolExpr f = \case
  Query x -> f x
  Disjunct x y -> "Or" <> line <> indent 2 (prettyBoolExpr f x <> line <> prettyBoolExpr f y)
  Conjunct x y -> "And" <> line <> indent 2 (prettyBoolExpr f x <> line <> prettyBoolExpr f y)

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

eliminateTrivialAtoms :: BooleanExpr (MaybeTrivial a) -> MaybeTrivial (BooleanExpr a)
eliminateTrivialAtoms = \case
  Query (NonTrivial a) -> NonTrivial (Query a)
  Query (Trivial b) -> Trivial b
  Conjunct a b -> andTrivial Conjunct (eliminateTrivialAtoms a) (eliminateTrivialAtoms b)
  Disjunct a b -> orTrivial Disjunct (eliminateTrivialAtoms a) (eliminateTrivialAtoms b)

concatBooleanExpr :: BooleanExpr (BooleanExpr a) -> BooleanExpr a
concatBooleanExpr = \case
  Query a -> a
  Conjunct a b -> Conjunct (concatBooleanExpr a) (concatBooleanExpr b)
  Disjunct a b -> Disjunct (concatBooleanExpr a) (concatBooleanExpr b)

--------------------------------------------------------------------------------
-- Disjunctions

newtype DisjunctAll a = DisjunctAll
  { unDisjunctAll :: NonEmpty a
  }
  deriving (Show, Generic, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

instance (ToJSON a) => ToJSON (DisjunctAll a)

instance (FromJSON a) => FromJSON (DisjunctAll a)

instance (Pretty a) => Pretty (DisjunctAll a) where
  pretty x = "Or" <> pretty (unDisjunctAll x)

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

concatDisjuncts :: DisjunctAll (DisjunctAll a) -> DisjunctAll a
concatDisjuncts xs = DisjunctAll $ sconcat (coerce xs)

zipDisjuncts :: NonEmpty a -> DisjunctAll b -> DisjunctAll (a, b)
zipDisjuncts xs ys = DisjunctAll $ NonEmpty.zip xs (unDisjunctAll ys)

singletonDisjunct :: a -> DisjunctAll a
singletonDisjunct a = DisjunctAll [a]

--------------------------------------------------------------------------------
-- Conjunctions

newtype ConjunctAll a = ConjunctAll
  { unConjunctAll :: NonEmpty a
  }
  deriving (Show, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

instance (Pretty a) => Pretty (ConjunctAll a) where
  pretty x = "And" <> pretty (unConjunctAll x)

conjunctsToList :: ConjunctAll a -> [a]
conjunctsToList = NonEmpty.toList . unConjunctAll

concatConjuncts :: ConjunctAll (ConjunctAll a) -> ConjunctAll a
concatConjuncts xs = ConjunctAll $ sconcat (coerce xs)

prependConjunctions :: [a] -> ConjunctAll a -> ConjunctAll a
prependConjunctions xs ys = ConjunctAll $ prependList xs $ unConjunctAll ys

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
-- DNF

-- | A tree of expressions in disjunctive normal form.
type DNFTree a = MaybeTrivial (DisjunctAll (ConjunctAll a))

orDNF :: DNFTree a -> DNFTree a -> DNFTree a
orDNF = orTrivial (<>)

andDNF :: DNFTree a -> DNFTree a -> DNFTree a
andDNF = andTrivial $ \(DisjunctAll cs) (DisjunctAll ds) -> createAllConjunctions cs ds
  where
    createAllConjunctions cs ds =
      DisjunctAll $ NonEmpty.fromList [as <> bs | as <- NonEmpty.toList cs, bs <- NonEmpty.toList ds]

singletonDNF :: a -> DNFTree a
singletonDNF a = NonTrivial $ DisjunctAll [ConjunctAll [a]]

exprToDNF :: BooleanExpr a -> DNFTree a
exprToDNF = \case
  Query a -> singletonDNF a
  Conjunct a b -> andDNF (exprToDNF a) (exprToDNF b)
  Disjunct a b -> orDNF (exprToDNF a) (exprToDNF b)
