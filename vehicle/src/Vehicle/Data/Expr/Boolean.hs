{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Expr.Boolean where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import Vehicle.Prelude (Pretty (..), cartesianProduct, indent, line, prependList)

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
    Trivial True -> "TriviallyTrue"
    Trivial False -> "TriviallyFalse"
    NonTrivial a -> pretty a

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
-- BooleanExpr

data BooleanExpr a
  = Conjunct !(ConjunctAll (BooleanExpr a))
  | Disjunct !(DisjunctAll (BooleanExpr a))
  | Query !a
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (NFData a) => NFData (BooleanExpr a)

instance (ToJSON a) => ToJSON (BooleanExpr a)

instance (FromJSON a) => FromJSON (BooleanExpr a)

instance (Pretty a) => Pretty (BooleanExpr a) where
  pretty = \case
    Query x -> pretty x
    Disjunct xs -> pretty xs
    Conjunct xs -> pretty xs

evaluate :: (a -> Bool) -> BooleanExpr a -> Bool
evaluate f = \case
  Query v -> f v
  Disjunct xs -> any (evaluate f) xs
  Conjunct xs -> all (evaluate f) xs

eliminateTrivialAtoms :: BooleanExpr (MaybeTrivial a) -> MaybeTrivial (BooleanExpr a)
eliminateTrivialAtoms = \case
  Query (NonTrivial a) -> NonTrivial (Query a)
  Query (Trivial b) -> Trivial b
  Conjunct xs -> Conjunct <$> eliminateTrivialConjunctions (fmap eliminateTrivialAtoms xs)
  Disjunct xs -> Disjunct <$> eliminateTrivialDisjunctions (fmap eliminateTrivialAtoms xs)

filterTrivialAtoms :: MaybeTrivial (BooleanExpr (MaybeTrivial a)) -> MaybeTrivial (BooleanExpr a)
filterTrivialAtoms = flattenTrivial . fmap eliminateTrivialAtoms

conjunct :: [a] -> MaybeTrivial (BooleanExpr a)
conjunct [] = Trivial True
conjunct (x : xs) = NonTrivial $ Conjunct (ConjunctAll (fmap Query (x :| xs)))

andBoolExpr :: BooleanExpr a -> BooleanExpr a -> BooleanExpr a
andBoolExpr (Conjunct (ConjunctAll xs)) (Conjunct (ConjunctAll ys)) = Conjunct (ConjunctAll (xs <> ys))
andBoolExpr (Conjunct (ConjunctAll xs)) y = Conjunct (ConjunctAll ([y] <> xs))
andBoolExpr x (Conjunct (ConjunctAll ys)) = Conjunct (ConjunctAll ([x] <> ys))
andBoolExpr x y = Conjunct $ ConjunctAll [x, y]

orBoolExpr :: BooleanExpr a -> BooleanExpr a -> BooleanExpr a
orBoolExpr (Disjunct (DisjunctAll xs)) (Disjunct (DisjunctAll ys)) = Disjunct (DisjunctAll (xs <> ys))
orBoolExpr (Disjunct (DisjunctAll xs)) y = Disjunct (DisjunctAll (xs <> [y]))
orBoolExpr x (Disjunct (DisjunctAll ys)) = Disjunct (DisjunctAll ([x] <> ys))
orBoolExpr x y = Disjunct $ DisjunctAll [x, y]

--------------------------------------------------------------------------------
-- Disjunctions

newtype DisjunctAll a = DisjunctAll
  { unDisjunctAll :: NonEmpty a
  }
  deriving (Show, Generic, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

instance (NFData a) => NFData (DisjunctAll a)

instance (ToJSON a) => ToJSON (DisjunctAll a)

instance (FromJSON a) => FromJSON (DisjunctAll a)

instance (Pretty a) => Pretty (DisjunctAll a) where
  pretty x = "Or" <> line <> indent 2 (pretty (unDisjunctAll x))

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

disjunctDisjuncts :: DisjunctAll (DisjunctAll a) -> DisjunctAll a
disjunctDisjuncts xs = DisjunctAll $ sconcat (coerce xs)

zipDisjuncts :: NonEmpty a -> DisjunctAll b -> DisjunctAll (a, b)
zipDisjuncts xs ys = DisjunctAll $ NonEmpty.zip xs (unDisjunctAll ys)

singletonDisjunct :: a -> DisjunctAll a
singletonDisjunct a = DisjunctAll [a]

disjunctsToList :: DisjunctAll a -> [a]
disjunctsToList = NonEmpty.toList . unDisjunctAll

conjunctDisjuncts :: (a -> b -> c) -> DisjunctAll a -> DisjunctAll b -> DisjunctAll c
conjunctDisjuncts f xs ys =
  DisjunctAll $ NonEmpty.fromList (cartesianProduct f (disjunctsToList xs) (disjunctsToList ys))

--------------------------------------------------------------------------------
-- Conjunctions

newtype ConjunctAll a = ConjunctAll
  { unConjunctAll :: NonEmpty a
  }
  deriving (Show, Semigroup, Functor, Applicative, Monad, Foldable, Traversable, Generic)

instance (NFData a) => NFData (ConjunctAll a)

instance (Pretty a) => Pretty (ConjunctAll a) where
  pretty x = "And" <> line <> indent 2 (pretty (unConjunctAll x))

instance (ToJSON a) => ToJSON (ConjunctAll a)

instance (FromJSON a) => FromJSON (ConjunctAll a)

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
type DNFTree a = DisjunctAll (ConjunctAll a)

orDNF :: DNFTree a -> DNFTree a -> DNFTree a
orDNF = (<>)

andDNF :: DNFTree a -> DNFTree a -> DNFTree a
andDNF = conjunctDisjuncts (<>)

singletonDNF :: a -> DNFTree a
singletonDNF a = DisjunctAll [ConjunctAll [a]]

exprToDNF :: BooleanExpr a -> DNFTree a
exprToDNF = \case
  Query a -> singletonDNF a
  Conjunct xs -> foldr1 andDNF (fmap exprToDNF xs)
  Disjunct xs -> foldr1 orDNF (fmap exprToDNF xs)
