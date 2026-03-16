{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Code.BooleanExpr where

import Control.DeepSeq (NFData)
import Control.Monad.Identity (Identity (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Semigroup (Semigroup (..))
import GHC.Generics (Generic)
import Vehicle.Data.MaybeTrivial
import Vehicle.Prelude (Pretty (..), lineIndent, nonEmptyCartesianProductM, prependList)

--------------------------------------------------------------------------------
-- Disjunctions

newtype DisjunctAll a = DisjunctAll
  { unDisjunctAll :: NonEmpty a
  }
  deriving (Show, Eq, Ord, Generic, Semigroup, Functor, Applicative, Monad, Foldable, Traversable)

instance (NFData a) => NFData (DisjunctAll a)

instance (ToJSON a) => ToJSON (DisjunctAll a)

instance (FromJSON a) => FromJSON (DisjunctAll a)

instance (Pretty a) => Pretty (DisjunctAll a) where
  pretty x = "Or" <> lineIndent (pretty (unDisjunctAll x))

eliminateTrivialDisjunctions :: DisjunctAll (MaybeTrivial a) -> MaybeTrivial (DisjunctAll a)
eliminateTrivialDisjunctions disjunction = do
  let disjuncts = NonEmpty.toList (unDisjunctAll disjunction)
  let (bools, nonTrivialValues) = partitionEithers (fmap maybeTrivialToEither disjuncts)
  let triviallyTrue = or bools
  if triviallyTrue
    then Trivial True
    else case nonTrivialValues of
      [] -> Trivial False
      x : xs -> NonTrivial $ DisjunctAll (x :| xs)

disjunctDisjuncts :: DisjunctAll (DisjunctAll a) -> DisjunctAll a
disjunctDisjuncts xs = DisjunctAll $ sconcat (coerce xs)

disjunctsToList :: DisjunctAll a -> [a]
disjunctsToList = NonEmpty.toList . unDisjunctAll

conjunctDisjunctsM :: (Monad m) => (a -> b -> m c) -> DisjunctAll a -> DisjunctAll b -> m (DisjunctAll c)
conjunctDisjunctsM f xs ys = DisjunctAll <$> nonEmptyCartesianProductM f (unDisjunctAll xs) (unDisjunctAll ys)

conjunctDisjuncts :: (a -> b -> c) -> DisjunctAll a -> DisjunctAll b -> DisjunctAll c
conjunctDisjuncts f xs ys = runIdentity $ conjunctDisjunctsM (\u v -> return $ f u v) xs ys

--------------------------------------------------------------------------------
-- Conjunctions

newtype ConjunctAll a = ConjunctAll
  { unConjunctAll :: NonEmpty a
  }
  deriving (Show, Eq, Ord, Semigroup, Functor, Applicative, Monad, Foldable, Traversable, Generic)

instance (NFData a) => NFData (ConjunctAll a)

instance (Pretty a) => Pretty (ConjunctAll a) where
  pretty x = "And" <> lineIndent (pretty (unConjunctAll x))

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
  let (bools, nonTrivialValues) = partitionEithers (fmap maybeTrivialToEither conjuncts)
  let triviallyFalse = not (and bools)
  if triviallyFalse
    then Trivial False
    else case nonTrivialValues of
      [] -> Trivial True
      x : xs -> NonTrivial $ ConjunctAll (x :| xs)

collapseTrivialConjunctions :: ConjunctAll (MaybeTrivial (ConjunctAll a)) -> MaybeTrivial (ConjunctAll a)
collapseTrivialConjunctions = fmap concatConjuncts . eliminateTrivialConjunctions

--------------------------------------------------------------------------------
-- BooleanExpr

-- TODO make this use `conjunctExprs` and `disjunctExprs` as smart constructors.
data BooleanExpr a
  = Conjunct !(ConjunctAll (BooleanExpr a))
  | Disjunct !(DisjunctAll (BooleanExpr a))
  | Query !a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance (NFData a) => NFData (BooleanExpr a)

instance (ToJSON a) => ToJSON (BooleanExpr a)

instance (FromJSON a) => FromJSON (BooleanExpr a)

instance (Pretty a) => Pretty (BooleanExpr a) where
  pretty = \case
    Query x -> pretty x
    Disjunct xs -> pretty xs
    Conjunct xs -> pretty xs

conjunctExprs :: ConjunctAll (BooleanExpr a) -> BooleanExpr a
conjunctExprs = \case
  ConjunctAll (e :| []) -> e
  es -> Conjunct es

disjunctExprs :: DisjunctAll (BooleanExpr a) -> BooleanExpr a
disjunctExprs = \case
  DisjunctAll (e :| []) -> e
  es -> Disjunct es

evaluate :: (a -> Bool) -> BooleanExpr a -> Bool
evaluate f = \case
  Query v -> f v
  Disjunct xs -> any (evaluate f) xs
  Conjunct xs -> all (evaluate f) xs

eliminateTrivialAtoms :: BooleanExpr (MaybeTrivial a) -> MaybeTrivial (BooleanExpr a)
eliminateTrivialAtoms = \case
  Query (NonTrivial a) -> NonTrivial (Query a)
  Query (Trivial b) -> Trivial b
  Conjunct xs -> conjunctExprs <$> eliminateTrivialConjunctions (fmap eliminateTrivialAtoms xs)
  Disjunct xs -> disjunctExprs <$> eliminateTrivialDisjunctions (fmap eliminateTrivialAtoms xs)

filterTrivialAtoms :: MaybeTrivial (BooleanExpr (MaybeTrivial a)) -> MaybeTrivial (BooleanExpr a)
filterTrivialAtoms = flattenTrivial . fmap eliminateTrivialAtoms

flattenBoolExpr :: BooleanExpr (BooleanExpr a) -> BooleanExpr a
flattenBoolExpr = \case
  Query x -> x
  Conjunct xs -> conjunctExprs $ fmap flattenBoolExpr xs
  Disjunct xs -> disjunctExprs $ fmap flattenBoolExpr xs

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
