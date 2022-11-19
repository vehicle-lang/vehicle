module Vehicle.Verify.Specification
  ( Query(..)
  , traverseQuery
  , evaluateQuery
  , QueryID
  , PropertyExpr(..)
  , disjunctQueries
  , traversePropertyExpr
  , foldMPropertyExpr
  , fmapNumberedPropertyExpr
  , evaluatePropertyExpr
  , propertyExprToList
  , NegationStatus
  , Property(..)
  , traverseProperty
  , Specification(..)
  , traverseSpecification
  ) where

import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import GHC.Generics (Generic)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Negation

-- | Tracks whether or not a given result should be negated.
type NegationStatus = Bool

--------------------------------------------------------------------------------
-- Query

-- | A single individual query for a verifier. Is either a trivial query or
-- holds arbitrary data.
data Query a
  = Trivial Bool
  | NonTrivial a
  deriving (Generic)

instance ToJSON a => ToJSON (Query a)
instance FromJSON a => FromJSON (Query a)

instance Functor Query where
  fmap f = \case
    Trivial    s -> Trivial s
    NonTrivial s -> NonTrivial (f s)

instance Pretty a => Pretty (Query a) where
  pretty = \case
    Trivial True  -> "TriviallyTrue"
    Trivial False -> "TriviallyFalse"
    NonTrivial a  -> pretty a

traverseQuery :: Monad m
              => (a -> m b)
              -> Query a
              -> m (Query b)
traverseQuery f = \case
  Trivial b    -> return $ Trivial b
  NonTrivial a -> NonTrivial <$> f a

getTrivial :: Query a -> Either Bool (Query a)
getTrivial = \case
  Trivial b -> Left b
  l         -> Right l

evaluateQuery :: NegationStatus -> (a -> Bool) -> Query a -> Bool
evaluateQuery negated f q = negated `xor` case q of
  Trivial    b -> b
  NonTrivial a -> f a

--------------------------------------------------------------------------------
-- Property expression

-- | Properties may have arbitrary boolean structure above queries.
--
-- e.g. (forall ....) or (exists (...) and (forall ...)).
--
-- This type captures this boolean structure, and is parameterised by the type
-- of data stored at the position of each query.
data PropertyExpr a
  = Query NegationStatus (Query a)
  | Disjunct (PropertyExpr a) (PropertyExpr a)
  | Conjunct (PropertyExpr a) (PropertyExpr a)
  deriving (Generic)

instance ToJSON a => ToJSON (PropertyExpr a)
instance FromJSON a => FromJSON (PropertyExpr a)

instance Functor PropertyExpr where
  fmap f = \case
    Query n x    -> Query n (fmap f x)
    Disjunct x y -> Disjunct (fmap f x) (fmap f y)
    Conjunct x y -> Conjunct (fmap f x) (fmap f y)

instance Pretty a => Pretty (PropertyExpr a) where
  pretty = \case
    Query n x    -> pretty x <+> "(negate =" <+> pretty n <> ")"
    Disjunct x y -> "And[" <> pretty x <+> pretty y <> "]"
    Conjunct x y -> "Or["  <> pretty x <+> pretty y <> "]"

-- | Lazily folds over the property expression. Avoids evaluating parts
-- of the expression that are not needed.
foldMPropertyExpr :: Monad m
                  => (a -> m b)
                  -> (b -> Bool)
                  -> PropertyExpr a
                  -> m (NegationStatus, Query b)
foldMPropertyExpr f c = \case
  Query n x -> do
    r <- traverseQuery f x
    return (n, r)

  Disjunct x y -> do
    result@(negated, x') <- foldMPropertyExpr f c x
    if evaluateQuery negated c x'
      then return result
      else foldMPropertyExpr f c y

  Conjunct x y -> do
    result@(negated, x') <- foldMPropertyExpr f c x
    if not (evaluateQuery negated c x')
      then return result
      else foldMPropertyExpr f c y

traversePropertyExpr :: Monad m
                     => (a -> m b)
                     -> PropertyExpr a
                     -> m (PropertyExpr b)
traversePropertyExpr f = \case
  Query n x    -> Query n <$> traverseQuery f x
  Disjunct x y -> Disjunct <$> traversePropertyExpr f x <*> traversePropertyExpr f y
  Conjunct x y -> Conjunct <$> traversePropertyExpr f x <*> traversePropertyExpr f y

-- | The number of an individual query within a `PropertyExpr` when traversed
-- depth-first.
type QueryID = Int

fmapNumberedPropertyExpr :: forall a b . ((QueryID, a) -> b)
                         -> PropertyExpr a
                         -> PropertyExpr b
fmapNumberedPropertyExpr f s =
  runSupply (traversePropertyExpr f' s) [1..]
  where
    f' :: a -> Supply Int b
    f' x = do
      queryID <- demand
      lift $ return $ f (queryID, x)

disjunctQueries :: NegationStatus -> [Query a] -> PropertyExpr a
disjunctQueries negated queries = do
  let (trivial, nonTrivial) = partitionEithers (fmap getTrivial queries)
  let mkQuery = Query negated

  if not negated then do
    let triviallyTrue = or trivial
    if triviallyTrue
      then mkQuery $ Trivial True
      else foldr Disjunct (mkQuery $ Trivial False) (fmap mkQuery nonTrivial)

  else do
    let triviallyFalse = not (and trivial)
    if triviallyFalse
      then mkQuery $ Trivial True
      else foldr Conjunct (mkQuery $ Trivial True) (fmap mkQuery nonTrivial)

evaluatePropertyExpr :: (a -> Bool) -> PropertyExpr a -> Bool
evaluatePropertyExpr f = \case
  Query negated x -> evaluateQuery negated f x `xor` negated
  Disjunct x y    -> evaluatePropertyExpr f x || evaluatePropertyExpr f y
  Conjunct x y    -> evaluatePropertyExpr f x && evaluatePropertyExpr f y

propertyExprToList :: PropertyExpr a -> [(NegationStatus, a)]
propertyExprToList = \case
  Query n a       -> case a of
    Trivial{}    -> []
    NonTrivial b -> [(n, b)]
  Disjunct e1 e2 -> propertyExprToList e1 <> propertyExprToList e2
  Conjunct e1 e2 -> propertyExprToList e1 <> propertyExprToList e2

--------------------------------------------------------------------------------
-- Property

data Property queryData
  = SingleProperty (PropertyExpr queryData)
  -- ^ A single property.
  -- The bool denotes if the property is negated.
  -- The queries are implicitly disjuncted.
  | MultiProperty [Property queryData]
  -- ^ Multiple sub-properties e.g. generated by a `foreach`.
  -- They are implicitly conjuncted.
  deriving (Functor)

traverseProperty :: Monad m
                 => (a -> m b)
                 -> Property a
                 -> m (Property b)
traverseProperty f = \case
  SingleProperty p -> SingleProperty <$> traversePropertyExpr f p
  MultiProperty ps -> MultiProperty <$> traverse (traverseProperty f) ps

--------------------------------------------------------------------------------
-- Specification

-- | A compiled specification, parameterised by the data stored at each query.
newtype Specification queryData
  = Specification [(Name, Property queryData)]


traverseSpecification :: Monad m
                 => (a -> m b)
                 -> Specification a
                 -> m (Specification b)
traverseSpecification f (Specification properties) =
  Specification <$> traverse (\(n, q) -> (n,) <$> traverseProperty f q) properties
