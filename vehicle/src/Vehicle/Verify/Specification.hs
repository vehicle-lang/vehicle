module Vehicle.Verify.Specification
  ( QueryMetaData (..),
    QueryText,
    evaluateQuery,
    isNonTrivial,
    QueryID,
    QuerySet (..),
    BoolProperty,
    traverseBoolProperty,
    evaluateBoolPropertyM,
    Property (..),
    traverseProperty,
    Specification (..),
    traverseSpecification,
    specificationPropertyNames,
    VerificationPlan (..),
    VerificationQueries,
    MetaNetwork,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Vehicle.Expr.Boolean
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Syntax.AST (Name)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Query meta data

data QueryMetaData = QueryData
  { metaNetwork :: MetaNetwork,
    userVar :: UserVarReconstructionInfo
  }
  deriving (Show, Generic)

instance ToJSON QueryMetaData

instance FromJSON QueryMetaData

instance Pretty QueryMetaData where
  pretty (QueryData metaNetwork _userVar) =
    "Meta-network:" <+> pretty metaNetwork

-- | The number of an individual query within a `BoolProperty` when traversed
-- depth-first.
type QueryID = Int

--------------------------------------------------------------------------------
-- Query

evaluateQuery :: QueryNegationStatus -> (a -> Bool) -> MaybeTrivial a -> Bool
evaluateQuery negated f q =
  negated `xor` case q of
    Trivial b -> b
    NonTrivial a -> f a

--------------------------------------------------------------------------------
-- Query set

data QuerySet a = QuerySet
  { negated :: QueryNegationStatus,
    queries :: MaybeTrivial (DisjunctAll a)
  }
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (QuerySet a)

instance FromJSON a => FromJSON (QuerySet a)

evaluateQuerySetM ::
  Monad m =>
  (a -> m b) ->
  (b -> Bool) ->
  QuerySet a ->
  m (QueryNegationStatus, MaybeTrivial b)
evaluateQuerySetM f c (QuerySet negated queries) = case queries of
  Trivial b -> return (negated, Trivial b)
  NonTrivial disjuncts -> do
    result <- evaluateDisjunctionM f c disjuncts
    return (negated, NonTrivial result)

evaluateDisjunctionM ::
  forall m a b.
  Monad m =>
  (a -> m b) ->
  (b -> Bool) ->
  DisjunctAll a ->
  m b
evaluateDisjunctionM f c (DisjunctAll ys) = go ys
  where
    go :: Monad m => NonEmpty a -> m b
    go (x :| []) = f x
    go (x :| y : xs) = do
      r <- f x
      if c r
        then return r
        else go (y :| xs)

--------------------------------------------------------------------------------
-- Property expression

-- | Properties may have arbitrary boolean structure above queries.
--
-- e.g. (forall ....) or (exists (...) and (forall ...)).
--
-- This type captures this boolean structure, and is parameterised by the type
-- of data stored at the position of each query.
type BoolProperty a = BooleanExpr (QuerySet a)

-- | Lazily folds over the property expression. Avoids evaluating parts
-- of the expression that are not needed.
evaluateBoolPropertyM ::
  Monad m =>
  (a -> m b) ->
  (b -> Bool) ->
  BoolProperty a ->
  m (QueryNegationStatus, MaybeTrivial b)
evaluateBoolPropertyM f c = \case
  Query qs -> evaluateQuerySetM f c qs
  Disjunct x y -> do
    result@(negated, x') <- evaluateBoolPropertyM f c x
    if evaluateQuery negated c x'
      then return result
      else evaluateBoolPropertyM f c y
  Conjunct x y -> do
    result@(negated, x') <- evaluateBoolPropertyM f c x
    if not (evaluateQuery negated c x')
      then return result
      else evaluateBoolPropertyM f c y

traverseBoolProperty ::
  Monad m =>
  (a -> m b) ->
  BoolProperty a ->
  m (BoolProperty b)
traverseBoolProperty f = \case
  Query qs -> Query <$> traverse f qs
  Disjunct x y -> Disjunct <$> traverseBoolProperty f x <*> traverseBoolProperty f y
  Conjunct x y -> Conjunct <$> traverseBoolProperty f x <*> traverseBoolProperty f y

--------------------------------------------------------------------------------
-- Property

data Property queryData
  = -- | A single boolean property.
    SingleProperty (BoolProperty queryData)
  | -- | Multiple nested boolean properties e.g. generated by a `foreach`.
    -- They are implicitly conjuncted.
    MultiProperty [Property queryData]
  deriving (Show, Functor, Generic)

instance ToJSON queryData => ToJSON (Property queryData)

instance FromJSON queryData => FromJSON (Property queryData)

traverseProperty ::
  Monad m =>
  (a -> m b) ->
  Property a ->
  m (Property b)
traverseProperty f = \case
  SingleProperty p -> SingleProperty <$> traverseBoolProperty f p
  MultiProperty ps -> MultiProperty <$> traverse (traverseProperty f) ps

--------------------------------------------------------------------------------
-- Specification

-- | A compiled specification, parameterised by the data stored at each query.
newtype Specification queryData
  = Specification [(Name, Property queryData)]
  deriving (Show, Generic, Functor)

instance ToJSON queryData => ToJSON (Specification queryData)

instance FromJSON queryData => FromJSON (Specification queryData)

specificationPropertyNames :: Specification a -> PropertyNames
specificationPropertyNames (Specification properties) = fmap fst properties

traverseSpecification ::
  Monad m =>
  (a -> m b) ->
  Specification a ->
  m (Specification b)
traverseSpecification f (Specification properties) =
  Specification <$> traverse (\(n, q) -> (n,) <$> traverseProperty f q) properties

data VerificationPlan = VerificationPlan
  { specificationPlan :: Specification QueryMetaData,
    resourceIntegrityInfo :: ResourcesIntegrityInfo
  }
  deriving (Generic)

instance ToJSON VerificationPlan

instance FromJSON VerificationPlan

type VerificationQueries = Specification QueryText
