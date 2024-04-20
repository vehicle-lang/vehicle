module Vehicle.Verify.Specification
  ( QueryMetaData (..),
    QueryText,
    isNonTrivial,
    QueryID,
    QuerySet (..),
    Property,
    traverseProperty,
    forQueryInProperty,
    propertySize,
    MultiProperty (..),
    multiPropertyAddresses,
    Specification (..),
    specificationPropertyNames,
    SpecificationCacheIndex (..),
    PropertyVerificationPlan (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Data.BooleanExpr
import Vehicle.Resource (ResourcesIntegrityInfo)
import Vehicle.Syntax.AST (Name)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Query meta data

data QueryMetaData = QueryMetaData
  { queryAddress :: !QueryAddress,
    metaNetwork :: !MetaNetwork,
    variableReconstruction :: !UserVariableReconstruction
  }
  deriving (Show, Generic)

instance NFData QueryMetaData

instance ToJSON QueryMetaData

instance FromJSON QueryMetaData

{-
instance Pretty QueryMetaData where
  pretty (QueryMetaData _ metaNetwork _userVar) =
    "Meta-network:" <+> pretty metaNetwork
-}
--------------------------------------------------------------------------------
-- Query set

data QuerySet a = QuerySet
  { negated :: !QuerySetNegationStatus,
    queries :: !(DisjunctAll a)
  }
  deriving (Show, Generic, Functor, Foldable, Traversable)

instance (NFData a) => NFData (QuerySet a)

instance (ToJSON a) => ToJSON (QuerySet a)

instance (FromJSON a) => FromJSON (QuerySet a)

traverseQuerySet ::
  (Monad m) =>
  (a -> m b) ->
  QuerySet a ->
  m (QuerySet b)
traverseQuerySet f QuerySet {..} = do
  queries' <- traverse f queries
  return $ QuerySet negated queries'

querySetSize :: QuerySet a -> Int
querySetSize QuerySet {..} = length queries

--------------------------------------------------------------------------------
-- Property expression

-- | Properties may have arbitrary boolean structure above queries.
--
-- e.g. (forall ....) or (exists (...) and (forall ...)).
--
-- This type captures this boolean structure, and is parameterised by the type
-- of data stored at the position of each query.
type Property a = MaybeTrivial (BooleanExpr (QuerySet a))

traverseProperty ::
  forall m a b.
  (Monad m) =>
  (a -> m b) ->
  Property a ->
  m (Property b)
traverseProperty f = traverse (traverse (traverseQuerySet f))

forQueryInProperty ::
  (Monad m) =>
  Property a ->
  (a -> m ()) ->
  m ()
forQueryInProperty p f = do
  _ <- traverseProperty f p
  return ()

propertySize :: Property a -> Int
propertySize = \case
  Trivial {} -> 0
  NonTrivial p -> sum (fmap querySetSize p)

--------------------------------------------------------------------------------
-- MultiProperty

-- | A multi-property is something that can be annotated with `@property`
-- annotation in the front-end, and recreates the possible nested vector
-- structure.
data MultiProperty property
  = -- | A single boolean property.
    SingleProperty PropertyAddress property
  | -- | Multiple nested boolean properties e.g. generated by a `foreach`.
    -- They are implicitly conjuncted.
    MultiProperty [MultiProperty property]
  deriving (Show, Functor, Generic)

instance (ToJSON property) => ToJSON (MultiProperty property)

instance (FromJSON property) => FromJSON (MultiProperty property)

multiPropertyAddresses :: MultiProperty () -> [PropertyAddress]
multiPropertyAddresses = \case
  SingleProperty address _ -> [address]
  MultiProperty ps -> concatMap multiPropertyAddresses ps

--------------------------------------------------------------------------------
-- Specification

-- | A compiled specification, parameterised by the data stored at each query.
newtype Specification property
  = Specification [(Name, MultiProperty property)]
  deriving (Show, Generic, Functor)

instance (ToJSON property) => ToJSON (Specification property)

instance (FromJSON property) => FromJSON (Specification property)

specificationPropertyNames :: Specification a -> PropertyNames
specificationPropertyNames (Specification properties) = fmap fst properties

--------------------------------------------------------------------------------
-- Verification plans

-- | The object that provides the required information to perform or check the
-- verification of an entire specification.
data SpecificationCacheIndex = SpecificationCacheIndex
  { resourcesIntegrityInfo :: ResourcesIntegrityInfo,
    properties :: [(Name, MultiProperty ())]
  }
  deriving (Generic)

instance ToJSON SpecificationCacheIndex

instance FromJSON SpecificationCacheIndex

-- | The object that provides the required information to perform the
-- verification of a single property within a specification.
newtype PropertyVerificationPlan = PropertyVerificationPlan
  { queryMetaData :: Property QueryMetaData
  }
  deriving (Generic)

instance ToJSON PropertyVerificationPlan

instance FromJSON PropertyVerificationPlan
