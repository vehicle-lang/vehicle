module Vehicle.Verify.Core where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import System.FilePath ((<.>))
import Vehicle.Compile.Context.Bound.Core (GenericBoundCtx)
import Vehicle.Compile.Resource
import Vehicle.Data.BooleanExpr
import Vehicle.Data.LinearExpr
import Vehicle.Prelude
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin.BasicOperations
import Vehicle.Verify.Variable

--------------------------------------------------------------------------------
-- Meta-network

data NetworkContextInfo = NetworkContextInfo
  { networkFilepath :: FilePath,
    networkType :: NetworkType
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON NetworkContextInfo

instance FromJSON NetworkContextInfo

data MetaNetworkEntry = MetaNetworkEntry
  { metaNetworkEntryName :: Name,
    metaNetworkEntryInfo :: NetworkContextInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MetaNetworkEntry

instance FromJSON MetaNetworkEntry

instance Pretty MetaNetworkEntry where
  pretty MetaNetworkEntry {..} =
    pretty metaNetworkEntryName
      <> ":"
      <> softline
      <> pretty (networkType metaNetworkEntryInfo)

-- <> softline <> parens (pretty metaNetworkEntryFilePath)

-- | A list of neural networks used in a given query.
data MetaNetwork = MetaNetwork
  { networkEntries :: [MetaNetworkEntry],
    variables :: [NetworkRationalVariable]
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON MetaNetwork

instance FromJSON MetaNetwork

--------------------------------------------------------------------------------
-- Queries misc

-- | Location of a verifier query file.
type QueryFile = FilePath

type QueryText = Text

-- | Tracks whether or not the result of the query set should be negated.
-- Not that at first glance this might seem like it can be lifted to whether
-- or not the property is negated, but recall a property can have multiple
-- query sets. e.g. prop = (forall x . P x) and (exists x . Q y).
type QuerySetNegationStatus = Bool

--------------------------------------------------------------------------------
-- Query results

data QueryResult witness
  = SAT (Maybe witness)
  | UnSAT
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (FromJSON witness) => FromJSON (QueryResult witness)

instance (ToJSON witness) => ToJSON (QueryResult witness)

instance (Pretty witness) => Pretty (QueryResult witness) where
  pretty = \case
    SAT w -> "SAT:" <+> pretty w
    UnSAT -> "UNSAT"

--------------------------------------------------------------------------------
-- Property addresses

-- | The number of an individual property within a specification.
type PropertyID = Int

-- | A name of a property in the specification.
type PropertyName = Name

-- | A set of properties in the specification.
type PropertyNames = [PropertyName]

-- | A unique identifier for every individual property that needs to be verified.
-- Not simply an identifier, as we need to identifier sub-properties in tensors of
-- properties.
data PropertyAddress = PropertyAddress
  { propertyID :: PropertyID,
    propertyName :: PropertyName,
    propertyIndices :: TensorIndices
  }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON PropertyAddress

instance FromJSON PropertyAddress

instance Pretty PropertyAddress where
  pretty (PropertyAddress _ name indices) =
    concatWith (\a b -> a <> "!" <> b) (pretty name : fmap pretty indices)

calculatePropertyFilePrefix :: PropertyAddress -> FilePath
calculatePropertyFilePrefix (PropertyAddress _ propertyName propertyIndices) = do
  let indexStr
        | null propertyIndices = ""
        | otherwise = showTensorIndices propertyIndices
  unpack propertyName <> indexStr

--------------------------------------------------------------------------------
-- Addresses

-- | The number of an individual query within a `Property` when traversed
-- depth-first.
type QueryID = Int

type QueryAddress = (PropertyAddress, QueryID)

calculateQueryFileName :: QueryAddress -> FilePath
calculateQueryFileName (propertyAddress, queryID) = do
  calculatePropertyFilePrefix propertyAddress <> "-query" <> show queryID <.> "txt"

--------------------------------------------------------------------------------
-- Variable status

data UnderConstrainedVariableStatus
  = Unconstrained
  | BoundedAbove
  | BoundedBelow
  deriving (Show, Eq, Ord)

instance Pretty UnderConstrainedVariableStatus where
  pretty = \case
    Unconstrained -> "Unconstrained"
    BoundedAbove -> "BoundedAbove"
    BoundedBelow -> "BoundedBelow"

instance Semigroup UnderConstrainedVariableStatus where
  Unconstrained <> r = r
  r <> Unconstrained = r
  BoundedAbove <> r = r
  r <> BoundedAbove = r
  BoundedBelow <> BoundedBelow = BoundedBelow

--------------------------------------------------------------------------------
-- Queries

data QueryRelation
  = EqualRel
  | OrderRel OrderOp
  deriving (Show, Eq, Ord)

instance Pretty QueryRelation where
  pretty = \case
    EqualRel -> "="
    OrderRel op -> pretty op

flipQueryRel :: QueryRelation -> QueryRelation
flipQueryRel = \case
  EqualRel -> EqualRel
  OrderRel op -> OrderRel (flipOrder op)

-- A single assertion for a query.
data QueryAssertion = QueryAssertion
  { lhs :: NonEmpty (Coefficient, NetworkRationalVariable),
    rel :: QueryRelation,
    rhs :: Rational
  }

instance Pretty QueryAssertion where
  pretty (QueryAssertion lhs rel rhs) = pretty lhs <> pretty rel <> pretty rhs

queryAssertionVariables :: QueryAssertion -> Set NetworkRationalVariable
queryAssertionVariables = Set.fromList . fmap snd . NonEmpty.toList . lhs

-- | The contents of a single query for a verifier.
data QueryContents = QueryContents
  { queryVariables :: GenericBoundCtx NetworkRationalVariable,
    queryAssertions :: ConjunctAll QueryAssertion
  }
