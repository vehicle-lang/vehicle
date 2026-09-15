{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Verify.Core where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..), genericToJSON)
import Data.Bifunctor qualified
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Prettyprinter (brackets)
import System.FilePath ((<.>), (</>))
import Vehicle.Compile.Resource
import Vehicle.Data.Assertion (InequalityRelation (..), Relation (..))
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Tensor (RatTensor, TensorIndices, TensorShape, showTensorIndices)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Meta-network

data NetworkContextInfo = NetworkContextInfo
  { networkFilepath :: FilePath,
    networkType :: NetworkType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkContextInfo

instance ToJSON NetworkContextInfo

instance FromJSON NetworkContextInfo

-- | A list of neural networks used in a given query.
-- The last `int` refers to the number of times it is applied.
type MetaNetwork = [(Name, NetworkContextInfo, Int)]

inputShape :: NetworkContextInfo -> NetworkModality TensorShape
inputShape = fmap getIODims . networkInputType . networkType

outputShape :: NetworkContextInfo -> NetworkModality TensorShape
outputShape = fmap getIODims . networkOutputType . networkType

--------------------------------------------------------------------------------
-- Queries misc

-- | Location of a solver query file.
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

querySatisified :: QueryResult witness -> Bool
querySatisified = \case
  SAT {} -> True
  UnSAT -> False

--------------------------------------------------------------------------------
-- Property addresses

-- | A name of a property in the specification.
type PropertyName = Name

-- | A unique identifier for every individual property that needs to be verified.
-- Not simply an identifier, as we need to identifier sub-properties in tensors of
-- properties.
data PropertyAddress = PropertyAddress
  { propertyName :: PropertyName,
    propertyIndices :: TensorIndices
  }
  deriving (Eq, Show, Generic)

instance NFData PropertyAddress

instance ToJSON PropertyAddress where
  toJSON = genericToJSON jsonOptions

instance FromJSON PropertyAddress

instance Pretty PropertyAddress where
  pretty (PropertyAddress name indices) =
    concatWith (\a b -> a <> "!" <> b) (pretty name : fmap pretty indices)

calculatePropertyFilePrefix :: PropertyAddress -> FilePath
calculatePropertyFilePrefix (PropertyAddress propertyName indices) = do
  let indexStr
        | null indices = ""
        | otherwise = showTensorIndices indices
  unpack propertyName <> indexStr

--------------------------------------------------------------------------------
-- Addresses

-- | The number of an individual query within a `Property` when traversed
-- depth-first.
type QueryID = Int

data QueryAddress = QueryAddress
  { property :: PropertyAddress,
    queryID :: QueryID
  }
  deriving (Eq, Show, Generic)

instance NFData QueryAddress

instance ToJSON QueryAddress where
  toJSON = genericToJSON jsonOptions

instance FromJSON QueryAddress

instance Pretty QueryAddress where
  pretty (QueryAddress property queryID) = pretty property <+> brackets (pretty queryID)

calculateQueryFileName :: FilePath -> QueryAddress -> FilePath
calculateQueryFileName verificationCache (QueryAddress propertyAddress queryID) = do
  verificationCache
    </> calculatePropertyFilePrefix propertyAddress
      <> "-query"
      <> show queryID
        <.> "txt"

--------------------------------------------------------------------------------
-- Queries

data QueryRelation
  = EqRel
  | LeRel
  | LtRel
  | GeRel
  | GtRel
  deriving (Show, Eq, Ord)

instance Pretty QueryRelation where
  pretty = \case
    EqRel -> pretty Eq
    LeRel -> pretty Le
    LtRel -> pretty Lt
    GeRel -> pretty Ge
    GtRel -> pretty Gt

relationToQueryRelation :: Relation -> QueryRelation
relationToQueryRelation = \case
  OEq -> EqRel
  OLt -> LtRel
  OLe -> LeRel

inequalityToQueryRelation :: InequalityRelation -> QueryRelation
inequalityToQueryRelation = \case
  Strict -> LtRel
  NonStrict -> LeRel

flipQueryRel :: QueryRelation -> QueryRelation
flipQueryRel = \case
  EqRel -> EqRel
  LeRel -> GeRel
  LtRel -> GtRel
  GeRel -> LeRel
  GtRel -> GtRel

--------------------------------------------------------------------------------
-- User variable assignments

-- | A (satisfying) assignment to a set of user-level variables.
newtype UserVariableAssignment
  = UserVariableAssignment [(Name, UserVariableValue)]
  deriving (Generic)

instance Pretty UserVariableAssignment where
  pretty (UserVariableAssignment assignments) =
    vsep (fmap pretty assignments)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

data UserVariableValue
  = TensorValue RatTensor
  | RecordValue (NonEmpty (Name, RatTensor))
  deriving (Show, Generic, Eq)

instance ToJSON UserVariableValue

instance FromJSON UserVariableValue

instance Pretty UserVariableValue where
  pretty t = case t of
    TensorValue tens -> pretty tens
    RecordValue fields -> prettyRecordValueEntries $ map (Data.Bifunctor.bimap pretty pretty) (NonEmpty.toList fields)
