{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Verify.Core where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..), genericToJSON)
import Data.Set (Set)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Prettyprinter (brackets)
import System.FilePath ((<.>), (</>))
import Vehicle.Compile.Resource
import Vehicle.Data.Assertion (InequalityRelation (..), Relation (..))
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr (BooleanExpr (..))
import Vehicle.Data.MaybeTrivial (MaybeTrivial)
import Vehicle.Data.Tensor (RatTensor, TensorIndices, TensorShape, showTensorIndices)
import Vehicle.Prelude
import Vehicle.Verify.QueryFormat.Core (QueryVariable)

--------------------------------------------------------------------------------
-- Meta-network
--------------------------------------------------------------------------------

data NetworkContextInfo = NetworkContextInfo
  { networkFilepath :: FilePath,
    networkType :: NetworkType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkContextInfo

instance ToJSON NetworkContextInfo

instance FromJSON NetworkContextInfo

-- | A list of neural networks used in a given query.
type MetaNetwork = [(Name, NetworkContextInfo, Int)]

inputShape :: NetworkContextInfo -> TensorShape
inputShape ctx = case networkInputType (networkType ctx) of
  TensorIOType (NetworkTensorType _ dims) -> dims
  RecordIOType (NetworkRecordType _ _ dims _) -> dims

outputShape :: NetworkContextInfo -> TensorShape
outputShape ctx = case networkOutputType (networkType ctx) of
  TensorIOType (NetworkTensorType _ dims) -> dims
  RecordIOType (NetworkRecordType _ _ dims _) -> dims

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | Errors thrown by a verifier
data VerifierError
  = VerifierTerminatedByOS Int
  | VerifierError String
  | VerifierOutputMalformed String
  | VerifierIncompleteWitness (Set QueryVariable)
  | VerifierTimedOut
  deriving (Show, Generic)

instance ToJSON VerifierError where
  toJSON = genericToJSON jsonOptions

instance FromJSON VerifierError

isTimeoutError :: VerifierError -> Bool
isTimeoutError = \case
  VerifierTimedOut -> True
  _ -> False

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

type QueryText = Text

-- | Tracks whether or not the the query set is solving an existential or
-- a universal property.
-- Note that at first glance this might seem like it can be lifted to whether
-- or not the property is negated, but recall a property can have multiple
-- query sets. e.g. prop = (forall x . P x) and (exists x . Q y).
type QuerySetPolarity = Quantifier

--------------------------------------------------------------------------------
-- Query result

data QueryResult
  = QuerySAT (Maybe QueryVariablesAssignment)
  | QueryUnSAT
  | QueryErrored VerifierError
  deriving (Show, Generic)

instance ToJSON QueryResult where
  toJSON = genericToJSON jsonOptions

instance FromJSON QueryResult

instance Pretty QueryResult where
  pretty = \case
    QuerySAT w -> "SAT:" <+> pretty w
    QueryUnSAT -> "UNSAT"
    QueryErrored {} -> "ERRORED"

isQuerySAT :: QueryResult -> Either VerifierError Bool
isQuerySAT = \case
  QuerySAT {} -> Right True
  QueryUnSAT {} -> Right False
  QueryErrored err -> Left err

--------------------------------------------------------------------------------
-- Query address

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

-- | Location of a verifier query file.
type QueryFile = FilePath

calculateQueryFileName :: FilePath -> QueryAddress -> QueryFile
calculateQueryFileName verificationCache (QueryAddress propertyAddress queryID) = do
  verificationCache
    </> calculatePropertyFilePrefix propertyAddress
      <> "-query"
      <> show queryID
        <.> "txt"

--------------------------------------------------------------------------------
-- Query variable assignment

data QueryVariableAssignment = QueryVariableAssignment
  { queryVariable :: QueryVariable,
    value :: Rational
  }
  deriving (Generic)

instance ToJSON QueryVariableAssignment where
  toJSON = genericToJSON jsonOptions

-- | A (satisfying) assignment to a set of reduced network-level variables.
type QueryVariablesAssignment = [(QueryVariable, Rational)]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

-- | A name of a property in the specification.
type PropertyName = Name

--------------------------------------------------------------------------------
-- Property addresses

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
-- User variable assignments

-- | A (satisfying) assignment to a set of user-level variables.
newtype UserVariablesAssignment
  = UserVariablesAssignment [(Name, RatTensor)]
  deriving (Generic)

instance ToJSON UserVariablesAssignment

instance FromJSON UserVariablesAssignment

instance Pretty UserVariablesAssignment where
  pretty (UserVariablesAssignment assignment) =
    vsep (fmap pretty assignment)

--------------------------------------------------------------------------------
-- Query result

data QuerySetResult
  = -- | One of the queries in this set produced an error
    Errored QueryAddress VerifierError
  | -- | The set of queries was evaluated and returned the following result.
    Returned QuerySetPolarity (Maybe UserVariablesAssignment)
  deriving (Generic)

instance ToJSON QuerySetResult where
  toJSON = genericToJSON jsonOptions

isQuerySetVerified :: QuerySetResult -> Either VerifierError Bool
isQuerySetVerified = \case
  Errored _ err -> Left err
  Returned polarity ass -> _

type PropertyResult = MaybeTrivial (BooleanExpr QuerySetResult)

calculatePropertyVerified :: PropertyResult -> Either VerifierError Bool
calculatePropertyVerified = \case
  Conjunct xs -> _
  Disjunct xs -> _
  Atom x -> isQuerySetVerified x

--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

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
