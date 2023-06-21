module Vehicle.Verify.Core where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude (Name)
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.Variable
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Verifiers

data VerifierID
  = Marabou
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Pretty VerifierID where
  pretty = pretty . show

-- | Location of the verifier executable file
type VerifierExecutable = FilePath

-- | The type of methods to call a verifier on a query
type VerifierInvocation =
  forall m.
  (MonadIO m) =>
  VerifierExecutable ->
  MetaNetwork ->
  QueryFile ->
  m (Either Text (QueryResult NetworkVariableAssignments))

-- | A complete verifier implementation
data Verifier = Verifier
  { -- | The identifier for the verifier within Vehicle itself
    verifierIdentifier :: VerifierID,
    -- | The query format that the verifier accepts
    verifierQueryFormat :: QueryFormatID,
    -- | The name of the executable for the verifier
    verifierExecutableName :: String,
    -- | The command to invoke the verifier
    invokeVerifier :: VerifierInvocation
  }

--------------------------------------------------------------------------------
-- Addresses

-- | The number of an individual query within a `Property` when traversed
-- depth-first.
type QueryID = Int

type QueryAddress = (PropertyAddress, QueryID)

data PropertyAddress = PropertyAddress
  { propertyName :: Name,
    propertyIndices :: TensorIndices
  }
  deriving (Show, Generic)

instance ToJSON PropertyAddress

instance FromJSON PropertyAddress

instance Pretty PropertyAddress where
  pretty (PropertyAddress name indices) =
    concatWith (\a b -> a <> "!" <> b) (pretty name : fmap pretty indices)

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

-- | A list of neural networks used in a given query.
type MetaNetwork = [(Name, FilePath)]

--------------------------------------------------------------------------------
-- Variable assignments

data UserVariableAssignment = UserVariableAssignment
  { variableName :: Name,
    variableDimensions :: TensorDimensions,
    variableValue :: Vector Double
  }
  deriving (Generic)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

-- An assignment to network variables ordered sequentially.
type NetworkVariableAssignments = Vector Double

-- An assignment to user variables ordered sequentially.
type UserVariableAssignments = [UserVariableAssignment]

--------------------------------------------------------------------------------
-- Query formats

data QueryFormatID
  = MarabouQueries
  | VNNLibQueries
  deriving (Show, Eq, Bounded, Enum)

instance Pretty QueryFormatID where
  pretty = \case
    MarabouQueries -> "Marabou query format"
    VNNLibQueries -> "VNNLib query format"

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    -- | The command to compile an individual query
    compileQuery :: forall m. (MonadLogger m) => CLSTProblem NetworkVariable -> m QueryText
  }

--------------------------------------------------------------------------------
-- Query results

data QueryResult witness
  = SAT (Maybe witness)
  | UnSAT
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (FromJSON witness) => FromJSON (QueryResult witness)

instance (ToJSON witness) => ToJSON (QueryResult witness)

--------------------------------------------------------------------------------
-- Variable reconstruction

-- | Information for mapping normalised user variables back to
-- unnormalised user variables. These are stored in reverse order, i.e. the
-- deepest variables are at the head of the list.
type QueryUnnormalisedVariableInfo = [(Name, TensorDimensions)]

-- | Information for mapping network variables back to normalised
-- user variables.
type QueryNormalisedVariableInfo = [(LinearVar, VariableSolution)]

-- | Information for mapping network variables back to unnormalise user variables.
data QueryVariableInfo = QueryVariableInfo
  { unnormalisedVariableInfo :: QueryUnnormalisedVariableInfo,
    normalisedVariableInfo :: QueryNormalisedVariableInfo
  }
  deriving (Show, Generic)

instance ToJSON QueryVariableInfo

instance FromJSON QueryVariableInfo

-- | Information neccesary to reconstruct the user variables from the magic
-- input/output variables.
data VariableSolution
  = GaussianSolution GaussianVariableSolution
  | FourierMotzkinSolution FourierMotzkinVariableSolution
  deriving (Show, Generic)

instance ToJSON VariableSolution

instance FromJSON VariableSolution

-- | A solution for a normalised user variable that is an equation
-- where the coefficient for that variable is 1.
newtype GaussianVariableSolution = GaussianVariableSolution
  { solutionEquality :: SparseLinearExpr
  }
  deriving (Show, Generic)

instance ToJSON GaussianVariableSolution

instance FromJSON GaussianVariableSolution

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data FourierMotzkinVariableSolution = FMSolution
  { lowerBounds :: [Assertion SparseLinearExpr],
    upperBounds :: [Assertion SparseLinearExpr]
  }
  deriving (Show, Generic)

instance ToJSON FourierMotzkinVariableSolution

instance FromJSON FourierMotzkinVariableSolution
