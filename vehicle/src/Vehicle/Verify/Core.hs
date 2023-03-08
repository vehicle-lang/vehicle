module Vehicle.Verify.Core where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
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
  MonadIO m =>
  VerifierExecutable ->
  MetaNetwork ->
  UserVarReconstructionInfo ->
  QueryFile ->
  m QueryResult

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
-- Queries

-- | Location of a verifier query file.
type QueryFile = FilePath

type QueryText = Text

-- | Tracks whether or not the result of the query should be negated.
type QueryNegationStatus = Bool

-- | A list of neural networks used in a given query.
type MetaNetwork = [(Name, FilePath)]

--------------------------------------------------------------------------------
-- Query formats

data QueryFormatID
  = MarabouQueryFormat
  deriving (Show, Eq)

instance Pretty QueryFormatID where
  pretty = \case
    MarabouQueryFormat -> "Marabou query format"

-- | A format for an output query that verifiers can parse.
data QueryFormat = QueryFormat
  { queryFormatID :: QueryFormatID,
    queryOutputFormat :: ExternalOutputFormat,
    -- | The command to compile an individual query
    compileQuery :: forall m. MonadLogger m => CLSTProblem NetworkVariable -> m QueryText
  }

--------------------------------------------------------------------------------
-- Query results

data QueryResult
  = SAT (Maybe Text)
  | UnSAT
  deriving (Show, Generic)

instance FromJSON QueryResult

instance ToJSON QueryResult

type UserVarReconstructionInfo = [(LinearVar, VariableSolution)]

-- | A solution for a query variable that is an equation where the coefficient
-- for that variable is 1.
newtype GaussianVariableSolution = GaussianVariableSolution
  { solutionEquality :: SparseLinearExpr
  }
  deriving (Generic)

instance ToJSON GaussianVariableSolution

instance FromJSON GaussianVariableSolution

-- | A FM solution for a variable is two lists of constraints. The variable value
-- must be greater than the set of assertions, and less than the first is that
data FourierMotzkinVariableSolution = FMSolution
  { lowerBounds :: [Assertion SparseLinearExpr],
    upperBounds :: [Assertion SparseLinearExpr]
  }
  deriving (Generic)

instance ToJSON FourierMotzkinVariableSolution

instance FromJSON FourierMotzkinVariableSolution

-- | Information neccesary to reconstruct the user variables from the magic
-- input/output variables.
data VariableSolution
  = GaussianSolution GaussianVariableSolution
  | FourierMotzkinSolution FourierMotzkinVariableSolution
  deriving (Generic)

instance ToJSON VariableSolution

instance FromJSON VariableSolution
