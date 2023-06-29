module Vehicle.Verify.Core where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude (Name)
import Vehicle.Compile.Queries.LinearExpr (Assertion, CLSTProblem, SparseLinearExpr)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Resource
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Assignments to variables

-- | A (satisfying) assignment to a set of reduced network-level variables.
newtype NetworkVariableAssignment
  = NetworkVariableAssignment (Vector Double)

-- | A (satisfying) assignment to a set of user-level variables.
newtype UserVariableAssignment
  = UserVariableAssignment [(UserVariable, VariableValue)]
  deriving (Generic)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

instance Pretty UserVariableAssignment where
  pretty :: UserVariableAssignment -> Doc a
  pretty (UserVariableAssignment assignment) = do
    vsep (fmap prettyVariable assignment)
    where
      prettyVariable :: (UserVariable, VariableValue) -> Doc a
      prettyVariable (var, value) = do
        let name = pretty $ userVarName var
        let valueDoc = prettyConstant True (userVarDimensions var) value
        name <> ":" <+> valueDoc

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
  m (Either Text (QueryResult NetworkVariableAssignment))

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
-- Meta-network

data MetaNetworkEntry = MetaNetworkEntry
  { metaNetworkEntryName :: Name,
    metaNetworkEntryType :: NetworkType,
    metaNetworkEntryFilePath :: FilePath
  }
  deriving (Show, Generic)

instance ToJSON MetaNetworkEntry

instance FromJSON MetaNetworkEntry

instance Pretty MetaNetworkEntry where
  pretty MetaNetworkEntry {..} =
    pretty metaNetworkEntryName
      <> ":"
      <> softline
      <> pretty metaNetworkEntryType

-- <> softline <> parens (pretty metaNetworkEntryFilePath)

-- | A list of neural networks used in a given query.
type MetaNetwork = [MetaNetworkEntry]

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

metaNetworkEntryVariables ::
  Bool ->
  MetaNetworkEntry ->
  (Map Name Int, [[NetworkVariable]]) ->
  (Map Name Int, [[NetworkVariable]])
metaNetworkEntryVariables reduced MetaNetworkEntry {..} (applications, vars) = do
  let applicationNumber = Map.findWithDefault 0 metaNetworkEntryName applications
  let newApplications = Map.insert metaNetworkEntryName (applicationNumber + 1) applications

  let (inputIndices, outputIndices, inputDimensions, outputDimensions)
        | not reduced = do
            let inputDims = dimensions $ inputTensor metaNetworkEntryType
            let outputDims = dimensions $ outputTensor metaNetworkEntryType
            ([Nothing], [Nothing], inputDims, outputDims)
        | otherwise = do
            let inputs = Just <$> [0 .. tensorSize (inputTensor metaNetworkEntryType) - 1]
            let outputs = Just <$> [0 .. tensorSize (outputTensor metaNetworkEntryType) - 1]
            (inputs, outputs, [], [])
  let mkVariable = NetworkVariable metaNetworkEntryName applicationNumber
  let mkInputVariable = mkVariable inputDimensions Input
  let mkOutputVariable = mkVariable outputDimensions Output
  let inputVariables = [mkInputVariable i | i <- inputIndices]
  let outputVariables = [mkOutputVariable i | i <- outputIndices]

  (newApplications, (inputVariables <> outputVariables) : vars)

metaNetworkVariables :: Bool -> MetaNetwork -> [NetworkVariable]
metaNetworkVariables reduced metaNetwork = do
  let (_, result) = foldr (metaNetworkEntryVariables reduced) (mempty, mempty) metaNetwork
  concat (reverse result)

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
    compileQuery :: forall m. (MonadLogger m) => CLSTProblem -> m QueryText
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

-- | A solution for a normalised user variable that is an equation
-- where the coefficient for that variable is 1.
newtype GaussianVariableSolution = GaussianVariableSolution
  { solutionEquality :: SparseLinearExpr MixedVariable
  }
  deriving (Show, Generic)

instance ToJSON GaussianVariableSolution

instance FromJSON GaussianVariableSolution

instance Pretty GaussianVariableSolution where
  pretty = pretty . solutionEquality

-- | A FM solution for an normalised user variable is two lists of constraints.
-- The variable value must be greater than the first set of assertions, and less than
-- the second set of assertions.
data FourierMotzkinVariableSolution = FMSolution
  { lowerBounds :: [Assertion MixedVariable],
    upperBounds :: [Assertion MixedVariable]
  }
  deriving (Show, Generic)

instance ToJSON FourierMotzkinVariableSolution

instance FromJSON FourierMotzkinVariableSolution

-- | One step in the process for transforming unreduced user variables into
-- reduced network input and output variables.
data VariableNormalisationStep
  = EliminateViaGaussian MixedVariable GaussianVariableSolution
  | EliminateViaFourierMotzkin MixedVariable FourierMotzkinVariableSolution
  | Reduce MixedVariable
  | Introduce MixedVariable
  deriving (Show, Generic)

instance ToJSON VariableNormalisationStep

instance FromJSON VariableNormalisationStep

instance Pretty VariableNormalisationStep where
  pretty = \case
    EliminateViaGaussian v s -> "EliminateGaussian[" <+> pretty v <+> "=" <+> pretty s <+> "]"
    EliminateViaFourierMotzkin v _ -> "EliminateFourierMotzkin[" <+> pretty v <+> "]"
    Reduce v -> "Reduce[" <+> pretty v <+> "]"
    Introduce v -> "Introduce[" <+> pretty v <+> "]"

-- | The steps for transforming unreduced user variables into reduced network
-- input and output varibles.
-- These are used to recreate a satisfying assignment for the user variables
-- from the satisfying assignment for the network variables spat out by the
-- verifier.
--
-- The steps are stored in the same order they occured during compilation.
type VariableNormalisationSteps = [VariableNormalisationStep]
