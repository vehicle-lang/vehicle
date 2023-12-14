module Vehicle.Verify.Core where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text, unpack)
import Data.Vector (Vector)
import Data.Vector qualified as Vector (toList)
import GHC.Generics (Generic)
import System.FilePath ((<.>))
import Vehicle.Backend.Queries.LinearExpr (Assertion, SparseLinearExpr)
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Resource
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Assignments to variables

-- | A (satisfying) assignment to a set of reduced network-level variables.
newtype NetworkVariableAssignment
  = NetworkVariableAssignment (Vector Rational)

instance Pretty NetworkVariableAssignment where
  pretty :: NetworkVariableAssignment -> Doc a
  pretty (NetworkVariableAssignment assignment) = do
    vsep (prettyVariable <$> zip [0 ..] (Vector.toList assignment))
    where
      prettyVariable :: (Int, Rational) -> Doc a
      prettyVariable (var, value) = "x" <> pretty var <> ":" <+> pretty value

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
-- Addresses

-- | The number of an individual query within a `Property` when traversed
-- depth-first.
type QueryID = Int

type QueryAddress = (PropertyAddress, QueryID)

calculateQueryFileName :: QueryAddress -> FilePath
calculateQueryFileName (PropertyAddress propertyName propertyIndices, queryID) = do
  let propertyStr
        | null propertyIndices = ""
        | otherwise = showTensorIndices propertyIndices

  unpack propertyName
    <> propertyStr
    <> "-query"
    <> show queryID <.> "txt"

-- | A unique identifier for every individual property that needs to be verified.
-- Not simply an identifier, as we need to identifier sub-properties in tensors of
-- properties.
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

--------------------------------------------------------------------------------
-- Variable status

data UnderConstrainedVariableStatus
  = Unconstrained
  | BoundedAbove
  | BoundedBelow
  deriving (Show, Eq)

instance Semigroup UnderConstrainedVariableStatus where
  Unconstrained <> r = r
  r <> Unconstrained = r
  BoundedAbove <> r = r
  r <> BoundedAbove = r
  BoundedBelow <> BoundedBelow = BoundedBelow

-- | How the value of a particular value of a variable is constrained.
data VariableConstraintStatus
  = UnderConstrained UnderConstrainedVariableStatus
  | Bounded
  | Constant
  deriving (Show, Eq)

instance Semigroup VariableConstraintStatus where
  UnderConstrained r <> UnderConstrained s = case (r, s) of
    (BoundedBelow, BoundedAbove) -> Bounded
    (BoundedAbove, BoundedBelow) -> Bounded
    _ -> UnderConstrained (r <> s)
  UnderConstrained {} <> r = r
  r <> UnderConstrained {} = r
  Bounded <> r = r
  r <> Bounded = r
  Constant <> Constant = Constant

toUnderConstrainedStatus :: VariableConstraintStatus -> Maybe UnderConstrainedVariableStatus
toUnderConstrainedStatus = \case
  UnderConstrained s -> Just s
  _ -> Nothing
