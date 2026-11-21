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
    CompilationStep (..),
    ReconstructionDepth (..),
    VariableCompilationTrace (..),
    VariableStore (..),
    getQueryVariables,
    getVehicleVariableCtx,
    getQueryVariableMap,
    getUserVariables,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON (..), genericToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Prelude (Name, Pretty (..), jsonOptions)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Resource (ResourcesIntegrityInfo)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- User variable

data ReconstructionDepth
  = AllDimensions
  | OneDimension
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Pretty ReconstructionDepth where
  pretty = \case
    AllDimensions -> "all"
    OneDimension -> "1D"

-- | One step in the process for transforming unreduced user variables into
-- reduced network input and output variables.
data CompilationStep
  = SolveEquality NestedSliceVariable LinearExpression
  | SolveInequalities SliceVariable LinearBounds
  | ReconstructTensorVariable NestedSliceVariable ReconstructionDepth
  | ConvertQuantifiedTensorLike Name Name (NonEmpty Name)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | The steps for transforming unreduced user variables into reduced network
-- input and output varibles.
-- These are used to recreate a satisfying assignment for the user variables
-- from the satisfying assignment for the network variables spat out by the
-- solver.
-- The steps are stored in the same order they occured during compilation.
newtype VariableCompilationTrace = Reconstruction
  { reconstructionSteps :: [CompilationStep]
  }
  deriving stock (Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Variable store

-- Storing the Variable is unnessary and is just for readability. We can get
-- rid of it if we switch to a non-human readable format for the .vcl-plan files.
data VariableStore = VariableStore
  { queryVariableMapping :: Map QueryVariable NetworkIOElementVariable,
    vehicleVariableCtx :: CompleteNamedBoundCtx,
    userVariables :: Set UserSliceVariable
  }
  deriving stock (Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

getVehicleVariableCtx :: VariableStore -> CompleteNamedBoundCtx
getVehicleVariableCtx VariableStore {..} = vehicleVariableCtx

getUserVariables :: VariableStore -> Set UserSliceVariable
getUserVariables VariableStore {..} = userVariables

getQueryVariables :: VariableStore -> [QueryVariable]
getQueryVariables VariableStore {..} = Map.keys queryVariableMapping

getQueryVariableMap :: VariableStore -> Map QueryVariable NetworkIOElementVariable
getQueryVariableMap = queryVariableMapping

--------------------------------------------------------------------------------
-- Query meta data

data QueryMetaData = QueryMetaData
  { queryAddress :: !QueryAddress,
    metaNetwork :: !MetaNetwork,
    variableStore :: !VariableStore,
    variableCompilationTrace :: !VariableCompilationTrace
  }
  deriving stock (Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Query set

data QuerySet a = QuerySet
  { negated :: !QuerySetNegationStatus,
    queries :: !(DisjunctAll a)
  }
  deriving stock (Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData, ToJSON, FromJSON)

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
    SingleProperty property
  | -- | Multiple nested boolean properties e.g. generated by a `foreach`.
    -- They are implicitly conjuncted.
    MultiProperty [MultiProperty property]
  deriving stock (Show, Functor, Generic)
  deriving anyclass (FromJSON)

instance (ToJSON property) => ToJSON (MultiProperty property) where
  toJSON = genericToJSON jsonOptions

multiPropertyAddresses :: MultiProperty PropertyAddress -> [PropertyAddress]
multiPropertyAddresses = \case
  SingleProperty address -> [address]
  MultiProperty ps -> concatMap multiPropertyAddresses ps

--------------------------------------------------------------------------------
-- Specification

-- | A compiled specification, parameterised by the data stored at each query.
newtype Specification property
  = Specification [(Name, MultiProperty property)]
  deriving stock (Show, Generic, Functor)
  deriving anyclass (ToJSON, FromJSON)

specificationPropertyNames :: Specification a -> [PropertyName]
specificationPropertyNames (Specification properties) = fmap fst properties

--------------------------------------------------------------------------------
-- Verification plans

-- | The object that provides the required information to perform or check the
-- verification of an entire specification.
data SpecificationCacheIndex = SpecificationCacheIndex
  { resourcesIntegrityInfo :: ResourcesIntegrityInfo,
    properties :: [(PropertyName, MultiProperty PropertyAddress)]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The object that provides the required information to perform the
-- verification of a single property within a specification.
newtype PropertyVerificationPlan = PropertyVerificationPlan
  { queryMetaData :: Property QueryMetaData
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
