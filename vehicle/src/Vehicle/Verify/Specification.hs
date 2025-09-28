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
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude (CompleteNamedBoundCtx, Name, Pretty (..))
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Resource (ResourcesIntegrityInfo)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- User variable

data ReconstructionDepth
  = AllDimensions
  | OneDimension
  deriving (Show, Eq, Ord, Generic)

instance NFData ReconstructionDepth

instance ToJSON ReconstructionDepth

instance FromJSON ReconstructionDepth

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
  deriving (Show, Eq, Ord, Generic)

instance NFData CompilationStep

instance ToJSON CompilationStep

instance FromJSON CompilationStep

-- | The steps for transforming unreduced user variables into reduced network
-- input and output varibles.
-- These are used to recreate a satisfying assignment for the user variables
-- from the satisfying assignment for the network variables spat out by the
-- verifier.
-- The steps are stored in the same order they occured during compilation.
newtype VariableCompilationTrace = Reconstruction
  { reconstructionSteps :: [CompilationStep]
  }
  deriving (Generic)

instance NFData VariableCompilationTrace

instance ToJSON VariableCompilationTrace

instance FromJSON VariableCompilationTrace

--------------------------------------------------------------------------------
-- Variable store

-- Storing the Variable is unnessary and is just for readability. We can get
-- rid of it if we switch to a non-human readable format for the .vcl-plan files.
data VariableStore = VariableStore
  { queryVariableMapping :: Map QueryVariable NetworkIOElementVariable,
    vehicleVariableCtx :: CompleteNamedBoundCtx,
    userVariables :: Set UserVariable
  }
  deriving (Generic)

instance NFData VariableStore

instance ToJSON VariableStore

instance FromJSON VariableStore

{-
getQueryVariableCtx :: VariableCompilationTrace -> GenericBoundCtx (Maybe QueryVariable)
getQueryVariableCtx d = fmap (\(_, _, c) -> c) (variableStore d)
-}
getVehicleVariableCtx :: VariableStore -> CompleteNamedBoundCtx
getVehicleVariableCtx VariableStore {..} = vehicleVariableCtx

getUserVariables :: VariableStore -> Set UserVariable
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
  deriving (Generic)

instance NFData QueryMetaData

instance ToJSON QueryMetaData

instance FromJSON QueryMetaData

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
    properties :: [(PropertyName, MultiProperty ())]
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
