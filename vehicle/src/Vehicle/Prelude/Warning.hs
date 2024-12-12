module Vehicle.Prelude.Warning
  ( CompileWarning (..),
    SummarisedCompileWarning (..),
    groupWarnings,
  )
where

import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map (insertWith, singleton, toList, unionWith)
import Data.Set (Set)
import Data.Tuple (swap)
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices)
import Vehicle.Prelude (Name)
import Vehicle.Resource (ExternalResource)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Non-unique compile warnings

data CompileWarning
  = UnusedResources ExternalResource (Set Name)
  | TrivialProperty PropertyAddress Bool
  | UnderSpecifiedProblemSpaceVar PropertyAddress Name
  | UnsoundStrictOrderConversion QueryFormatID QueryAddress
  | AllConstantNetworkInputVars QueryFormatID QueryAddress
  | UnboundedNetworkInputVariables QueryFormatID QueryAddress [(Name, UnderConstrainedVariableStatus)]

data SummarisedCompileWarning
  = UnusedResourcesSummary ExternalResource (Set Name)
  | TrivialPropertySummary PropertyAddress Bool
  | UnderSpecifiedProblemSpaceVariablesSummary PropertyID PropertyName (NonEmpty (Name, TensorIndices))
  | UnsoundStrictOrderConversionsSummary QueryFormatID PropertyID PropertyName Int
  | AllConstantNetworkInputVariablesSummary QueryFormatID PropertyID PropertyName (NonEmpty (QueryID, TensorIndices))
  | UnboundedNetworkInputVariablesSummary QueryFormatID PropertyID PropertyName [(NonEmpty QueryID, [(Name, UnderConstrainedVariableStatus)])]

--------------------------------------------------------------------------------
-- Combinable compile warnings

type UnderConstrainedSignature = [(Name, UnderConstrainedVariableStatus)]

data CombiningState = CombiningState
  { uniqueWarnings :: [SummarisedCompileWarning],
    underSpecifiedProblemSpaceVars :: Map (PropertyID, PropertyName) (NonEmpty (Name, TensorIndices)),
    unsoundStrictnessConversions :: Map (QueryFormatID, PropertyID, PropertyName) Int,
    allConstantNetworkInputVars :: Map (QueryFormatID, PropertyID, PropertyName) (NonEmpty (QueryID, TensorIndices)),
    unboundedNetworkInputs :: Map (QueryFormatID, PropertyID, PropertyName) (Map UnderConstrainedSignature (NonEmpty QueryID)),
    inefficientTensorCode :: Map Name [(Builtin, NamedBoundCtx, Value TensorBuiltin)]
  }

emptyState :: CombiningState
emptyState = CombiningState mempty mempty mempty mempty mempty mempty

addWarningToState :: CombiningState -> CompileWarning -> CombiningState
addWarningToState CombiningState {..} = \case
  UnusedResources r names ->
    CombiningState
      { uniqueWarnings = UnusedResourcesSummary r names : uniqueWarnings,
        ..
      }
  TrivialProperty r names ->
    CombiningState
      { uniqueWarnings = TrivialPropertySummary r names : uniqueWarnings,
        ..
      }
  UnderSpecifiedProblemSpaceVar PropertyAddress {..} var ->
    CombiningState
      { underSpecifiedProblemSpaceVars = Map.insertWith (<>) (propertyID, propertyName) [(var, propertyIndices)] underSpecifiedProblemSpaceVars,
        ..
      }
  UnsoundStrictOrderConversion queryFormat (PropertyAddress {..}, _queryID) ->
    CombiningState
      { unsoundStrictnessConversions = Map.insertWith (+) (queryFormat, propertyID, propertyName) 1 unsoundStrictnessConversions,
        ..
      }
  AllConstantNetworkInputVars queryFormat (PropertyAddress {..}, queryID) ->
    CombiningState
      { allConstantNetworkInputVars =
          Map.insertWith (<>) (queryFormat, propertyID, propertyName) [(queryID, propertyIndices)] allConstantNetworkInputVars,
        ..
      }
  UnboundedNetworkInputVariables queryFormat (PropertyAddress {..}, queryID) vars ->
    CombiningState
      { unboundedNetworkInputs = Map.insertWith (Map.unionWith (<>)) (queryFormat, propertyID, propertyName) (Map.singleton vars [queryID]) unboundedNetworkInputs,
        ..
      }

groupWarnings :: [CompileWarning] -> [SummarisedCompileWarning]
groupWarnings warnings = stateToWarnings $ foldl addWarningToState emptyState warnings

stateToWarnings :: CombiningState -> [SummarisedCompileWarning]
stateToWarnings CombiningState {..} =
  sortBy compareWarning $
    uniqueWarnings
      <> fmap combineUnderSpecifiedProblemSpaceVars (Map.toList underSpecifiedProblemSpaceVars)
      <> fmap combineUnsoundStrictnessConversions (Map.toList unsoundStrictnessConversions)
      <> fmap combineAllConstantNetworkInputVars (Map.toList allConstantNetworkInputVars)
      <> fmap combineUnboundedNetworkInputVars (Map.toList unboundedNetworkInputs)

combineUnderSpecifiedProblemSpaceVars :: ((PropertyID, PropertyName), NonEmpty (Name, TensorIndices)) -> SummarisedCompileWarning
combineUnderSpecifiedProblemSpaceVars ((propertyID, property), vars) = UnderSpecifiedProblemSpaceVariablesSummary propertyID property vars

combineUnsoundStrictnessConversions :: ((QueryFormatID, PropertyID, PropertyName), Int) -> SummarisedCompileWarning
combineUnsoundStrictnessConversions ((queryFormatID, propertyID, property), number) =
  UnsoundStrictOrderConversionsSummary queryFormatID propertyID property number

combineAllConstantNetworkInputVars :: ((QueryFormatID, PropertyID, PropertyName), NonEmpty (QueryID, TensorIndices)) -> SummarisedCompileWarning
combineAllConstantNetworkInputVars ((queryFormatID, propertyID, property), queries) =
  AllConstantNetworkInputVariablesSummary queryFormatID propertyID property queries

combineUnboundedNetworkInputVars :: ((QueryFormatID, PropertyID, PropertyName), Map UnderConstrainedSignature (NonEmpty QueryID)) -> SummarisedCompileWarning
combineUnboundedNetworkInputVars ((queryFormatID, propertyID, property), constraintsBySignature) = do
  let result = swap <$> Map.toList constraintsBySignature
  UnboundedNetworkInputVariablesSummary queryFormatID propertyID property result

compareWarning :: SummarisedCompileWarning -> SummarisedCompileWarning -> Ordering
compareWarning w1 w2 = compare (warningPropertyId w1) (warningPropertyId w2)
  where
    warningPropertyId :: SummarisedCompileWarning -> Maybe PropertyID
    warningPropertyId w =
      case w of
        UnusedResourcesSummary {} -> Nothing
        TrivialPropertySummary address _ -> Just $ propertyID address
        UnderSpecifiedProblemSpaceVariablesSummary propertyID _ _ -> Just propertyID
        UnsoundStrictOrderConversionsSummary _ propertyID _ _ -> Just propertyID
        AllConstantNetworkInputVariablesSummary _ propertyID _ _ -> Just propertyID
        UnboundedNetworkInputVariablesSummary _ propertyID _ _ -> Just propertyID
