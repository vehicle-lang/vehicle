module Vehicle.Prelude.Warning
  ( CompileWarning (..),
    SummarisedCompileWarning (..),
    groupWarnings,
  )
where

import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty, sort)
import Data.Map (Map)
import Data.Map qualified as Map (insertWith, singleton, toList, unionWith)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.Builtin.Loss.Core (LossBuiltin)
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Expr.Value
import Vehicle.Data.QuantifiedVariable
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Resource (ExternalResource)
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Non-unique compile warnings

data CompileWarning
  = UnusedResources ExternalResource (Set Name)
  | TrivialProperty PropertyAddress Bool
  | UnderSpecifiedProblemSpaceVar PropertyAddress UserRationalVariable
  | UnsoundStrictOrderConversion QueryFormatID QueryAddress
  | AllConstantNetworkInputVars QueryFormatID QueryAddress
  | UnboundedNetworkInputVariables QueryFormatID QueryAddress MetaNetwork [(NetworkRationalVariable, UnderConstrainedVariableStatus)]
  | InefficientTensorCode Name LossBuiltin NamedBoundCtx (NFValue TensorBuiltin)

data SummarisedCompileWarning
  = UnusedResourcesSummary ExternalResource (Set Name)
  | TrivialPropertySummary PropertyAddress Bool
  | UnderSpecifiedProblemSpaceVariablesSummary PropertyAddress (Set UserRationalVariable)
  | UnsoundStrictOrderConversionsSummary QueryFormatID PropertyAddress Int
  | AllConstantNetworkInputVariablesSummary QueryFormatID PropertyAddress (NonEmpty QueryID)
  | UnboundedNetworkInputVariablesSummary QueryFormatID PropertyAddress [(NonEmpty QueryID, [(NetworkRationalVariable, UnderConstrainedVariableStatus)])]
  | InefficientTensorCodeSummary Name (Either BuiltinFunction StdLibFunction) NamedBoundCtx (NFValue TensorBuiltin)

--------------------------------------------------------------------------------
-- Combinable compile warnings

type UnderConstrainedSignature = (MetaNetwork, [(NetworkRationalVariable, UnderConstrainedVariableStatus)])

data CombiningState = CombiningState
  { uniqueWarnings :: [SummarisedCompileWarning],
    underSpecifiedProblemSpaceVars :: Map PropertyAddress (Set UserRationalVariable),
    unsoundStrictnessConversions :: Map (QueryFormatID, PropertyAddress) Int,
    allConstantNetworkInputVars :: Map (QueryFormatID, PropertyAddress) (NonEmpty QueryID),
    unboundedNetworkInputs :: Map (QueryFormatID, PropertyAddress) (Map UnderConstrainedSignature (NonEmpty QueryID)),
    inefficientTensorCode :: Map Name [(LossBuiltin, NamedBoundCtx, NFValue TensorBuiltin)]
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
  UnderSpecifiedProblemSpaceVar property var ->
    CombiningState
      { underSpecifiedProblemSpaceVars = Map.insertWith (<>) property (Set.singleton var) underSpecifiedProblemSpaceVars,
        ..
      }
  UnsoundStrictOrderConversion queryFormat (propertyAddress, _queryID) ->
    CombiningState
      { unsoundStrictnessConversions = Map.insertWith (+) (queryFormat, propertyAddress) 1 unsoundStrictnessConversions,
        ..
      }
  AllConstantNetworkInputVars queryFormat (propertyAddress, queryID) ->
    CombiningState
      { allConstantNetworkInputVars = Map.insertWith (<>) (queryFormat, propertyAddress) [queryID] allConstantNetworkInputVars,
        ..
      }
  UnboundedNetworkInputVariables queryFormat (propertyAddress, queryID) metaNetwork vars ->
    CombiningState
      { unboundedNetworkInputs = Map.insertWith (Map.unionWith (<>)) (queryFormat, propertyAddress) (Map.singleton (metaNetwork, vars) [queryID]) unboundedNetworkInputs,
        ..
      }
  InefficientTensorCode name fun ctx value ->
    CombiningState
      { inefficientTensorCode = Map.insertWith (<>) name [(fun, ctx, value)] inefficientTensorCode,
        ..
      }

groupWarnings :: [CompileWarning] -> [SummarisedCompileWarning]
groupWarnings warnings = stateToWarnings $ foldl addWarningToState emptyState warnings

stateToWarnings :: CombiningState -> [SummarisedCompileWarning]
stateToWarnings CombiningState {..} =
  sortBy compareWarning $
    do
      uniqueWarnings
      <> fmap combineUnderSpecifiedProblemSpaceVars (Map.toList underSpecifiedProblemSpaceVars)
      <> fmap combineUnsoundStrictnessConversions (Map.toList unsoundStrictnessConversions)
      <> fmap combineAllConstantNetworkInputVars (Map.toList allConstantNetworkInputVars)
      <> fmap combineUnboundedNetworkInputVars (Map.toList unboundedNetworkInputs)

combineUnderSpecifiedProblemSpaceVars :: (PropertyAddress, Set UserRationalVariable) -> SummarisedCompileWarning
combineUnderSpecifiedProblemSpaceVars (property, vars) = UnderSpecifiedProblemSpaceVariablesSummary property vars

combineUnsoundStrictnessConversions :: ((QueryFormatID, PropertyAddress), Int) -> SummarisedCompileWarning
combineUnsoundStrictnessConversions ((queryFormatID, property), number) = UnsoundStrictOrderConversionsSummary queryFormatID property number

combineAllConstantNetworkInputVars :: ((QueryFormatID, PropertyAddress), NonEmpty QueryID) -> SummarisedCompileWarning
combineAllConstantNetworkInputVars ((queryFormatID, property), queries) = AllConstantNetworkInputVariablesSummary queryFormatID property (sort queries)

combineUnboundedNetworkInputVars :: ((QueryFormatID, PropertyAddress), Map UnderConstrainedSignature (NonEmpty QueryID)) -> SummarisedCompileWarning
combineUnboundedNetworkInputVars ((queryFormatID, property), constraintsBySignature) = do
  let result = (\((_metaNetwork, constraints), queries) -> (queries, constraints)) <$> Map.toList constraintsBySignature
  UnboundedNetworkInputVariablesSummary queryFormatID property result

compareWarning :: SummarisedCompileWarning -> SummarisedCompileWarning -> Ordering
compareWarning w1 w2 = compare (warningPropertyId w1) (warningPropertyId w2)
  where
    warningPropertyId :: SummarisedCompileWarning -> Maybe PropertyID
    warningPropertyId w =
      propertyID <$> case w of
        UnusedResourcesSummary {} -> Nothing
        TrivialPropertySummary address _ -> Just address
        UnderSpecifiedProblemSpaceVariablesSummary address _ -> Just address
        UnsoundStrictOrderConversionsSummary _ address _ -> Just address
        AllConstantNetworkInputVariablesSummary _ address _ -> Just address
        UnboundedNetworkInputVariablesSummary _ address _ -> Just address
        InefficientTensorCodeSummary {} -> Nothing
