module Vehicle.Prelude.Warning
  ( CompileWarning (..),
    SummarisedCompileWarning (..),
    groupWarnings,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Ordered (OMap)
import Data.Map.Ordered qualified as OMap (empty, singleton, toAscList, unionWithR)
import Data.Set (Set)
import Data.Set qualified as Set (insert, toList)
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
  | AllConstantNetworkInputVars QueryFormatID PropertyAddress
  | BoundsOnlyQuantifier PropertyName Name

data SummarisedCompileWarning
  = UnusedResourcesSummary ExternalResource (Set Name)
  | TrivialPropertySummary PropertyAddress Bool
  | UnderSpecifiedProblemSpaceVariablesSummary PropertyName (NonEmpty (Name, TensorIndices))
  | UnsoundStrictOrderConversionsSummary QueryFormatID PropertyName Int
  | AllConstantNetworkInputVariablesSummary QueryFormatID PropertyName (NonEmpty TensorIndices)
  | BoundsOnlyQuantifierSummary PropertyName Name

--------------------------------------------------------------------------------
-- Combinable compile warnings

data CombiningState = CombiningState
  { uniqueWarnings :: [SummarisedCompileWarning],
    underSpecifiedProblemSpaceVars :: OMap PropertyName (NonEmpty (Name, TensorIndices)),
    unsoundStrictnessConversions :: OMap (QueryFormatID, PropertyName) Int,
    allConstantNetworkInputVars :: OMap (QueryFormatID, PropertyName) (NonEmpty TensorIndices),
    allBoundsOnlyQuantifier :: Set (PropertyName, Name)
  }

emptyState :: CombiningState
emptyState =
  CombiningState
    { uniqueWarnings = mempty,
      underSpecifiedProblemSpaceVars = OMap.empty,
      unsoundStrictnessConversions = OMap.empty,
      allConstantNetworkInputVars = OMap.empty,
      allBoundsOnlyQuantifier = mempty
    }

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
      { underSpecifiedProblemSpaceVars = orderedInsertWith (<>) (propertyName, [(var, propertyIndices)]) underSpecifiedProblemSpaceVars,
        ..
      }
  UnsoundStrictOrderConversion queryFormat (QueryAddress (PropertyAddress {..}) _queryID) ->
    CombiningState
      { unsoundStrictnessConversions = orderedInsertWith (+) ((queryFormat, propertyName), 1) unsoundStrictnessConversions,
        ..
      }
  AllConstantNetworkInputVars queryFormat PropertyAddress {..} ->
    CombiningState
      { allConstantNetworkInputVars =
          orderedInsertWith (<>) ((queryFormat, propertyName), [propertyIndices]) allConstantNetworkInputVars,
        ..
      }
  BoundsOnlyQuantifier ident varName ->
    CombiningState
      { allBoundsOnlyQuantifier =
          Set.insert (ident, varName) allBoundsOnlyQuantifier,
        ..
      }

groupWarnings :: [CompileWarning] -> [SummarisedCompileWarning]
groupWarnings warnings = stateToWarnings $ foldl addWarningToState emptyState warnings

stateToWarnings :: CombiningState -> [SummarisedCompileWarning]
stateToWarnings CombiningState {..} =
  uniqueWarnings
    <> fmap combineUnderSpecifiedProblemSpaceVars (OMap.toAscList underSpecifiedProblemSpaceVars)
    <> fmap combineUnsoundStrictnessConversions (OMap.toAscList unsoundStrictnessConversions)
    <> fmap combineAllConstantNetworkInputVars (OMap.toAscList allConstantNetworkInputVars)
    <> fmap combineAllBoundsOnlyQuantifier (Set.toList allBoundsOnlyQuantifier)

combineUnderSpecifiedProblemSpaceVars :: (PropertyName, NonEmpty (Name, TensorIndices)) -> SummarisedCompileWarning
combineUnderSpecifiedProblemSpaceVars (property, vars) = UnderSpecifiedProblemSpaceVariablesSummary property vars

combineUnsoundStrictnessConversions :: ((QueryFormatID, PropertyName), Int) -> SummarisedCompileWarning
combineUnsoundStrictnessConversions ((queryFormatID, property), number) =
  UnsoundStrictOrderConversionsSummary queryFormatID property number

combineAllConstantNetworkInputVars :: ((QueryFormatID, PropertyName), NonEmpty TensorIndices) -> SummarisedCompileWarning
combineAllConstantNetworkInputVars ((queryFormatID, property), queries) =
  AllConstantNetworkInputVariablesSummary queryFormatID property queries

combineAllBoundsOnlyQuantifier :: (PropertyName, Name) -> SummarisedCompileWarning
combineAllBoundsOnlyQuantifier (propertyName, quantifierName) =
  BoundsOnlyQuantifierSummary propertyName quantifierName

orderedInsertWith :: (Ord k) => (a -> a -> a) -> (k, a) -> OMap k a -> OMap k a
orderedInsertWith f (k, a) = OMap.unionWithR (const f) (OMap.singleton (k, a))
