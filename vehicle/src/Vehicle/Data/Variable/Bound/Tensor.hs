module Vehicle.Data.Variable.Bound.Tensor
  ( NestedSliceVariable,
    mkNestedSliceVariable,
    childVariablesOf,
    elementVariablesOf,
    nestedChildVariables,
    nestedVariablesSize,
    isSliceOf,
    findIndicesAndShape,
    variableNamesForAllSlices,
    NestedTensorVariableCtx,
    lookupTensorVariable,
    lookupTensorVariableAndName,
    appendTensorVariableToNestedCtx,
    nestedCtxToNameCtx,
    emptyNestedCtx,
    findCorrespondingSliceVariable,
    findCorrespondingTensorVariable,
    findCorrespondingTensorVariables,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.List (find, sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics (Generic)
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Core (CompleteNamedBoundCtx, GenericBoundCtx)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- NestedSliceVariable

-- This represents a pyramid, e.g. for a tensor shape of [2,2,3] the pyramid
-- will represent 19 variables:
--  - 1 variable x representing the tensor
--  - 2 variables x!0 and x!1 representing the first dimensions
--  - 4 variables x!0!0, x!0!1, x!1!0, x!1!1 represnting the second dimensions
--  - 12 variables ... representing the element dimensions
--
-- We store it like this in order to maximise space efficiency.
data NestedSliceVariable = NestedSliceVariable
  { nestedTensorShape :: TensorShape,
    nestedStartingVariable :: SliceVariable
  }
  deriving (Show, Eq, Ord, Generic)

mkNestedSliceVariable :: TensorShape -> SliceVariable -> NestedSliceVariable
mkNestedSliceVariable = NestedSliceVariable

instance NFData NestedSliceVariable

instance ToJSON NestedSliceVariable

instance FromJSON NestedSliceVariable

instance Pretty NestedSliceVariable where
  pretty (NestedSliceVariable shape l) = pretty (toLv l) <> ":" <+> pretty shape

instance HasShape NestedSliceVariable where
  shapeOf = nestedTensorShape

instance VariableLike NestedSliceVariable where
  toLv = toLv . nestedStartingVariable

instance SliceVariableLike NestedSliceVariable where
  toSliceVar = nestedStartingVariable

nestedVariablesSize :: TensorShape -> Int
nestedVariablesSize shape = sum $ NonEmpty.scanl (*) 1 shape

childVariablesOf :: NestedSliceVariable -> Maybe [NestedSliceVariable]
childVariablesOf (NestedSliceVariable shape startingVar) = case shape of
  [] -> Nothing
  d : ds -> Just $ do
    let subSize = nestedVariablesSize ds
    let calculateChildStartingVar i = SliceVariable $ toLv startingVar + Lv (1 + subSize * i)
    fmap (NestedSliceVariable ds . calculateChildStartingVar) [0 .. d - 1]

nestedChildVariables :: NestedSliceVariable -> Maybe [SliceVariable]
nestedChildVariables = fmap (fmap nestedStartingVariable) . childVariablesOf

elementVariablesOf :: NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
elementVariablesOf = go mempty
  where
    go :: TensorIndices -> NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
    go indices var = case childVariablesOf var of
      Nothing -> [(coerce (nestedStartingVariable var), reverse indices)]
      Just childVars -> concatMap (\(v, index) -> go (index : indices) v) $ zip childVars [0 ..]

--------------------------------------------------------------------------------
-- Tensor variables

data NestedTensorVariableCtx = NestedTensorVariableCtx
  { _ctx :: GenericBoundCtx (NestedSliceVariable, Name),
    _name :: CompleteNamedBoundCtx,
    _totalSize :: Int
  }

instance Pretty NestedTensorVariableCtx where
  pretty (NestedTensorVariableCtx _ n s) = pretty n <+> pretty s <+> pretty (length n)

emptyNestedCtx :: NestedTensorVariableCtx
emptyNestedCtx = NestedTensorVariableCtx mempty mempty 0

nestedCtxToNameCtx :: NestedTensorVariableCtx -> CompleteNamedBoundCtx
nestedCtxToNameCtx (NestedTensorVariableCtx _ nameCtx _) = nameCtx

lookupTensorVariableAndName ::
  NestedTensorVariableCtx ->
  TensorVariable ->
  (NestedSliceVariable, Name)
lookupTensorVariableAndName (NestedTensorVariableCtx ctx _ _) var = do
  case find (\(v, _) -> nestedStartingVariable v == toSliceVar var) ctx of
    Nothing -> developerError $ "Missing nested tensor variable" <+> pretty (toLv var)
    Just result -> result

lookupTensorVariable ::
  (TensorVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
lookupTensorVariable ctx var =
  fst $ lookupTensorVariableAndName ctx (toTensorVar var)

findCorrespondingSliceVariable ::
  (HasCallStack, SliceVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
findCorrespondingSliceVariable ctx var = do
  let parentVar = findCorrespondingTensorVariable ctx var
  let (_, shape) = findIndicesAndShape parentVar (toSliceVar var)
  NestedSliceVariable shape (toSliceVar var)

findIndicesAndShape :: NestedSliceVariable -> SliceVariable -> (TensorIndices, TensorShape)
findIndicesAndShape (NestedSliceVariable shape lv) var = go mempty shape (unLv $ toLv var - toLv lv)
  where
    go :: TensorIndices -> TensorShape -> Int -> (TensorIndices, TensorShape)
    go indices ds 0 = (reverse indices, ds)
    go _indices [] _flatIndex = developerError "Malformed shape and index"
    go indices (_d : ds) flatIndex = do
      let newIndex = (flatIndex - 1) `div` nestedVariablesSize ds
      let newFlatIndex = (flatIndex - 1) `rem` nestedVariablesSize ds
      go (newIndex : indices) ds newFlatIndex

isSliceOf :: Lv -> NestedSliceVariable -> Bool
isSliceOf lv (NestedSliceVariable shape startLv) =
  toLv startLv <= lv && lv < toLv startLv + Lv (nestedVariablesSize shape)

findCorrespondingTensorVariable ::
  (SliceVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
findCorrespondingTensorVariable ctx var =
  -- TODO turn this into a binary search for added efficiency?
  case findCorrespondingTensorSliceVariables ctx (Set.singleton (toSliceVar var)) of
    [v] -> v
    _ -> developerError "Missing variable"

-- | Given a set of variables representing the slices of a given set of tensors
-- returns the set of tensor variables that those slices are taken from.
findCorrespondingTensorVariables ::
  NestedTensorVariableCtx ->
  Set SliceVariable ->
  Set TensorVariable
findCorrespondingTensorVariables ctx sliceVars = do
  let result = findCorrespondingTensorSliceVariables ctx sliceVars
  Set.fromList $ fmap (coerce . nestedStartingVariable) result

findCorrespondingTensorSliceVariables ::
  NestedTensorVariableCtx ->
  Set SliceVariable ->
  [NestedSliceVariable]
findCorrespondingTensorSliceVariables (NestedTensorVariableCtx wholeCtx _ _) vars = do
  let sortedVarList = sortBy (comparing Down) (Set.toList vars)
  go wholeCtx sortedVarList
  where
    go :: GenericBoundCtx (NestedSliceVariable, Name) -> [SliceVariable] -> [NestedSliceVariable]
    go [] _ = []
    go _ [] = []
    go ((tensorVar, _) : ctx) (v : vs) = do
      let startPoint = toLv tensorVar
      let endPoint = startPoint + Lv (nestedVariablesSize $ shapeOf tensorVar)
      if toLv v >= endPoint
        then developerError "Incorrectly sorted slice variables"
        else
          if toLv v < startPoint
            then go ctx (v : vs)
            else do
              let newVars = dropWhile (\u -> toLv u >= startPoint) vs
              tensorVar : go ctx newVars

appendTensorVariableToNestedCtx ::
  NestedTensorVariableCtx ->
  (Name, TensorShape) ->
  (SliceVariable, NestedTensorVariableCtx)
appendTensorVariableToNestedCtx (NestedTensorVariableCtx ctx nameCtx totalSize) (name, shape) = do
  let var = NestedSliceVariable shape (SliceVariable $ Lv totalSize)
  let newCtx = (var, name) : ctx
  let newNameCtx = variableNamesForAllSlices name shape <> nameCtx
  let newTotalSize = totalSize + nestedVariablesSize shape
  let newNestedCtx = NestedTensorVariableCtx newCtx newNameCtx newTotalSize
  (toSliceVar var, newNestedCtx)

variableNamesForAllSlices :: Name -> TensorShape -> [Name]
variableNamesForAllSlices parentName shape = reverse (fmap mkName (allIndicesForShape shape))
  where
    mkName :: TensorIndices -> Name
    mkName indices = parentName <> Text.pack (showTensorIndices indices)
