module Vehicle.Data.Variable.Bound.Context.Tensor.Core where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Core (GenericBoundCtx, boundCtxLv)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

data NestedTensorVariableCtx = NestedTensorVariableCtx
  { nestedVariableCtx :: GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable),
    nestedVariableCtxNames :: CompleteNamedBoundCtx
  }

instance Pretty NestedTensorVariableCtx where
  pretty (NestedTensorVariableCtx _ n) = pretty n <+> pretty (length n)

emptyNestedCtx :: NestedTensorVariableCtx
emptyNestedCtx = NestedTensorVariableCtx mempty mempty

findCorrespondingTensorSliceVariables ::
  NestedTensorVariableCtx ->
  Set SliceVariable ->
  [NestedSliceVariable]
findCorrespondingTensorSliceVariables (NestedTensorVariableCtx wholeCtx _) vars = do
  let sortedVarList = sortBy (comparing Down) (Set.toList vars)
  go wholeCtx sortedVarList
  where
    go :: GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable) -> [SliceVariable] -> [NestedSliceVariable]
    go [] _ = []
    go _ [] = []
    go ((_binder, maybeTensorVar) : ctx) (v : vs) = case maybeTensorVar of
      Nothing -> go ctx (v : vs)
      Just tensorVar -> do
        let startPoint = toLv tensorVar
        let endPoint = startPoint + Lv (numberOfSliceVariablesIn $ shapeOf tensorVar)
        if toLv v >= endPoint
          then developerError "Incorrectly sorted slice variables"
          else
            if toLv v < startPoint
              then go ctx (v : vs)
              else do
                let newVars = dropWhile (\u -> toLv u >= startPoint) vs
                tensorVar : go ctx newVars

appendNonTensorVariableToNestedCtx :: GenericBinder () -> NestedTensorVariableCtx -> NestedTensorVariableCtx
appendNonTensorVariableToNestedCtx binder (NestedTensorVariableCtx ctx nameCtx) = do
  NestedTensorVariableCtx
    { nestedVariableCtx = (binder, Nothing) : ctx,
      nestedVariableCtxNames = fromMaybe "_" (nameOf binder) : nameCtx
    }

appendTensorVariableToNestedCtx ::
  GenericBinder () ->
  TensorShape ->
  NestedTensorVariableCtx ->
  NestedTensorVariableCtx
appendTensorVariableToNestedCtx binder shape (NestedTensorVariableCtx ctx nameCtx) = do
  let var = NestedSliceVariable shape (SliceVariable $ boundCtxLv nameCtx)
  let newCtx = (binder, Just var) : ctx
  let newNameCtx = variableNamesForAllSlices (getBinderName binder) shape <> nameCtx
  NestedTensorVariableCtx newCtx newNameCtx

variableNamesForAllSlices :: Name -> TensorShape -> [Name]
variableNamesForAllSlices parentName shape = reverse (fmap mkName (allIndicesForShape shape))
  where
    mkName :: TensorIndices -> Name
    mkName indices = parentName <> Text.pack (showTensorIndices indices)
