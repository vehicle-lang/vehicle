module Vehicle.Data.Variable.Bound.Context.Tensor.Core where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
import Vehicle.Compile.Resource (NetworkModality (..))
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Core (GenericBoundCtx, boundCtxLv)
import Vehicle.Data.Variable.Bound.Context.Generic (BoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

-- | The level of a variable in the original context (i.e. without all the
-- tensor variables have being expanded out).
type OriginalLv = Lv

data NestedTensorVariableCtx = NestedTensorVariableCtx
  { nestedVariableCtx :: GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable),
    nestedVariableCtxNames :: CompleteNamedBoundCtx
  }

instance Pretty NestedTensorVariableCtx where
  pretty (NestedTensorVariableCtx _ n) = pretty n <+> pretty (length n)

originalCtx :: NestedTensorVariableCtx -> BoundCtx ()
originalCtx = fmap fst . nestedVariableCtx

emptyNestedCtx :: NestedTensorVariableCtx
emptyNestedCtx = NestedTensorVariableCtx mempty mempty

-- | Given a set of variables in the extended tensor context,
-- returns a pair consisting of the Lv of the variable in the original context
-- and the top-level tensor variable if the variable is a tensor variable.
findCorrespondingVariableInOriginalCtx ::
  (VariableLike var) =>
  NestedTensorVariableCtx ->
  Set var ->
  [(OriginalLv, Maybe NestedSliceVariable)]
findCorrespondingVariableInOriginalCtx (NestedTensorVariableCtx wholeCtx _) vars = do
  let sortedVarList = sortBy (comparing Down) (Set.toList vars)
  go 0 wholeCtx sortedVarList
  where
    go :: (VariableLike var) => OriginalLv -> GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable) -> [var] -> [(Lv, Maybe NestedSliceVariable)]
    go _ [] _ = []
    go _ _ [] = []
    go lv ((_binder, maybeTensorVar) : ctx) (v : vs) = case maybeTensorVar of
      Nothing
        | lv == toLv v -> (lv, Nothing) : go (lv + 1) ctx (v : vs)
        | otherwise -> go (lv + 1) ctx (v : vs)
      Just tensorVar -> do
        let startPoint = toLv tensorVar
        let endPoint = startPoint + Lv (numberOfSliceVariablesIn $ shapeOf tensorVar)
        if toLv v >= endPoint
          then developerError "Incorrectly sorted slice variables"
          else
            if toLv v < startPoint
              then go (lv + 1) ctx (v : vs)
              else do
                let newVars = dropWhile (\u -> toLv u >= startPoint) vs
                (lv, Just tensorVar) : go (lv + 1) ctx newVars

appendNonTensorVariableToNestedCtx :: GenericBinder () -> NestedTensorVariableCtx -> NestedTensorVariableCtx
appendNonTensorVariableToNestedCtx binder (NestedTensorVariableCtx ctx nameCtx) = do
  NestedTensorVariableCtx
    { nestedVariableCtx = (binder, Nothing) : ctx,
      nestedVariableCtxNames = fromMaybe "_" (nameOf binder) : nameCtx
    }

appendTensorVariableToNestedCtx ::
  GenericBinder () ->
  NetworkModality TensorShape ->
  NestedTensorVariableCtx ->
  NestedTensorVariableCtx
appendTensorVariableToNestedCtx binder shape (NestedTensorVariableCtx ctx nameCtx) = do
  let var = NestedSliceVariable shape (SliceVariable $ boundCtxLv nameCtx)
  let newCtx = (binder, Just var) : ctx
  let newNameCtx = variableNamesForAllSlices (fst $ getNamedBinderInfo binder) shape <> nameCtx
  NestedTensorVariableCtx newCtx newNameCtx

variableNamesForAllSlices :: Name -> NetworkModality TensorShape -> [Name]
variableNamesForAllSlices parentName = \case
  UniModal shape -> reverse (fmap mkName (allIndicesForShape shape))
  MultiModal _shapes -> error "MultiModal IO is not implemented yet"
  where
    mkName :: TensorIndices -> Name
    mkName indices = parentName <> Text.pack (showTensorIndices indices)
