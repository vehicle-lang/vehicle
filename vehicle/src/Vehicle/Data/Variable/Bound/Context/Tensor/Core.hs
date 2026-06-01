module Vehicle.Data.Variable.Bound.Context.Tensor.Core where

import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
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

-- | Given a set of variables in the extended tensor context, returns a list
-- of pairs of (Lv of the variable in the original context, top-level tensor
-- variable if the variable is a tensor variable).
findCorrespondingVariableInOriginalCtx ::
  (VariableLike var) =>
  NestedTensorVariableCtx ->
  Set var ->
  [(OriginalLv, Maybe NestedSliceVariable)]
findCorrespondingVariableInOriginalCtx (NestedTensorVariableCtx wholeCtx nameCtx) vars = do
  let sortedVarList = sortBy (comparing Down) (Set.toList vars)
  -- Returns Lvs in *shrunken* (binder-count) space. NBE-emitted VBoundVars
  -- live in nameCtx space (one Lv per slice slot); we walk the binder ctx
  -- right-to-left, mapping each input var's nameCtx-Lv to the originating
  -- binder's shrunken-Lv. The shrunken Lv for the binder at walking
  -- position `lv` (0=leftmost/outermost) is `ctxLen - 1 - lv`.
  let ctxLen = boundCtxLv wholeCtx
  go 0 ctxLen (boundCtxLv nameCtx) wholeCtx sortedVarList
  where
    go :: (VariableLike var) => Lv -> Lv -> Lv -> GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable) -> [var] -> [(OriginalLv, Maybe NestedSliceVariable)]
    go _ _ _ [] _ = []
    go _ _ _ _ [] = []
    go lv ctxLen topSlice ((_binder, maybeTensorVar) : ctx) (v : vs) = case maybeTensorVar of
      Nothing -> do
        -- Non-tensor binder: 1 slot in shrunken AND 1 slot in nameCtx.
        let mySlice = topSlice - 1
        let shrunkenLv = ctxLen - 1 - lv
        if mySlice == toLv v
          then (shrunkenLv, Nothing) : go (lv + 1) ctxLen mySlice ctx vs
          else go (lv + 1) ctxLen mySlice ctx (v : vs)
      Just tensorVar -> do
        let startPoint = toLv tensorVar
        let endPoint = startPoint + Lv (numberOfSliceVariablesIn $ shapeOf tensorVar)
        let shrunkenLv = ctxLen - 1 - lv
        if toLv v >= endPoint
          then developerError "Incorrectly sorted slice variables"
          else
            if toLv v < startPoint
              then go (lv + 1) ctxLen startPoint ctx (v : vs)
              else do
                let newVars = dropWhile (\u -> toLv u >= startPoint) vs
                -- Parent tensor binder maps to its shrunken-Lv; slice
                -- indices are derived from the nameCtx offset by callers.
                (shrunkenLv, Just tensorVar) : go (lv + 1) ctxLen startPoint ctx newVars

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
  let newNameCtx = variableNamesForAllSlices (fst $ getNamedBinderInfo binder) shape <> nameCtx
  NestedTensorVariableCtx newCtx newNameCtx

variableNamesForAllSlices :: Name -> TensorShape -> [Name]
variableNamesForAllSlices parentName shape = reverse (fmap mkName (allIndicesForShape shape))
  where
    mkName :: TensorIndices -> Name
    mkName indices = parentName <> Text.pack (showTensorIndices indices)
