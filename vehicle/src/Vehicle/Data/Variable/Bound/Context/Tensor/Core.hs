module Vehicle.Data.Variable.Bound.Context.Tensor.Core where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Vehicle.Compile.Prelude.Utils (getNamedBinderInfo)
import Vehicle.Compile.Resource (NetworkModality (..))
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Context.Core (GenericBoundCtx, boundCtxLv)
import Vehicle.Data.Variable.Bound.Context.Generic.Core (BoundCtx)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

-- | We may not be able to calculate the exact dimensions a tensor, but this
-- value represents the prefix that of the shape that is known, e.g.
-- [1,2,n] would have a prefix of [1,2]
type KnownPrefixOfTensorShape = TensorShape

-- | The level of a variable in the original context (i.e. without all the
-- tensor variables have being expanded out).
type OriginalLv = Lv

data NestedTensorVariableCtx = NestedTensorVariableCtx
  { nestedVariableCtx :: GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable),
    nestedVariableCtxNames :: CompleteNamedBoundCtx,
    -- | An index for efficiently looking up which variable in the original
    -- context a level in the nested context belongs to. Each entry is keyed
    -- by the first level in the nested context occupied by that variable,
    -- and stores the variable's level in the original context and, if it
    -- is a tensor variable, the tensor variable itself.
    nestedVariableIndex :: Map Lv (OriginalLv, Maybe NestedSliceVariable)
  }

instance Pretty NestedTensorVariableCtx where
  pretty (NestedTensorVariableCtx _ n _) = pretty n <+> pretty (length n)

originalCtx :: NestedTensorVariableCtx -> BoundCtx ()
originalCtx = fmap fst . nestedVariableCtx

emptyNestedCtx :: NestedTensorVariableCtx
emptyNestedCtx = NestedTensorVariableCtx mempty mempty mempty

nestedTensorVariableCtxLength :: NestedTensorVariableCtx -> Lv
nestedTensorVariableCtxLength = Lv . length . nestedVariableCtxNames

-- | Looks up the entry of the variable in the original context that the
-- provided level in the nested context belongs to.
lookupInNestedVariableIndex :: NestedTensorVariableCtx -> Lv -> (OriginalLv, Maybe NestedSliceVariable)
lookupInNestedVariableIndex ctx lv =
  case Map.lookupLE lv (nestedVariableIndex ctx) of
    Just (startLv, entry@(_, maybeTensorVar))
      | lv < startLv + nestedVariableWidth maybeTensorVar -> entry
    _ -> developerError "variables not found in nested tensor context"

-- | The number of levels in the nested context occupied by a variable.
nestedVariableWidth :: Maybe NestedSliceVariable -> Lv
nestedVariableWidth = \case
  Nothing -> 1
  Just tensorVar -> Lv (numberOfSliceVariablesIn $ shapeOf tensorVar)

-- | Adds a new variable to the end of the index.
appendToNestedVariableIndex ::
  Maybe NestedSliceVariable ->
  Map Lv (OriginalLv, Maybe NestedSliceVariable) ->
  Map Lv (OriginalLv, Maybe NestedSliceVariable)
appendToNestedVariableIndex maybeTensorVar index = do
  let (nextLv, nextOriginalLv) = case Map.lookupMax index of
        Nothing -> (0, 0)
        Just (startLv, (originalLv, lastVar)) -> (startLv + nestedVariableWidth lastVar, originalLv + 1)
  Map.insert nextLv (nextOriginalLv, maybeTensorVar) index

-- | Looks up a level in the tensor variable context. It returns
--    1. The level in the original context without any slice variables.
--    2. If a slice variable, then the parent tensor variable as well as the slice variable it represents.
findOriginalVariableInCtx ::
  NestedTensorVariableCtx ->
  Lv ->
  (OriginalLv, Maybe (NestedSliceVariable, SliceVariable))
findOriginalVariableInCtx ctx lv = do
  let (originalCtxLv, maybeParentVar) = lookupInNestedVariableIndex ctx lv
  (originalCtxLv, fmap (,SliceVariable lv) maybeParentVar)

appendNonTensorVariableToNestedCtx :: GenericBinder () -> NestedTensorVariableCtx -> NestedTensorVariableCtx
appendNonTensorVariableToNestedCtx binder (NestedTensorVariableCtx ctx nameCtx index) = do
  NestedTensorVariableCtx
    { nestedVariableCtx = (binder, Nothing) : ctx,
      nestedVariableCtxNames = fromMaybe "_" (nameOf binder) : nameCtx,
      nestedVariableIndex = appendToNestedVariableIndex Nothing index
    }

appendTensorVariableToNestedCtx ::
  GenericBinder () ->
  NetworkModality TensorShape ->
  NestedTensorVariableCtx ->
  NestedTensorVariableCtx
appendTensorVariableToNestedCtx binder shape (NestedTensorVariableCtx ctx nameCtx index) = do
  let var = NestedSliceVariable shape (SliceVariable $ boundCtxLv nameCtx)
  let newCtx = (binder, Just var) : ctx
  let newNameCtx = variableNamesForAllSlices (fst $ getNamedBinderInfo binder) shape <> nameCtx
  let newIndex = appendToNestedVariableIndex (Just var) index
  NestedTensorVariableCtx newCtx newNameCtx newIndex

variableNamesForAllSlices :: Name -> NetworkModality TensorShape -> [Name]
variableNamesForAllSlices parentName = \case
  UniModal shape -> reverse (fmap mkName (allIndicesForShape shape))
  MultiModal _shapes -> error "MultiModal IO is not implemented yet"
  where
    mkName :: TensorIndices -> Name
    mkName indices = parentName <> Text.pack (showTensorIndices indices)
