module Vehicle.Data.Variable.Bound.Context.Tensor.Core where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
  let sortedVarList = sort (Set.toList vars)
  go (reverse wholeCtx) sortedVarList (0, 0)
  where
    go :: (VariableLike var) => GenericBoundCtx (GenericBinder (), Maybe NestedSliceVariable) -> [var] -> (OriginalLv, Lv) -> [(Lv, Maybe NestedSliceVariable)]
    go _ [] _ = []
    go [] (_v : _vs) _ = developerError "variables not found in nested tensor context"
    go ((_binder, maybeTensorVar) : ctx) (v : vs) (currentOriginalLv, currentLv) = do
      case maybeTensorVar of
        Nothing
          | toLv v == currentLv -> (currentOriginalLv, Nothing) : go ctx vs (currentOriginalLv + 1, currentLv + 1)
          | otherwise -> go ctx (v : vs) (currentOriginalLv + 1, currentLv + 1)
        Just tensorVar -> do
          let endPoint = currentLv + Lv (numberOfSliceVariablesIn $ shapeOf tensorVar)
          let (found, notFound) = span (\x -> toLv x < endPoint) (v : vs)
          fmap (const (currentOriginalLv, Just tensorVar)) found ++ go ctx notFound (currentOriginalLv + 1, endPoint)

-- | Looks up a level in the tensor variable context. It returns
--    1. The level in the original context without any slice variables.
--    2. If a slice variable, then the parent tensor variable as well as the slice variable it represents.
findOriginalVariableInCtx ::
  NestedTensorVariableCtx ->
  Lv ->
  (OriginalLv, Maybe (NestedSliceVariable, SliceVariable))
findOriginalVariableInCtx ctx lv =
  -- TODO turn this into a binary search for added efficiency?
  case findCorrespondingVariableInOriginalCtx ctx (Set.singleton lv) of
    [(originalCtxLv, maybeParentVar)] -> (originalCtxLv, fmap (,SliceVariable lv) maybeParentVar)
    _ -> developerError "could not find variable in nested context"

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
