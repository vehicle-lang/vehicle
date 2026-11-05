module Vehicle.Data.Variable.Bound.Context.Tensor
  ( module Export,
    -- replaceTensorVariableWithStackedChildren,
    unfoldVariableIntoChildren,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Ordered qualified as OMap
import Vehicle.Compile.Resource (AbstractNetworkModality (..), NetworkTensorModality (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name.Class as Export
import Vehicle.Data.Variable.Bound.Context.Name.Core as Export
import Vehicle.Data.Variable.Bound.Context.Name.Instance as Export
import Vehicle.Data.Variable.Bound.Context.Tensor.Class as Export
import Vehicle.Data.Variable.Bound.Context.Tensor.Core as Export
import Vehicle.Data.Variable.Bound.Context.Tensor.Instance as Export
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude (FieldName (..), Identifier, Name, developerError)

-- | Given a variable `x`:
--   - if not a tensor variable errors
--   - if an element variable then returns `x`
--   - if not an element variable returns `[x!0, x!1, ..., x!n]

-- replaceTensorVariableWithStackedChildren ::
--   (MonadReadableTensorBoundContext m) =>
--   SliceVariable ->
--   m (Thunk Builtin)
-- replaceTensorVariableWithStackedChildren var = do
--   nestedVar <- lookupNestedSliceVariable var
--   case (childVariablesOf nestedVar, shapeOf nestedVar) of
--     (Nothing, []) -> return $ Forced $ Forced.VBoundVar (toLv var) []
--     (Just childVars, d : ds) ->
--       return $
--         Forced $
--           mkExpr accessStackTensor $
--             StackTensorArgs
--               { stackType = Forced IRatType,
--                 stackFirstDim = Forced $ INatLiteral d,
--                 stackRemainingDims = Forced $ mkDims ds,
--                 stackElements = flip map childVars $ \v -> Forced $ Forced.VBoundVar (toLv v) []
--               }
--     _ -> developerError "mismatched children and shape"

unfoldVariableIntoChildren ::
  (MonadReadableTensorBoundContext m) =>
  SliceVariable ->
  m (Thunk Builtin)
unfoldVariableIntoChildren var = do
  nestedVar <- lookupNestedSliceVariable var
  return $ case nestedTensorShape nestedVar of
    UniModal (TensorModality shape) -> replaceWithStackedChildren shape
    UniModal (RecordModality recordId fields) -> replaceWithRecordOfChildren recordId (NonEmpty.toList fields)
    MultiModal recordId shapes -> replaceWithRecordOfChildren recordId shapes
  where
    lv :: Lv
    lv = toLv var

    replaceWithStackedChildren :: TensorShape -> Thunk Builtin
    replaceWithStackedChildren shape = Forced $ case (getChildrenOfTensor lv shape, shape) of
      (Nothing, []) -> VBoundVar lv []
      (Just childVars, d : ds) ->
        mkExpr accessStackTensor $
          StackTensorArgs
            { stackType = Forced IRatType,
              stackFirstDim = Forced (INatLiteral d),
              stackRemainingDims = Forced (mkDims ds),
              stackElements = flip map childVars $ \v -> Forced $ VBoundVar (toLv v) []
            }
      _ -> developerError "mismatched children and shape"

    replaceWithRecordOfChildren :: Identifier -> [(Name, NetworkTensorModality TensorShape)] -> Thunk Builtin
    replaceWithRecordOfChildren recordId shapes = do
      let recordType = Forced (VFreeVar recordId [])
      let childShapes = getChildrenOfRecord lv shapes
      let childFields = OMap.fromList . flip map childShapes $ \(name, v) ->
            ( FieldName mempty name,
              Forced (flip VBoundVar [] $ toLv v)
            )
      Forced (VRecord recordType childFields)

    getChildrenOfTensor :: Lv -> TensorShape -> Maybe [NestedSliceVariable]
    getChildrenOfTensor _ [] = Nothing
    getChildrenOfTensor start (d : ds) = Just $ do
      let subSize = numberOfSliceVariablesIn ds
      let calculateChildStartingVar i = SliceVariable $ start + Lv (1 + subSize * i)
      fmap (NestedSliceVariable (UniModal (TensorModality ds)) . calculateChildStartingVar) [0 .. d - 1]

    getChildrenOfRecord :: Lv -> [(Name, NetworkTensorModality TensorShape)] -> [(Name, NestedSliceVariable)]
    getChildrenOfRecord _ [] = []
    getChildrenOfRecord start ((field, shape) : rest) = do
      let first = (field, NestedSliceVariable (UniModal shape) $ SliceVariable start)
      let nextStart = start + Lv (numberOfSliceVariablesIn . equivalentShapeOf $ shape)
      first : getChildrenOfRecord nextStart rest
