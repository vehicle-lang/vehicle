module Vehicle.Compile.Property
  ( traverseMultiProperty,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadTrans (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, TensorShape, unstack)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification

-- TODO move somewhere else more reusable?
traverseMultiProperty ::
  forall m a.
  (Monad m) =>
  (PropertyAddress -> Value Builtin -> m a) ->
  PropertyID ->
  Name ->
  Value Builtin ->
  Value Builtin ->
  m (Either MultiPropertyTraveralError (MultiProperty a))
traverseMultiProperty compileProp propertyID propertyName declType declBody = runExceptT (go declType mempty declBody)
  where
    go :: VType Builtin -> TensorIndices -> Value Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    go typ indices body = case toTypeValue typ of
      VVectorType elemType dimValue -> case getDim dimValue of
        Nothing -> throwError $ UnsupportedVectorDimension dimValue
        Just dim -> goVector elemType dim indices body
      VBoolTensorType dimsValue -> case getDims dimsValue of
        Nothing -> throwError $ UnsupportedTensorDimensions dimsValue
        Just dims -> goTensor dims indices body
      _ -> throwError $ UnreducableType typ

    goVector :: VType Builtin -> Int -> TensorIndices -> Value Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    goVector typ _dim indices value = case value of
      -- TODO refactor in terms of a VectorValue class to `TypedValue` module
      (getExpr accessVecLit -> Just args) -> do
        let es' = zip [0 :: Int ..] $ vecLitElements args
        MultiProperty <$> traverse (\(i, e) -> go typ (i : indices) e) es'
      _ -> throwError $ UnsupportedVectorValue value

    goTensor :: TensorShape -> TensorIndices -> Value Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    goTensor dims indices value = case dims of
      [] -> do
        let address = PropertyAddress propertyID propertyName indices
        SingleProperty <$> lift (compileProp address value)
      _d : ds -> case toBoolTensorValue value of
        VBoolTensorLiteral bs -> do
          let es' = zip [0 :: Int ..] (fromBoolTensorValue . VBoolTensorLiteral <$> unstack bs)
          MultiProperty <$> traverse (\(i, e) -> goTensor ds (i : indices) e) es'
        VBoolStackTensor args -> do
          let es' = zip [0 :: Int ..] $ stackElements args
          MultiProperty <$> traverse (\(i, e) -> goTensor ds (i : indices) e) es'
        _ -> throwError $ UnreducableTensorValue value
