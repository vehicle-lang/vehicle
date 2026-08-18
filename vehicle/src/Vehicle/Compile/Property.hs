module Vehicle.Compile.Property
  ( traverseMultiProperty,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (MonadTrans (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalForeachVector, getDim, getDims)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Unblock (forceEval)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (TensorIndices, TensorShape, unstack)
import Vehicle.Data.Variable.Bound.Context.Name.Instance (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification

-- TODO move somewhere else more reusable?
traverseMultiProperty ::
  forall m a.
  (MonadFreeContext Builtin m) =>
  (PropertyAddress -> Thunk Builtin -> m a) ->
  Name ->
  Thunk Builtin ->
  Thunk Builtin ->
  m (Either MultiPropertyTraveralError (MultiProperty a))
traverseMultiProperty compileProp propertyName declType declBody =
  runExceptT (go declType mempty declBody)
  where
    go :: UnforcedType Builtin -> TensorIndices -> Thunk Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    go typ indices body = do
      forcedType <- runFreshNameBoundContextT $ forceThunk typ
      case toTypeValue forcedType of
        VVectorType elemType dimValue -> do
          maybeDim <- runFreshNameBoundContextT $ getDim dimValue
          case maybeDim of
            Nothing -> throwError $ UnsupportedVectorDimension dimValue
            Just dim -> goVector elemType dim indices body
        VTensorType _elemType dimsValue -> do
          maybeDims <- runFreshNameBoundContextT $ getDims dimsValue
          case maybeDims of
            Nothing -> throwError $ UnsupportedTensorDimensions dimsValue
            Just dims -> goTensor dims indices body
        _ -> throwError $ UnreducableType typ

    goVector :: UnforcedType Builtin -> Int -> TensorIndices -> Thunk Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    goVector typ dim indices value = do
      forcedValue <- runFreshNameBoundContextT $ forceThunk value
      logDebug MaxDetail $ prettyFriendlyEmptyCtx forcedValue
      case toVectorValue forcedValue of
        VVectorLiteral args -> MultiProperty <$> zipWithM (\i e -> go typ (i : indices) e) [0 ..] (vecLitElements args)
        VVectorBoundVar {} -> unexpectedExprError currentPass "boundVar"
        VVectorRecordAcc {} -> unexpectedExprError currentPass "recordAcc"
        VVectorDataset {} -> throwError $ UnsupportedVectorValue forcedValue
        VVectorIf {} -> throwError $ UnsupportedVectorValue forcedValue
        VVectorAt {} -> throwError $ UnsupportedVectorValue forcedValue
        VVectorForeach args -> do
          evalResult <- runFreshNameBoundContextT $ forceEval evalForeachVector args
          logDebug MaxDetail $ prettyFriendlyEmptyCtx evalResult
          goVector typ dim indices evalResult

    goTensor :: TensorShape -> TensorIndices -> Thunk Builtin -> ExceptT MultiPropertyTraveralError m (MultiProperty a)
    goTensor dims indices value = case dims of
      [] -> do
        let address = PropertyAddress propertyName indices
        SingleProperty <$> lift (compileProp address value)
      _d : ds -> do
        forcedValue <- runFreshNameBoundContextT $ forceAndRewriteTensor value
        case toBoolTensorValue forcedValue of
          VBoolTensorLiteral bs -> do
            let es' = zip [0 :: Int ..] (Forced . IBoolTensorLiteral <$> unstack bs)
            MultiProperty <$> traverse (\(i, e) -> goTensor ds (i : indices) e) es'
          VBoolStackTensor args -> do
            let es' = zip [0 :: Int ..] $ stackElements args
            MultiProperty <$> traverse (\(i, e) -> goTensor ds (i : indices) e) es'
          _ -> throwError $ UnreducableTensorValue forcedValue

currentPass :: Doc a
currentPass = "multi-property traversal"
