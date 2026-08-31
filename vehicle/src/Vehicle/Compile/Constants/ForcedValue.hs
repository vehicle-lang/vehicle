{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.ForcedValue where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Normalise.TypedValue
  ( etaReduceTensor,
  )
import Vehicle.Data.Assertion
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Real
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensors of values

type HasRatTensors builtin =
  ( HasRatExpr ForcedValue Thunk builtin,
    HasRatType ForcedValue Thunk builtin,
    HasTensorLiterals ForcedValue builtin,
    NormalisableBuiltin builtin
  )

type TensorValueLinearExpr builtin = LinearExpr SliceVariable (DimensionedTensorValue builtin)

tensorValueLinearExprToValue ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  LinearExpr SliceVariable (DimensionedTensorValue builtin) ->
  m (DimensionedTensorValue builtin)
tensorValueLinearExprToValue linearExpr = do
  let dims = tensorValueDims $ constantValue linearExpr
  let mkVarTerm v = TensorValue dims (Forced $ VBoundVar (toLv v) [])
  let mkTerm (v, coeff) = scaleConstant coeff (mkVarTerm v)
  linearExprToExpr id mkTerm (addConstants 1 1) linearExpr

type UserVariableConstraint builtin = Assertion (TensorValueLinearExpr builtin)

constantDimensionedValue ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  UnforcedDims builtin ->
  ExtendedRational ->
  m (DimensionedTensorValue builtin)
constantDimensionedValue dims constant =
  TensorValue dims
    <$> ( forceEvaluation accessConstTensor evalConstTensor $
            ConstTensorArgs
              { constType = Forced IRatType,
                constValue = Forced $ IRatLiteral constant,
                constDims = dims
              }
        )

addDimensionedValue ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  m (DimensionedTensorValue builtin)
addDimensionedValue (TensorValue dims1 e1) (TensorValue _dims2 e2) = do
  TensorValue dims1
    <$> forceEvaluation
      accessAddRatTensor
      evalAddRatTensor
      (TensorOp2Args dims1 e1 e2)

scaleDimensionedValue ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  Coefficient ->
  DimensionedTensorValue builtin ->
  m (DimensionedTensorValue builtin)
scaleDimensionedValue c (TensorValue dims e) = do
  constant <- tensorValue <$> constantDimensionedValue dims (Finite c)
  TensorValue dims
    <$> forceEvaluation
      accessMulRatTensor
      evalMulRatTensor
      (TensorOp2Args dims constant e)

addDimensionedConstants ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  AddConstants (DimensionedTensorValue builtin) m
addDimensionedConstants c1 c2 v1 v2 = do
  cv1 <- scaleConstant c1 v1
  cv2 <- scaleConstant c2 v2
  addDimensionedValue cv1 cv2

dimensionedValueToRatTensor ::
  forall builtin m.
  (HasRatTensors builtin, MonadNorm builtin m) =>
  DimensionedTensorValue builtin ->
  m (Maybe RatTensor)
dimensionedValueToRatTensor (TensorValue _ e1) = do
  value <- forceThunk e1
  case value of
    IRatTensor (toFiniteRatTensor -> Just t) -> return $ Just t
    _ -> return Nothing

minTensorValues ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  m (DimensionedTensorValue builtin)
minTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims
    <$> forceEvaluation
      accessMinRatTensor
      evalMinRatTensor
      ( TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }
      )

maxTensorValues ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  m (DimensionedTensorValue builtin)
maxTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims
    <$> forceEvaluation
      accessMaxRatTensor
      evalMaxRatTensor
      ( TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }
      )

stackTensorValues ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  [DimensionedTensorValue builtin] ->
  m (DimensionedTensorValue builtin)
stackTensorValues = \case
  [] -> developerError "Cannot stack zero tensors"
  elements@(TensorValue dims _ : _) -> do
    let newDim = Forced $ INatLiteral (length elements)
    let newDims = Forced $ IDimCons newDim dims
    TensorValue newDims
      <$> forceEvaluation
        accessStackTensor
        evalStackTensor
        ( StackTensorArgs
            { stackType = Forced IRatType,
              stackFirstDim = newDim,
              stackRemainingDims = dims,
              stackElements = fmap tensorValue elements
            }
        )

unstackTensorValues ::
  (HasRatTensors builtin, MonadNorm builtin m) =>
  DimensionedTensorValue builtin ->
  m [DimensionedTensorValue builtin]
unstackTensorValues (TensorValue dims value) = case dims of
  Forced (IDimCons (Forced (INatLiteral d)) ds) -> do
    let values = etaReduceTensor (Forced IRatType) d ds value
    return $ fmap (TensorValue ds) values
  _ -> developerError "Cannot unstack tensor with unknown dimensions"

instance (HasRatTensors builtin, MonadNorm builtin m) => ConstantLike (DimensionedTensorValue builtin) m where
  addConstants = addDimensionedConstants
  scaleConstant = scaleDimensionedValue
  toRatTensor = dimensionedValueToRatTensor
  minConstants = minTensorValues
  maxConstants = maxTensorValues
  stackConstants = stackTensorValues
  unstackConstants = unstackTensorValues

tensorLinearExprToExpr :: (HasRatTensors builtin, MonadNorm builtin m) => UnforcedDims builtin -> TensorValueLinearExpr builtin -> m (Thunk builtin)
tensorLinearExprToExpr dims linexp = tensorValue <$> linearExprToExpr id fromVar addDimensionedValue linexp
  where
    fromVar (v, c) = scaleDimensionedValue c (TensorValue dims (Forced $ VBoundVar (toLv v) []))
