{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Constants.Value where

import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (etaReduceTensor)
import Vehicle.Data.Code.Value
import Vehicle.Data.Real
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude
import Vehicle.Prelude.Logging

--------------------------------------------------------------------------------
-- Tensors of values

type HasRatTensors builtin =
  ( HasRatExpr Value builtin,
    HasRatType Value builtin,
    HasTensorLiterals Value builtin
  )

type TensorValueLinearExpr builtin = LinearExpr SliceVariable (DimensionedTensorValue builtin)

tensorValueLinearExprToValue :: (HasRatTensors builtin) => LinearExpr SliceVariable (DimensionedTensorValue builtin) -> DimensionedTensorValue builtin
tensorValueLinearExprToValue linearExpr = do
  let dims = tensorValueDims $ constantValue linearExpr
  let mkVarTerm v = TensorValue dims (VBoundVar (toLv v) [])
  let mkTerm (v, coeff) = scaleConstant coeff (mkVarTerm v)
  linearExprToExpr id mkTerm (addConstants 1 1) linearExpr

type UserVariableConstraint builtin = Assertion (TensorValueLinearExpr builtin)

-- | An `AssertionTree` represents a boolean expression with assertions at
-- each terminal leaf.
type UserVariableConstraintTree = BooleanExpr (UserVariableConstraint LossBuiltin)

constantDimensionedValue :: (HasRatTensors builtin) => VDims builtin -> ExtendedRational -> DimensionedTensorValue builtin
constantDimensionedValue dims constant =
  TensorValue dims $
    runSilentLogger $
      evalConstTensor $
        ConstTensorArgs
          { constType = IRatType,
            constValue = IRatLiteral constant,
            constDims = dims
          }

addDimensionedValue ::
  (HasRatTensors builtin) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin
addDimensionedValue (TensorValue dims1 e1) (TensorValue _dims2 e2) = do
  TensorValue dims1 $
    runSilentLogger $
      evalAddRatTensor $
        TensorOp2Args dims1 e1 e2

scaleDimensionedValue ::
  (HasRatTensors builtin) =>
  Coefficient ->
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin
scaleDimensionedValue c (TensorValue dims e) = do
  let constant = tensorValue $ constantDimensionedValue dims (Finite c)
  let e' = runSilentLogger $ evalMulRatTensor $ TensorOp2Args dims constant e
  TensorValue dims e'

addDimensionedConstants ::
  (HasRatTensors builtin) =>
  AddConstants (DimensionedTensorValue builtin)
addDimensionedConstants c1 c2 v1 v2 = do
  let cv1 = scaleConstant c1 v1
  let cv2 = scaleConstant c2 v2
  addDimensionedValue cv1 cv2

dimensionedValueToRatTensor ::
  (HasRatTensors builtin) =>
  DimensionedTensorValue builtin ->
  Maybe RatTensor
dimensionedValueToRatTensor (TensorValue _ e1) = case e1 of
  IRatTensor (toFiniteRatTensor -> Just t) -> Just t
  _ -> Nothing

minTensorValues ::
  (HasRatTensors builtin) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin
minTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims $
    runSilentLogger $
      evalMinRatTensor $
        TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }

maxTensorValues ::
  (HasRatTensors builtin) =>
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin ->
  DimensionedTensorValue builtin
maxTensorValues (TensorValue dims v1) (TensorValue _ v2) =
  TensorValue dims $
    runSilentLogger $
      evalMaxRatTensor $
        TensorOp2Args
          { tensorOp2Dims = dims,
            tensorOp2Arg1 = v1,
            tensorOp2Arg2 = v2
          }

stackTensorValues :: (HasRatTensors builtin) => [DimensionedTensorValue builtin] -> DimensionedTensorValue builtin
stackTensorValues = \case
  [] -> developerError "Cannot stack zero tensors"
  elements@(TensorValue dims _ : _) -> do
    let newDim = INatLiteral (length elements)
    let newDims = IDimCons newDim dims
    TensorValue newDims $
      runSilentLogger $
        evalStackTensor $
          StackTensorArgs
            { stackType = IRatType,
              stackFirstDim = newDim,
              stackRemainingDims = dims,
              stackElements = fmap tensorValue elements
            }

unstackTensorValues :: (HasRatTensors builtin) => DimensionedTensorValue builtin -> [DimensionedTensorValue builtin]
unstackTensorValues (TensorValue dims value) = case dims of
  IDimCons (INatLiteral d) ds -> do
    let values = runSilentLogger $ etaReduceTensor IRatType d ds value
    fmap (TensorValue ds) values
  _ -> developerError "Cannot unstack tensor with unknown dimensions"

instance (HasRatTensors builtin) => ConstantLike (DimensionedTensorValue builtin) where
  addConstants = addDimensionedConstants
  scaleConstant = scaleDimensionedValue
  toRatTensor = dimensionedValueToRatTensor
  minConstants = minTensorValues
  maxConstants = maxTensorValues
  stackConstants = stackTensorValues
  unstackConstants = unstackTensorValues

tensorLinearExprToExpr :: (HasRatTensors builtin) => VDims builtin -> TensorValueLinearExpr builtin -> Value builtin
tensorLinearExprToExpr dims linexp = tensorValue $ linearExprToExpr id fromVar addDimensionedValue linexp
  where
    fromVar (v, c) = scaleDimensionedValue c (TensorValue dims (VBoundVar (toLv v) []))
