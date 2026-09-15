module Vehicle.Data.Code.Interface.Operations where

import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

class HasBuiltinConstructor expr thunk | expr -> thunk, thunk -> expr where
  accessBuiltinC :: Accessor (expr builtin) (builtin, [GenericArg (thunk builtin)])
  exprToThunk :: expr builtin -> thunk builtin

mkBuiltin ::
  (HasBuiltinConstructor expr thunk) =>
  Accessor builtin a ->
  a ->
  [GenericArg (thunk builtin)] ->
  expr builtin
mkBuiltin accessBuiltin v args = mkExpr accessBuiltinC (mkExpr accessBuiltin v, args)

getBuiltin ::
  (HasBuiltinConstructor expr thunk) =>
  Accessor builtin a ->
  expr builtin ->
  Maybe (a, [GenericArg (thunk builtin)])
getBuiltin accessBuiltin e = case getExpr accessBuiltinC e of
  Just (b, args) -> case getExpr accessBuiltin b of
    Just v -> Just (v, args)
    _ -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- Accessors for args
--------------------------------------------------------------------------------

accessNoArgs ::
  (HasBuiltinConstructor expr thunk) =>
  Accessor builtin a ->
  Accessor (expr builtin) a
accessNoArgs access =
  Access
    { getExpr = \case
        (getBuiltin access -> Just (b, [])) -> Just b
        _ -> Nothing,
      mkExpr = \b -> mkBuiltin access b []
    }

accessArgs ::
  (HasBuiltinConstructor expr thunk, IsArgs args) =>
  Accessor builtin () ->
  Accessor (expr builtin) (args (thunk builtin))
accessArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just ((), getExpr accessSpine -> Just args)) -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkBuiltin accessOp () (mkExpr accessSpine args)
    }

accessOpAndArgs ::
  (HasBuiltinConstructor expr thunk, IsArgs args) =>
  Accessor builtin op ->
  Accessor (expr builtin) (op, args (thunk builtin))
accessOpAndArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just (op, getExpr accessSpine -> Just args)) -> Just (op, args)
        _ -> Nothing,
      mkExpr = \(op, args) -> mkBuiltin accessOp op (mkExpr accessSpine args)
    }

accessArgsForOp ::
  (HasBuiltinConstructor expr thunk, IsArgs args, Eq op) =>
  Accessor (expr builtin) (op, args (thunk builtin)) ->
  op ->
  Accessor (expr builtin) (args (thunk builtin))
accessArgsForOp accessor op =
  Access
    { getExpr = \case
        (getExpr accessor -> Just (op2, args)) | op == op2 -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkExpr accessor (op, args)
    }

--------------------------------------------------------------------------------
-- Types of accessors
--------------------------------------------------------------------------------

type NatComparisonAccessor expr thunk builtin = Accessor (expr builtin) (ComparisonOp, Op2Args (thunk builtin))

type IndexComparisonAccessor expr thunk builtin = Accessor (expr builtin) (ComparisonOp, IndexComparisonArgs (thunk builtin))

type RatTensorComparisonAccessor expr thunk builtin = Accessor (expr builtin) (ComparisonOp, TensorComparisonArgs (thunk builtin))

type Op1Accessor expr thunk builtin = Accessor (expr builtin) (Op1Args (thunk builtin))

type Op2Accessor expr thunk builtin = Accessor (expr builtin) (Op2Args (thunk builtin))

type TensorOp1Accessor expr thunk builtin = Accessor (expr builtin) (TensorOp1Args (thunk builtin))

type TensorOp2Accessor expr thunk builtin = Accessor (expr builtin) (TensorOp2Args (thunk builtin))

type TensorReductionAccessor expr thunk builtin = Accessor (expr builtin) (TensorReductionArgs (thunk builtin))

--------------------------------------------------------------------------------
-- Accessors for operations
--------------------------------------------------------------------------------
-- Booleans

type HasBoolType expr thunk builtin =
  ( HasTensorExpr expr thunk builtin,
    BuiltinHasBoolType builtin
  )

type HasBoolExpr expr thunk builtin =
  ( HasTensorExpr expr thunk builtin,
    BuiltinHasBoolLiterals builtin
  )

accessBoolType :: (HasBoolType expr thunk builtin) => Accessor (expr builtin) ()
accessBoolType = accessNoArgs accessBoolTypeBuiltin

accessBoolTensorLiteral :: (HasBoolExpr expr thunk builtin) => Accessor (expr builtin) BoolTensor
accessBoolTensorLiteral = accessNoArgs accessBoolTensorLitBuiltin

accessNotTensor :: (HasBoolExpr expr thunk builtin) => TensorOp1Accessor expr thunk builtin
accessNotTensor = accessArgs accessNotBuiltin

accessAndTensor :: (HasBoolExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessAndTensor = accessArgs accessAndBuiltin

accessOrTensor :: (HasBoolExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessOrTensor = accessArgs accessOrBuiltin

accessImpliesTensor :: (HasBoolExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessImpliesTensor = accessArgs accessImpliesBuiltin

accessReduceAnd :: (HasBoolExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceAnd = accessArgs accessReduceAndBuiltin

accessReduceOr :: (HasBoolExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceOr = accessArgs accessReduceOrBuiltin

accessIf :: (HasBoolExpr expr thunk builtin) => Accessor (expr builtin) (IfArgs (thunk builtin))
accessIf = accessArgs accessIfBuiltin

accessCompareIndex :: (HasBoolExpr expr thunk builtin) => IndexComparisonAccessor expr thunk builtin
accessCompareIndex = accessOpAndArgs accessCompareIndexBuiltin

accessCompareNat :: (HasBoolExpr expr thunk builtin) => NatComparisonAccessor expr thunk builtin
accessCompareNat = accessOpAndArgs accessCompareNatBuiltin

accessCompareRatTensor :: (HasBoolExpr expr thunk builtin) => RatTensorComparisonAccessor expr thunk builtin
accessCompareRatTensor = accessOpAndArgs accessCompareRatTensorBuiltin

accessQuantifyRatTensor ::
  (HasBoolExpr expr thunk builtin, HasLambdaConstructor expr thunk closure) =>
  Accessor (expr builtin) (Quantifier, QuantifyRatTensorArgs (thunk builtin) (closure builtin))
accessQuantifyRatTensor =
  Access
    { getExpr = \case
        (getBuiltin accessQuantifyRatTensorBuiltin -> Just (q, spine)) ->
          case getExpr accessQuantifyRatTensorSpine spine of
            Just args -> Just (q, args)
            _ -> Nothing
        _ -> Nothing,
      mkExpr = \(q, args) -> mkBuiltin accessQuantifyRatTensorBuiltin q (mkExpr accessQuantifyRatTensorSpine args)
    }

accessQuantifyRecord ::
  (HasBoolExpr expr thunk builtin, HasLambdaConstructor expr thunk closure) =>
  Accessor (expr builtin) (Quantifier, QuantifyRecordArgs (thunk builtin) (closure builtin))
accessQuantifyRecord =
  Access
    { getExpr = \case
        (getBuiltin accessQuantifyRecordBuiltin -> Just (q, spine)) ->
          case getExpr accessQuantifyRecordSpine spine of
            Just args -> Just (q, args)
            _ -> Nothing
        _ -> Nothing,
      mkExpr = \(q, args) -> mkBuiltin accessQuantifyRecordBuiltin q (mkExpr accessQuantifyRecordSpine args)
    }

--------------------------------------------------------------------------------
-- Indices

type HasIndexType expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasIndexType builtin
  )

type HasIndexExpr expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasIndexLiterals builtin
  )

accessIndexType :: (HasIndexType expr thunk builtin) => Accessor (expr builtin) (IndexTypeArgs (thunk builtin))
accessIndexType = accessArgs accessIndexTypeBuiltin

accessIndexLiteral :: (HasIndexExpr expr thunk builtin) => Accessor (expr builtin) (Int, IndexLiteralArgs (thunk builtin))
accessIndexLiteral = accessOpAndArgs accessIndexLitBuiltin

--------------------------------------------------------------------------------
-- Naturals

type HasNatType expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasNatType builtin
  )

type HasNatExpr expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasNatLiterals builtin
  )

accessNatType :: (HasNatType expr thunk builtin) => Accessor (expr builtin) ()
accessNatType = accessNoArgs accessNatTypeBuiltin

accessNatLiteral :: (HasNatExpr expr thunk builtin) => Accessor (expr builtin) Int
accessNatLiteral = accessNoArgs accessNatLitBuiltin

accessNatTensorLiteral :: (HasNatExpr expr thunk builtin) => Accessor (expr builtin) NatTensor
accessNatTensorLiteral = accessNoArgs accessNatTensorLitBuiltin

accessAddNat :: (HasNatExpr expr thunk builtin) => Op2Accessor expr thunk builtin
accessAddNat = accessArgs accessAddNatBuiltin

accessMulNat :: (HasNatExpr expr thunk builtin) => Op2Accessor expr thunk builtin
accessMulNat = accessArgs accessMulNatBuiltin

--------------------------------------------------------------------------------
-- Rationals

type HasRatType expr thunk builtin =
  ( HasTensorExpr expr thunk builtin,
    BuiltinHasRatType builtin
  )

type HasRatExpr expr thunk builtin =
  ( HasTensorExpr expr thunk builtin,
    BuiltinHasRatLiterals builtin
  )

accessRatType :: (HasRatType expr thunk builtin) => Accessor (expr builtin) ()
accessRatType = accessNoArgs accessRatTypeBuiltin

accessRatTensorLiteral :: (HasRatExpr expr thunk builtin) => Accessor (expr builtin) ExtendedRatTensor
accessRatTensorLiteral = accessNoArgs accessRatTensorLitBuiltin

accessNegRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp1Accessor expr thunk builtin
accessNegRatTensor = accessArgs accessNegRatTensorBuiltin

accessLogRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp1Accessor expr thunk builtin
accessLogRatTensor = accessArgs accessLogRatTensorBuiltin

accessExpRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp1Accessor expr thunk builtin
accessExpRatTensor = accessArgs accessExpRatTensorBuiltin

accessAddRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessAddRatTensor = accessArgs accessAddRatTensorBuiltin

accessMulRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessMulRatTensor = accessArgs accessMulRatTensorBuiltin

accessSubRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessSubRatTensor = accessArgs accessSubRatTensorBuiltin

accessDivRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessDivRatTensor = accessArgs accessDivRatTensorBuiltin

accessMinRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessMinRatTensor = accessArgs accessMinRatTensorBuiltin

accessMaxRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessMaxRatTensor = accessArgs accessMaxRatTensorBuiltin

accessPowRatTensor :: (HasRatExpr expr thunk builtin) => TensorOp2Accessor expr thunk builtin
accessPowRatTensor = accessArgs accessPowRatTensorBuiltin

accessReduceAddRat :: (HasRatExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceAddRat = accessArgs accessReduceAddRatBuiltin

accessReduceMulRat :: (HasRatExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceMulRat = accessArgs accessReduceMulRatBuiltin

accessReduceMinRat :: (HasRatExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceMinRat = accessArgs accessReduceMinRatBuiltin

accessReduceMaxRat :: (HasRatExpr expr thunk builtin) => TensorReductionAccessor expr thunk builtin
accessReduceMaxRat = accessArgs accessReduceMaxRatBuiltin

--------------------------------------------------------------------------------
-- Lists

type HasListType expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasListType builtin
  )

type HasListExpr expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasListLiterals builtin
  )

accessListType :: (HasListType expr thunk builtin) => Op1Accessor expr thunk builtin
accessListType = accessArgs accessListTypeBuiltin

accessNil :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (NilArgs (thunk builtin))
accessNil = accessArgs accessNilBuiltin

accessCons :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (ConsArgs (thunk builtin))
accessCons = accessArgs accessConsBuiltin

accessMapList :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (MapListArgs (thunk builtin))
accessMapList = accessArgs accessMapListBuiltin

accessFoldList :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (FoldListArgs (thunk builtin))
accessFoldList = accessArgs accessFoldListBuiltin

accessReverseList :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (ReverseListArgs (thunk builtin))
accessReverseList = accessArgs accessReverseListBuiltin

accessAppendList :: (HasListExpr expr thunk builtin) => Accessor (expr builtin) (AppendListArgs (thunk builtin))
accessAppendList = accessArgs accessAppendListBuiltin

--------------------------------------------------------------------------------
-- Vector

type HasVectorType expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasVectorType builtin
  )

type HasVectorExpr expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasVectors builtin,
    BuiltinHasNatLiterals builtin
  )

accessVectorType :: (HasVectorType expr thunk builtin) => Accessor (expr builtin) (VectorTypeArgs (thunk builtin))
accessVectorType = accessArgs accessVectorTypeBuiltin

accessVecLit :: (HasVectorExpr expr thunk builtin) => Accessor (expr builtin) (VectorLitArgs (thunk builtin))
accessVecLit = accessArgs accessVecLitBuiltin

accessAtVector :: (HasVectorExpr expr thunk builtin) => Accessor (expr builtin) (AtVectorArgs (thunk builtin))
accessAtVector = accessArgs accessAtVectorBuiltin

accessForeachVector ::
  (HasBuiltinConstructor expr thunk, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachVectorArgs (thunk builtin))
accessForeachVector = accessArgs accessForeachVectorBuiltin

--------------------------------------------------------------------------------
-- Tensors

type HasTensorType expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasTensorType builtin
  )

type HasTensorExpr expr thunk builtin =
  ( HasBuiltinConstructor expr thunk,
    BuiltinHasTensors builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatLiterals builtin,
    BuiltinHasIndexLiterals builtin,
    BuiltinHasNatType builtin
  )

accessTensorType :: (HasTensorType expr thunk builtin) => Accessor (expr builtin) (TensorTypeArgs (thunk builtin))
accessTensorType = accessArgs accessTensorTypeBuiltin

accessStackTensor :: (HasTensorExpr expr thunk builtin) => Accessor (expr builtin) (StackTensorArgs (thunk builtin))
accessStackTensor = accessArgs accessStackTensorBuiltin

accessConstTensor :: (HasTensorExpr expr thunk builtin) => Accessor (expr builtin) (ConstTensorArgs (thunk builtin))
accessConstTensor = accessArgs accessConstTensorBuiltin

accessAtTensor :: (HasTensorExpr expr thunk builtin) => Accessor (expr builtin) (AtTensorArgs (thunk builtin))
accessAtTensor = accessArgs accessAtTensorBuiltin

accessForeachTensor ::
  (HasBuiltinConstructor expr thunk, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachTensorArgs (thunk builtin))
accessForeachTensor = accessArgs accessForeachTensorBuiltin

accessIterate ::
  (HasBuiltinConstructor expr thunk, BuiltinHasIterate builtin) =>
  Accessor (expr builtin) (IterateArgs (thunk builtin))
accessIterate = accessArgs accessIterateBuiltin

accessTransposeTensor ::
  (HasBuiltinConstructor expr thunk, BuiltinHasTensors builtin) =>
  Accessor (expr builtin) (TransposeTensorArgs (thunk builtin))
accessTransposeTensor = accessArgs accessTransposeBuiltin
