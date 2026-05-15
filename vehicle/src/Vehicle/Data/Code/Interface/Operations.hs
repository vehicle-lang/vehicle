module Vehicle.Data.Code.Interface.Operations where

import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

class HasBuiltinConstructor expr where
  accessBuiltinC :: Accessor (expr builtin) (builtin, [GenericArg (expr builtin)])

mkBuiltin ::
  (HasBuiltinConstructor expr) =>
  Accessor builtin a ->
  a ->
  [GenericArg (expr builtin)] ->
  expr builtin
mkBuiltin accessBuiltin v args = mkExpr accessBuiltinC (mkExpr accessBuiltin v, args)

getBuiltin ::
  (HasBuiltinConstructor expr) =>
  Accessor builtin a ->
  expr builtin ->
  Maybe (a, [GenericArg (expr builtin)])
getBuiltin accessBuiltin e = case getExpr accessBuiltinC e of
  Just (b, args) -> case getExpr accessBuiltin b of
    Just v -> Just (v, args)
    _ -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- Accessors for args
--------------------------------------------------------------------------------

accessNoArgs ::
  (HasBuiltinConstructor expr) =>
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
  (HasBuiltinConstructor expr, IsArgs args) =>
  Accessor builtin () ->
  Accessor (expr builtin) (args (expr builtin))
accessArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just ((), getExpr accessSpine -> Just args)) -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkBuiltin accessOp () (mkExpr accessSpine args)
    }

accessOpAndArgs ::
  (HasBuiltinConstructor expr, IsArgs args) =>
  Accessor builtin op ->
  Accessor (expr builtin) (op, args (expr builtin))
accessOpAndArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just (op, getExpr accessSpine -> Just args)) -> Just (op, args)
        _ -> Nothing,
      mkExpr = \(op, args) -> mkBuiltin accessOp op (mkExpr accessSpine args)
    }

accessArgsForOp ::
  (HasBuiltinConstructor expr, IsArgs args, Eq op) =>
  Accessor (expr builtin) (op, args (expr builtin)) ->
  op ->
  Accessor (expr builtin) (args (expr builtin))
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

type NatComparisonAccessor expr op = Accessor expr (op, Op2Args expr)

type IndexComparisonAccessor expr op = Accessor expr (op, IndexComparisonArgs expr)

type RatTensorPointwiseComparisonAccessor expr op = Accessor expr (op, TensorOp2Args expr)

type RatTensorReducedComparisonAccessor expr op = Accessor expr (op, TensorReduceComparisonArgs expr)

type Op1Accessor expr = Accessor expr (Op1Args expr)

type Op2Accessor expr = Accessor expr (Op2Args expr)

type TensorOp1Accessor expr = Accessor expr (TensorOp1Args expr)

type TensorOp2Accessor expr = Accessor expr (TensorOp2Args expr)

type TensorReductionAccessor expr = Accessor expr (TensorReductionArgs expr)

--------------------------------------------------------------------------------
-- Accessors for operations
--------------------------------------------------------------------------------
-- Booleans

type HasBoolType expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasBoolType builtin
  )

type HasBoolExpr expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasBoolLiterals builtin
  )

accessBoolType :: (HasBoolType expr builtin) => Accessor (expr builtin) ()
accessBoolType = accessNoArgs accessBoolTypeBuiltin

accessBoolTensorLiteral :: (HasBoolExpr expr builtin) => Accessor (expr builtin) BoolTensor
accessBoolTensorLiteral = accessNoArgs accessBoolTensorLitBuiltin

accessNotTensor :: (HasBoolExpr expr builtin) => TensorOp1Accessor (expr builtin)
accessNotTensor = accessArgs accessNotBuiltin

accessAndTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessAndTensor = accessArgs accessAndBuiltin

accessOrTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessOrTensor = accessArgs accessOrBuiltin

accessImpliesTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessImpliesTensor = accessArgs accessImpliesBuiltin

accessReduceAnd :: (HasBoolExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceAnd = accessArgs accessReduceAndBuiltin

accessReduceOr :: (HasBoolExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceOr = accessArgs accessReduceOrBuiltin

accessIf :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (IfArgs (expr builtin))
accessIf = accessArgs accessIfBuiltin

accessCompareIndex :: (HasBoolExpr expr builtin) => IndexComparisonAccessor (expr builtin) ComparisonOp
accessCompareIndex = accessOpAndArgs accessCompareIndexBuiltin

accessCompareNat :: (HasBoolExpr expr builtin) => NatComparisonAccessor (expr builtin) ComparisonOp
accessCompareNat = accessOpAndArgs accessCompareNatBuiltin

accessCompareRatTensorPointwise :: (HasBoolExpr expr builtin) => RatTensorPointwiseComparisonAccessor (expr builtin) ComparisonOp
accessCompareRatTensorPointwise = accessOpAndArgs accessCompareRatTensorPointwiseBuiltin

accessCompareRatTensorReduced :: (HasBoolExpr expr builtin) => RatTensorReducedComparisonAccessor (expr builtin) ComparisonOp
accessCompareRatTensorReduced = accessOpAndArgs accessCompareRatTensorReducedBuiltin

accessQuantifyRatTensor ::
  (HasBoolExpr expr builtin, HasLambdaConstructor expr body) =>
  Accessor (expr builtin) (Quantifier, QuantifyRatTensorArgs (expr builtin) (body builtin))
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

-- | Accessor for unary temporal operators (Globally, Finally). Until is excluded
-- because it has a different arity (TemporalOp2Args); use accessTemporalUntil directly.
accessTemporalUnary :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (TemporalOperator, TemporalOp1Args (expr builtin))
accessTemporalUnary = accessOpAndArgs accessTemporalBuiltin

accessTemporalGlobally :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (TemporalOp1Args (expr builtin))
accessTemporalGlobally = accessArgsForOp accessTemporalUnary Globally

accessTemporalFinally :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (TemporalOp1Args (expr builtin))
accessTemporalFinally = accessArgsForOp accessTemporalUnary Finally

accessTemporalUntil :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (TemporalOp2Args (expr builtin))
accessTemporalUntil =
  Access
    { getExpr = \case
        (getBuiltin accessTemporalBuiltin -> Just (Until, getExpr accessSpine -> Just args)) -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkBuiltin accessTemporalBuiltin Until (mkExpr accessSpine args)
    }

--------------------------------------------------------------------------------
-- Indices

type HasIndexType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasIndexType builtin
  )

type HasIndexExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasIndexLiterals builtin
  )

accessIndexType :: (HasIndexType expr builtin) => Accessor (expr builtin) (IndexTypeArgs (expr builtin))
accessIndexType = accessArgs accessIndexTypeBuiltin

accessIndexLiteral :: (HasIndexExpr expr builtin) => Accessor (expr builtin) (Int, IndexLiteralArgs (expr builtin))
accessIndexLiteral = accessOpAndArgs accessIndexLitBuiltin

--------------------------------------------------------------------------------
-- Naturals

type HasNatType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasNatType builtin
  )

type HasNatExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasNatLiterals builtin
  )

accessNatType :: (HasNatType expr builtin) => Accessor (expr builtin) ()
accessNatType = accessNoArgs accessNatTypeBuiltin

accessNatLiteral :: (HasNatExpr expr builtin) => Accessor (expr builtin) Int
accessNatLiteral = accessNoArgs accessNatLitBuiltin

accessNatTensorLiteral :: (HasNatExpr expr builtin) => Accessor (expr builtin) NatTensor
accessNatTensorLiteral = accessNoArgs accessNatTensorLitBuiltin

accessAddNat :: (HasNatExpr expr builtin) => Op2Accessor (expr builtin)
accessAddNat = accessArgs accessAddNatBuiltin

accessMulNat :: (HasNatExpr expr builtin) => Op2Accessor (expr builtin)
accessMulNat = accessArgs accessMulNatBuiltin

--------------------------------------------------------------------------------
-- Time

type HasTimeType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasTimeType builtin
  )

type HasTimeExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasTimeLiterals builtin
  )

accessTimeType :: (HasTimeType expr builtin) => Accessor (expr builtin) ()
accessTimeType = accessNoArgs accessTimeTypeBuiltin

accessTimeLiteral :: (HasTimeExpr expr builtin) => Accessor (expr builtin) Int
accessTimeLiteral = accessNoArgs accessTimeLitBuiltin

accessAddTime :: (HasTimeExpr expr builtin) => Op2Accessor (expr builtin)
accessAddTime = accessArgs accessAddTimeBuiltin

accessSubTime :: (HasTimeExpr expr builtin) => Op2Accessor (expr builtin)
accessSubTime = accessArgs accessSubTimeBuiltin

accessMulTime :: (HasTimeExpr expr builtin) => Op2Accessor (expr builtin)
accessMulTime = accessArgs accessMulTimeBuiltin

accessDivTime :: (HasTimeExpr expr builtin) => Op2Accessor (expr builtin)
accessDivTime = accessArgs accessDivTimeBuiltin

--------------------------------------------------------------------------------
-- Rationals

type HasRatType expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasRatType builtin
  )

type HasRatExpr expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasRatLiterals builtin
  )

accessRatType :: (HasRatType expr builtin) => Accessor (expr builtin) ()
accessRatType = accessNoArgs accessRatTypeBuiltin

accessRatTensorLiteral :: (HasRatExpr expr builtin) => Accessor (expr builtin) RatTensor
accessRatTensorLiteral = accessNoArgs accessRatTensorLitBuiltin

accessNegRatTensor :: (HasRatExpr expr builtin) => TensorOp1Accessor (expr builtin)
accessNegRatTensor = accessArgs accessNegRatTensorBuiltin

accessAddRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessAddRatTensor = accessArgs accessAddRatTensorBuiltin

accessMulRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMulRatTensor = accessArgs accessMulRatTensorBuiltin

accessSubRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessSubRatTensor = accessArgs accessSubRatTensorBuiltin

accessDivRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessDivRatTensor = accessArgs accessDivRatTensorBuiltin

accessMinRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMinRatTensor = accessArgs accessMinRatTensorBuiltin

accessMaxRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMaxRatTensor = accessArgs accessMaxRatTensorBuiltin

accessPowRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessPowRatTensor = accessArgs accessPowRatTensorBuiltin

accessExpRatTensor :: (HasRatExpr expr builtin) => TensorOp1Accessor (expr builtin)
accessExpRatTensor = accessArgs accessExpRatTensorBuiltin

accessLogRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessLogRatTensor = accessArgs accessLogRatTensorBuiltin

accessReduceAddRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceAddRat = accessArgs accessReduceAddRatBuiltin

accessReduceMulRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMulRat = accessArgs accessReduceMulRatBuiltin

accessReduceMinRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMinRat = accessArgs accessReduceMinRatBuiltin

accessReduceMaxRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMaxRat = accessArgs accessReduceMaxRatBuiltin

--------------------------------------------------------------------------------
-- Lists

type HasListType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasListType builtin
  )

type HasListExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasListLiterals builtin
  )

accessListType :: (HasListType expr builtin) => Op1Accessor (expr builtin)
accessListType = accessArgs accessListTypeBuiltin

accessNil :: (HasListExpr expr builtin) => Accessor (expr builtin) (NilArgs (expr builtin))
accessNil = accessArgs accessNilBuiltin

accessCons :: (HasListExpr expr builtin) => Accessor (expr builtin) (ConsArgs (expr builtin))
accessCons = accessArgs accessConsBuiltin

accessMapList :: (HasListExpr expr builtin) => Accessor (expr builtin) (MapListArgs (expr builtin))
accessMapList = accessArgs accessMapListBuiltin

accessFoldList :: (HasListExpr expr builtin) => Accessor (expr builtin) (FoldListArgs (expr builtin))
accessFoldList = accessArgs accessFoldListBuiltin

--------------------------------------------------------------------------------
-- Vector

type HasVectorType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasVectorType builtin
  )

type HasVectorExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasVectors builtin,
    BuiltinHasNatLiterals builtin
  )

accessVectorType :: (HasVectorType expr builtin) => Accessor (expr builtin) (VectorTypeArgs (expr builtin))
accessVectorType = accessArgs accessVectorTypeBuiltin

accessVecLit :: (HasVectorExpr expr builtin) => Accessor (expr builtin) (VecLitArgs (expr builtin))
accessVecLit = accessArgs accessVecLitBuiltin

accessAtVector :: (HasVectorExpr expr builtin) => Accessor (expr builtin) (AtVectorArgs (expr builtin))
accessAtVector = accessArgs accessAtVectorBuiltin

accessForeachVector ::
  (HasBuiltinConstructor expr, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachVectorArgs (expr builtin))
accessForeachVector = accessArgs accessForeachVectorBuiltin

--------------------------------------------------------------------------------
-- Tensors

type HasTensorType expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasTensorType builtin
  )

type HasTensorExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasTensors builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatLiterals builtin,
    BuiltinHasIndexLiterals builtin,
    BuiltinHasNatType builtin
  )

accessTensorType :: (HasTensorType expr builtin) => Accessor (expr builtin) (TensorTypeArgs (expr builtin))
accessTensorType = accessArgs accessTensorTypeBuiltin

accessStackTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (StackTensorArgs (expr builtin))
accessStackTensor = accessArgs accessStackTensorBuiltin

accessConstTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (ConstTensorArgs (expr builtin))
accessConstTensor = accessArgs accessConstTensorBuiltin

accessAtTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (AtTensorArgs (expr builtin))
accessAtTensor = accessArgs accessAtTensorBuiltin

accessForeachTensor ::
  (HasBuiltinConstructor expr, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachTensorArgs (expr builtin))
accessForeachTensor = accessArgs accessForeachTensorBuiltin

accessIterate ::
  (HasBuiltinConstructor expr, BuiltinHasIterate builtin) =>
  Accessor (expr builtin) (IterateArgs (expr builtin))
accessIterate = accessArgs accessIterateBuiltin

accessRollout ::
  (HasBuiltinConstructor expr, BuiltinHasRollout builtin) =>
  Accessor (expr builtin) (RolloutArgs (expr builtin))
accessRollout = accessArgs accessRolloutBuiltin

accessTranspose ::
  (HasBuiltinConstructor expr, BuiltinHasTensors builtin) =>
  Accessor (expr builtin) (TransposeArgs (expr builtin))
accessTranspose = accessArgs accessTransposeBuiltin
