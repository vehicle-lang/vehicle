{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Core
  ( module Syntax,
    builtinCast,
    accessFromNatToIndex,
    accessFromNatToRat,
    accessFromVectorToList,
    isTensorType,
    builtinDerivedFunction,
  )
where

import Vehicle.Data.Builtin.Core as Syntax
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Expr
import Vehicle.Data.Code.Interface
import Vehicle.Data.DSL
import Vehicle.Prelude (GenericArg (..), HasIdentifier (identifierOf))
import Vehicle.Syntax.Sugar (BinderType (..))

-----------------------------------------------------------------------------
-- Classes

typeAccessor :: BuiltinType -> Accessor Builtin ()
typeAccessor b =
  Access
    { getExpr = \case
        BuiltinType b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> BuiltinType b
    }

functionAccessor :: BuiltinFunction -> Accessor Builtin ()
functionAccessor b =
  Access
    { getExpr = \case
        BuiltinFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> BuiltinFunction b
    }

castAccessor :: BuiltinCast -> Accessor Builtin ()
castAccessor c =
  Access
    { getExpr = \case
        BuiltinCast b1 | c == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> BuiltinCast c
    }

compareIndexAccessor :: Accessor Builtin ComparisonOp
compareIndexAccessor =
  Access
    { getExpr = \case
        BuiltinFunction (CompareIndex op) -> Just op
        _ -> Nothing,
      mkExpr = \op -> BuiltinFunction (CompareIndex op)
    }

compareNatAccessor :: Accessor Builtin ComparisonOp
compareNatAccessor =
  Access
    { getExpr = \case
        BuiltinFunction (CompareNat op) -> Just op
        _ -> Nothing,
      mkExpr = \op -> BuiltinFunction (CompareNat op)
    }

compareRatTensorPointwiseAccessor :: Accessor Builtin ComparisonOp
compareRatTensorPointwiseAccessor =
  Access
    { getExpr = \case
        BuiltinFunction (CompareRatTensorPointwise op) -> Just op
        _ -> Nothing,
      mkExpr = \op -> BuiltinFunction (CompareRatTensorPointwise op)
    }

compareRatTensorReducedAccessor :: Accessor Builtin ComparisonOp
compareRatTensorReducedAccessor =
  Access
    { getExpr = \case
        DerivedFunction (CompareRatTensorReduced op) -> Just op
        _ -> Nothing,
      mkExpr = \op -> DerivedFunction (CompareRatTensorReduced op)
    }

instance BuiltinHasBoolLiterals Builtin where
  accessBoolTypeBuiltin = typeAccessor BoolType
  accessBoolTensorLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (BoolTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = BuiltinConstructor . BoolTensorLiteral
      }

  accessNotBuiltin = functionAccessor Not
  accessAndBuiltin = functionAccessor And
  accessOrBuiltin = functionAccessor Or
  accessImpliesBuiltin = functionAccessor Implies
  accessReduceAndBuiltin = functionAccessor ReduceAndTensor
  accessReduceOrBuiltin = functionAccessor ReduceOrTensor
  accessIfBuiltin = functionAccessor If

  accessCompareIndexBuiltin = compareIndexAccessor
  accessCompareNatBuiltin = compareNatAccessor
  accessCompareRatTensorPointwiseBuiltin = compareRatTensorPointwiseAccessor
  accessCompareRatTensorReducedBuiltin = compareRatTensorReducedAccessor

  accessQuantifyRatTensorBuiltin =
    Access
      { getExpr = \case
          BuiltinFunction (QuantifyRatTensor q) -> Just q
          _ -> Nothing,
        mkExpr = BuiltinFunction . QuantifyRatTensor
      }

instance BuiltinHasIndexLiterals Builtin where
  accessIndexLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (IndexLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = BuiltinConstructor . IndexLiteral
      }

instance BuiltinHasNatType Builtin where
  accessNatTypeBuiltin = typeAccessor NatType

instance BuiltinHasNatLiterals Builtin where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = BuiltinConstructor . NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = BuiltinConstructor . NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

instance BuiltinHasRatLiterals Builtin where
  accessRatTypeBuiltin = typeAccessor RatType
  accessRatTensorLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (RatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = BuiltinConstructor . RatTensorLiteral
      }

  accessNegRatTensorBuiltin = functionAccessor $ Neg NegRatTensor
  accessAddRatTensorBuiltin = functionAccessor $ Add AddRatTensor
  accessMulRatTensorBuiltin = functionAccessor $ Mul MulRatTensor
  accessSubRatTensorBuiltin = functionAccessor $ Sub SubRatTensor
  accessDivRatTensorBuiltin = functionAccessor $ Div DivRatTensor
  accessMinRatTensorBuiltin = functionAccessor $ Min MinRatTensor
  accessMaxRatTensorBuiltin = functionAccessor $ Max MaxRatTensor
  accessPowRatTensorBuiltin = functionAccessor PowRat
  accessReduceAddRatBuiltin = functionAccessor ReduceAddRatTensor
  accessReduceMulRatBuiltin = functionAccessor ReduceMulRatTensor
  accessReduceMinRatBuiltin = functionAccessor ReduceMinRatTensor
  accessReduceMaxRatBuiltin = functionAccessor ReduceMaxRatTensor

instance BuiltinHasListLiterals Builtin where
  accessNilBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor Nil -> Just ()
          _ -> Nothing,
        mkExpr = \() -> BuiltinConstructor Nil
      }

  accessConsBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor Cons -> Just ()
          _ -> Nothing,
        mkExpr = \() -> BuiltinConstructor Cons
      }

  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

instance BuiltinHasVectors Builtin where
  accessVecLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor VectorLiteral -> Just ()
          _ -> Nothing,
        mkExpr = \() -> BuiltinConstructor VectorLiteral
      }

  accessAtVectorBuiltin = functionAccessor AtVector

instance BuiltinHasTensors Builtin where
  accessConstTensorBuiltin = functionAccessor ConstTensor
  accessStackTensorBuiltin = functionAccessor StackTensor
  accessAtTensorBuiltin = functionAccessor AtTensor

instance BuiltinHasForeach Builtin where
  accessForeachTensorBuiltin = functionAccessor ForeachTensor
  accessForeachVectorBuiltin = functionAccessor ForeachVector

instance BuiltinHasStandardTypeClasses Builtin where
  mkBuiltinTypeClass = TypeClass

instance BuiltinHasStandardTypes Builtin where
  accessBuiltinType =
    Access
      { mkExpr = BuiltinType,
        getExpr = \case
          BuiltinType c -> Just c
          _ -> Nothing
      }

instance BuiltinHasStandardData Builtin where
  accessBuiltinFunction =
    Access
      { mkExpr = BuiltinFunction,
        getExpr = \case
          BuiltinFunction c -> Just c
          _ -> Nothing
      }

  accessBuiltinConstructor =
    Access
      { mkExpr = BuiltinConstructor,
        getExpr = \case
          BuiltinConstructor c -> Just c
          _ -> Nothing
      }

instance BuiltinHasIterate Builtin where
  accessIterateBuiltin = functionAccessor Iterate

instance BuiltinHasBinders Builtin where
  getBuiltinBinder = \case
    BuiltinFunction ForeachVector -> Just ForeachBinder
    BuiltinFunction ForeachTensor -> Just ForeachBinder
    BuiltinFunction (QuantifyRatTensor q) -> Just $ QuantifierBinder q
    _ -> Nothing

---------------------------------------------------------------------------------
-- Printing

instance PrintableBuiltin Builtin where
  coercionArgs b = case b of
    BuiltinCast FromNat {} -> Just $ \args -> argExpr $ last args
    BuiltinCast FromRat {} -> Just $ \args -> argExpr $ last args
    TypeClassOp FromNatTC {} -> Just $ \args -> argExpr $ last args
    TypeClassOp FromRatTC {} -> Just $ \args -> argExpr $ last args
    TypeClassOp VecLiteralTC {} -> Just $ \args -> normAppList (Builtin mempty b) args
    _ -> Nothing

  isDerivedBuiltin b = case b of
    DerivedFunction f -> Just $ identifierOf f
    _ -> Nothing

---------------------------------------------------------------------------------
--- Casts

builtinCast :: BuiltinCast -> DSLExpr Builtin
builtinCast = builtin . BuiltinCast

accessFromNatToIndex ::
  (HasBuiltinConstructor expr) =>
  Accessor (expr Builtin) (FromNatToIndexArgs (expr Builtin))
accessFromNatToIndex = accessArgs (castAccessor (FromNat FromNatToRat))

accessFromNatToRat ::
  (HasBuiltinConstructor expr) =>
  Accessor (expr Builtin) (FromNatToSimpleArgs (expr Builtin))
accessFromNatToRat = accessArgs (castAccessor (FromNat FromNatToIndex))

accessFromVectorToList ::
  (HasBuiltinConstructor expr) =>
  Accessor (expr Builtin) (VectorToListArgs (expr Builtin))
accessFromVectorToList = accessArgs (castAccessor FromVectorToList)

isTensorType :: DSLExpr Builtin -> DSLExpr Builtin -> DSLExpr Builtin
isTensorType tElem ds = builtinTypeClass IsTensorType @@ [tElem] .@@ [ds]

builtinDerivedFunction :: DerivedFunction -> DSLExpr Builtin
builtinDerivedFunction = builtin . DerivedFunction
