{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Core
  ( module Syntax,
    Builtin (..),
    builtinCast,
    accessFromNatToIndex,
    accessFromNatToRat,
    accessFromVectorToList,
    isTensorType,
    builtinDerivedFunction,
    builtinSymbols,
    builtinFromSymbol,
    symbolFromBuiltin,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import Data.Text (Text)
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Core as Syntax
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.DSL
import Vehicle.Prelude

-----------------------------------------------------------------------------
-- Definition

-- | Builtins in the Vehicle language
data Builtin
  = BuiltinConstructor BuiltinConstructor
  | BuiltinFunction BuiltinFunction
  | BuiltinType BuiltinType
  | BuiltinCast BuiltinCast
  | DerivedFunction DerivedFunction
  | TypeClass TypeClass
  | TypeClassOp TypeClassOp
  | NatInDomainConstraint
  deriving (Eq, Ord, Show, Generic)

instance NFData Builtin

instance Hashable Builtin

instance Serialize Builtin

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.
instance Pretty Builtin where
  pretty = \case
    BuiltinFunction f -> pretty f
    BuiltinType t -> pretty t
    BuiltinConstructor c -> pretty c
    BuiltinCast c -> pretty c
    DerivedFunction f -> pretty f
    TypeClass tc -> pretty tc
    TypeClassOp o -> pretty o
    NatInDomainConstraint {} -> "NatInDomainConstraint"

builtinSymbols :: [(Text, Builtin)]
builtinSymbols = mempty

builtinFromSymbol :: Text -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Text
symbolFromBuiltin b = layoutAsText $ pretty b

-----------------------------------------------------------------------------
-- Accessors

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

compareRatTensorAccessor :: Accessor Builtin ComparisonOp
compareRatTensorAccessor =
  Access
    { getExpr = \case
        BuiltinFunction (CompareRatTensor op) -> Just op
        _ -> Nothing,
      mkExpr = \op -> BuiltinFunction (CompareRatTensor op)
    }

--------------------------------------------------------------------------------
-- Bool

instance BuiltinHasBoolType Builtin where
  accessBoolTypeBuiltin = typeAccessor BoolType

instance BuiltinHasBoolLiterals Builtin where
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
  accessCompareRatTensorBuiltin = compareRatTensorAccessor

  accessQuantifyRatTensorBuiltin =
    Access
      { getExpr = \case
          BuiltinFunction (QuantifyRatTensor q) -> Just q
          _ -> Nothing,
        mkExpr = BuiltinFunction . QuantifyRatTensor
      }

  accessQuantifyRecordBuiltin =
    Access
      { getExpr = \case
          BuiltinFunction (QuantifyRecord q) -> Just q
          _ -> Nothing,
        mkExpr = BuiltinFunction . QuantifyRecord
      }

--------------------------------------------------------------------------------
-- Index

instance BuiltinHasIndexType Builtin where
  accessIndexTypeBuiltin = typeAccessor IndexType

instance BuiltinHasIndexLiterals Builtin where
  accessIndexLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (IndexLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = BuiltinConstructor . IndexLiteral
      }

--------------------------------------------------------------------------------
-- Nat

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

--------------------------------------------------------------------------------
-- Rat

instance BuiltinHasRatType Builtin where
  accessRatTypeBuiltin = typeAccessor RatType

instance BuiltinHasRatLiterals Builtin where
  accessRatTensorLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor (RatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = BuiltinConstructor . RatTensorLiteral
      }

  accessNegRatTensorBuiltin = functionAccessor $ Neg NegRatTensor
  accessLogRatTensorBuiltin = functionAccessor $ Log LogRatTensor
  accessExpRatTensorBuiltin = functionAccessor $ Exp ExpRatTensor
  accessAddRatTensorBuiltin = functionAccessor $ Add AddRatTensor
  accessMulRatTensorBuiltin = functionAccessor $ Mul MulRatTensor
  accessSubRatTensorBuiltin = functionAccessor $ Sub SubRatTensor
  accessDivRatTensorBuiltin = functionAccessor $ Div DivRatTensor
  accessMinRatTensorBuiltin = functionAccessor $ Min MinRatTensor
  accessMaxRatTensorBuiltin = functionAccessor $ Max MaxRatTensor
  accessPowRatTensorBuiltin = functionAccessor $ Pow PowRatTensor
  accessReduceAddRatBuiltin = functionAccessor ReduceAddRatTensor
  accessReduceMulRatBuiltin = functionAccessor ReduceMulRatTensor
  accessReduceMinRatBuiltin = functionAccessor ReduceMinRatTensor
  accessReduceMaxRatBuiltin = functionAccessor ReduceMaxRatTensor

--------------------------------------------------------------------------------
-- List

instance BuiltinHasListType Builtin where
  accessListTypeBuiltin = typeAccessor ListType

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

--------------------------------------------------------------------------------
-- Vector

instance BuiltinHasVectorType Builtin where
  accessVectorTypeBuiltin = typeAccessor VectorType

instance BuiltinHasVectors Builtin where
  accessVecLitBuiltin =
    Access
      { getExpr = \case
          BuiltinConstructor VectorLiteral -> Just ()
          _ -> Nothing,
        mkExpr = \() -> BuiltinConstructor VectorLiteral
      }

  accessAtVectorBuiltin = functionAccessor AtVector

--------------------------------------------------------------------------------
-- Tensor

instance BuiltinHasTensorType Builtin where
  accessTensorTypeBuiltin = typeAccessor TensorType

instance BuiltinHasTensors Builtin where
  accessConstTensorBuiltin = functionAccessor ConstTensor
  accessStackTensorBuiltin = functionAccessor StackTensor
  accessAtTensorBuiltin = functionAccessor AtTensor

--------------------------------------------------------------------------------
-- Others

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
