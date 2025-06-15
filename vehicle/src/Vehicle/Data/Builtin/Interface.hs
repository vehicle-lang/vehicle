module Vehicle.Data.Builtin.Interface where

import Vehicle.Data.Builtin.Core
import Vehicle.Data.Tensor (Tensor)
import Vehicle.Syntax.Sugar (BinderType)

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

--------------------------------------------------------------------------------
-- In these classes we need to separate out the types from the literals, as
-- various sets of builtins may have the literals but not the types (e.g.
-- `LinearityBuiltin`)
--------------------------------------------------------------------------------
-- HasBool

type Destruct expr v = expr -> Maybe v

type Construct expr v = v -> expr

data Accessor expr v = Access
  { getExpr :: Destruct expr v,
    mkExpr :: Construct expr v
  }

class BuiltinHasBoolLiterals builtin where
  accessBoolTensorLitBuiltin :: Accessor builtin (Tensor Bool)

  accessNotBuiltin :: Accessor builtin ()
  accessAndBuiltin :: Accessor builtin ()
  accessOrBuiltin :: Accessor builtin ()
  accessImpliesBuiltin :: Accessor builtin ()
  accessReduceAndBuiltin :: Accessor builtin ()
  accessReduceOrBuiltin :: Accessor builtin ()
  accessIfBuiltin :: Accessor builtin ()

  accessCompareIndexBuiltin :: Accessor builtin ComparisonOp
  accessCompareNatBuiltin :: Accessor builtin ComparisonOp
  accessCompareRatTensorPointwiseBuiltin :: Accessor builtin ComparisonOp
  accessCompareRatTensorReducedBuiltin :: Accessor builtin ComparisonOp

  accessQuantifyRatTensorBuiltin :: Accessor builtin Quantifier

class BuiltinHasIndexLiterals builtin where
  accessIndexLitBuiltin :: Accessor builtin Int

class BuiltinHasNatLiterals builtin where
  accessNatLitBuiltin :: Accessor builtin Int
  accessNatTensorLitBuiltin :: Accessor builtin (Tensor Int)

  accessAddNatBuiltin :: Accessor builtin ()
  accessMulNatBuiltin :: Accessor builtin ()

class BuiltinHasNatType builtin where
  accessNatTypeBuiltin :: Accessor builtin ()

class BuiltinHasListLiterals builtin where
  accessNilBuiltin :: Accessor builtin ()
  accessConsBuiltin :: Accessor builtin ()

  accessMapListBuiltin :: Accessor builtin ()
  accessFoldListBuiltin :: Accessor builtin ()

class BuiltinHasVectors builtin where
  accessVecLitBuiltin :: Accessor builtin ()
  accessAtVectorBuiltin :: Accessor builtin ()

class BuiltinHasTensors builtin where
  accessStackTensorBuiltin :: Accessor builtin ()
  accessConstTensorBuiltin :: Accessor builtin ()
  accessAtTensorBuiltin :: Accessor builtin ()

class BuiltinHasForeach builtin where
  accessForeachTensorBuiltin :: Accessor builtin ()
  accessForeachVectorBuiltin :: Accessor builtin ()

class (BuiltinHasTensors builtin) => BuiltinHasRatLiterals builtin where
  accessRatTensorLitBuiltin :: Accessor builtin (Tensor Rational)

  accessNegRatTensorBuiltin :: Accessor builtin ()
  accessAddRatTensorBuiltin :: Accessor builtin ()
  accessMulRatTensorBuiltin :: Accessor builtin ()
  accessSubRatTensorBuiltin :: Accessor builtin ()
  accessDivRatTensorBuiltin :: Accessor builtin ()
  accessMinRatTensorBuiltin :: Accessor builtin ()
  accessMaxRatTensorBuiltin :: Accessor builtin ()
  accessPowRatTensorBuiltin :: Accessor builtin ()
  accessReduceAddRatBuiltin :: Accessor builtin ()
  accessReduceMulRatBuiltin :: Accessor builtin ()
  accessReduceMinRatBuiltin :: Accessor builtin ()
  accessReduceMaxRatBuiltin :: Accessor builtin ()

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class BuiltinHasStandardData builtin where
  accessBuiltinConstructor :: Accessor builtin BuiltinConstructor
  accessBuiltinFunction :: Accessor builtin BuiltinFunction

-- | Indicates that this set of builtins has the standard set of types.
class BuiltinHasStandardTypes builtin where
  accessBuiltinType :: Accessor builtin BuiltinType

class BuiltinHasIterate builtin where
  accessIterateBuiltin :: Accessor builtin ()

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
class BuiltinHasStandardTypeClasses builtin where
  mkBuiltinTypeClass :: TypeClass -> builtin

-- | Indicates that this set of builtins has the standard set of constructors,
-- functions and types.
type HasStandardBuiltins builtin =
  ( BuiltinHasStandardTypes builtin,
    BuiltinHasStandardData builtin
  )

class BuiltinHasBinders builtin where
  getBuiltinBinder :: builtin -> Maybe BinderType
