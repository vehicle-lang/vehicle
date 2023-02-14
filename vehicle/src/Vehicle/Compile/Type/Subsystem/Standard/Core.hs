{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Core where

import Data.Serialize
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary

-----------------------------------------------------------------------------
-- Constraints

type StandardConstraintProgress = ConstraintProgress Builtin

type StandardTypeClassConstraint = TypeClassConstraint Builtin

type StandardUnificationConstraint = UnificationConstraint Builtin

type StandardConstraintContext = ConstraintContext Builtin

type StandardConstraint = Constraint Builtin

-----------------------------------------------------------------------------
-- Norm expressions

type StandardNormExpr = NormExpr Builtin

type StandardNormBinder = NormBinder Builtin

type StandardNormArg = NormArg Builtin

type StandardNormType = NormType Builtin

type StandardSpine = Spine Builtin

type StandardEnv = Env Builtin

pattern VBuiltinFunction :: BuiltinFunction -> StandardSpine -> StandardNormExpr
pattern VBuiltinFunction f spine = VBuiltin (BuiltinFunction f) spine

pattern VConstructor :: BuiltinConstructor -> StandardSpine -> StandardNormExpr
pattern VConstructor c args = VBuiltin (Constructor c) args

pattern VLinearityExpr :: Linearity -> StandardNormExpr
pattern VLinearityExpr l <- VConstructor (Linearity l) []
  where
    VLinearityExpr l = VConstructor (Linearity l) []

pattern VPolarityExpr :: Polarity -> StandardNormExpr
pattern VPolarityExpr l <- VConstructor (Polarity l) []
  where
    VPolarityExpr l = VConstructor (Polarity l) []

pattern VAnnBoolType :: StandardNormExpr -> StandardNormExpr -> StandardNormType
pattern VAnnBoolType lin pol <- VConstructor Bool [IrrelevantImplicitArg _ lin, IrrelevantImplicitArg _ pol]

pattern VBoolType :: StandardNormType
pattern VBoolType <- VConstructor Bool []
  where
    VBoolType = VConstructor Bool []

pattern VIndexType :: StandardNormType -> StandardNormType
pattern VIndexType size <- VConstructor Index [ExplicitArg _ size]

pattern VNatType :: StandardNormType
pattern VNatType <- VConstructor Nat []
  where
    VNatType = VConstructor Nat []

pattern VIntType :: StandardNormType
pattern VIntType <- VConstructor Int []
  where
    VIntType = VConstructor Int []

pattern VAnnRatType :: StandardNormExpr -> StandardNormType
pattern VAnnRatType lin <- VConstructor Rat [IrrelevantImplicitArg _ lin]

pattern VRatType :: StandardNormType
pattern VRatType <- VConstructor Rat []
  where
    VRatType = VConstructor Rat []

pattern VListType :: StandardNormType -> StandardNormType
pattern VListType tElem <- VConstructor List [ExplicitArg _ tElem]

pattern VVectorType :: StandardNormType -> StandardNormType -> StandardNormType
pattern VVectorType tElem dim <- VConstructor Vector [ExplicitArg _ tElem, ExplicitArg _ dim]

pattern VTensorType :: StandardNormType -> StandardNormType -> StandardNormType
pattern VTensorType tElem dims <- VFreeVar TensorIdent [ExplicitArg _ tElem, ExplicitArg _ dims]

mkVList :: StandardNormType -> [StandardNormExpr] -> StandardNormExpr
mkVList tElem = foldr cons nil
  where
    p = mempty
    t = ImplicitArg p tElem
    nil = VConstructor Nil [t]
    cons y ys = VConstructor Cons [t, ExplicitArg p y, ExplicitArg p ys]

-----------------------------------------------------------------------------
-- Glued expressions

type StandardGluedExpr = GluedExpr Builtin

type StandardGluedType = GluedType Builtin

type StandardTypedExpr = TypedExpr Builtin

instance Serialize StandardTypedExpr

type StandardTypedProg = GenericProg StandardTypedExpr

type StandardTypedDecl = GenericDecl StandardTypedExpr

type ImportedModules = [StandardTypedProg]
