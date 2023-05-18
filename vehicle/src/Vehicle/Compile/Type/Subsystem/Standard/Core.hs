{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Core where

import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary

-----------------------------------------------------------------------------
-- Types

data StandardBuiltinType
  = StandardBuiltinType BuiltinType
  | StandardTypeClass TypeClass
  | StandardTypeClassOp TypeClassOp
  deriving (Eq, Show, Generic)

instance Pretty StandardBuiltinType where
  pretty = \case
    StandardBuiltinType t -> pretty t
    StandardTypeClass t -> pretty t
    StandardTypeClassOp t -> pretty t

instance PrintableBuiltin StandardBuiltinType where
  convertBuiltin p b = Builtin p $ case b of
    StandardBuiltinType t -> BuiltinType t
    StandardTypeClass t -> TypeClass t
    StandardTypeClassOp t -> TypeClassOp t

  isTypeClassOp = \case
    StandardTypeClassOp {} -> True
    _ -> False

instance Hashable StandardBuiltinType

instance Serialize StandardBuiltinType

convertToNormalisableBuiltins :: DBExpr Builtin -> DBExpr StandardBuiltin
convertToNormalisableBuiltins = traverseBuiltins $ \p1 p2 b args -> do
  let fn = Builtin p2 $ case b of
        Constructor c -> CConstructor c
        BuiltinFunction f -> CFunction f
        BuiltinType t -> CType $ StandardBuiltinType t
        TypeClass t -> CType $ StandardTypeClass t
        TypeClassOp t -> CType $ StandardTypeClassOp t

  normAppList p1 fn args

-----------------------------------------------------------------------------
-- Expressions

type StandardBuiltin = NormalisableBuiltin StandardBuiltinType

type StandardExpr = NormalisableExpr StandardBuiltinType

type StandardBinder = NormalisableBinder StandardBuiltinType

type StandardArg = NormalisableArg StandardBuiltinType

type StandardDecl = NormalisableDecl StandardBuiltinType

type StandardProg = NormalisableProg StandardBuiltinType

type StandardType = StandardExpr

type StandardTelescope = NormalisableTelescope StandardBuiltinType

type StandardTypingBoundCtx = TypingBoundCtx StandardBuiltinType

-----------------------------------------------------------------------------
-- Norm expressions

type StandardNormExpr = NormExpr StandardBuiltinType

type StandardNormBinder = NormBinder StandardBuiltinType

type StandardNormArg = NormArg StandardBuiltinType

type StandardNormType = NormType StandardBuiltinType

type StandardSpine = Spine StandardBuiltinType

type StandardExplicitSpine = ExplicitSpine StandardBuiltinType

type StandardEnv = Env StandardBuiltinType

type StandardNormDeclCtx = NormDeclCtx StandardBuiltinType

-----------------------------------------------------------------------------
-- Glued expressions

type StandardGluedExpr = GluedExpr StandardBuiltinType

type StandardGluedType = GluedType StandardBuiltinType

type StandardGluedProg = GenericProg StandardGluedExpr

type StandardGluedDecl = GenericDecl StandardGluedExpr

type ImportedModules = [StandardGluedProg]

-----------------------------------------------------------------------------
-- Constraints

type StandardConstraintProgress = ConstraintProgress StandardBuiltinType

type StandardTypeClassConstraint = TypeClassConstraint StandardBuiltinType

type StandardUnificationConstraint = UnificationConstraint StandardBuiltinType

type StandardConstraintContext = ConstraintContext StandardBuiltinType

type StandardConstraint = Constraint StandardBuiltinType

-----------------------------------------------------------------------------
-- Normalised patterns

pattern VBuiltinType :: BuiltinType -> StandardExplicitSpine -> StandardNormExpr
pattern VBuiltinType c args = VBuiltin (CType (StandardBuiltinType c)) args

pattern VBoolType :: StandardNormType
pattern VBoolType <- VBuiltinType Bool []
  where
    VBoolType = VBuiltinType Bool []

pattern VIndexType :: StandardNormType -> StandardNormType
pattern VIndexType size <- VBuiltinType Index [size]
  where
    VIndexType size = VBuiltinType Index [size]

pattern VNatType :: StandardNormType
pattern VNatType <- VBuiltinType Nat []
  where
    VNatType = VBuiltinType Nat []

pattern VIntType :: StandardNormType
pattern VIntType <- VBuiltinType Int []
  where
    VIntType = VBuiltinType Int []

pattern VRatType :: StandardNormType
pattern VRatType <- VBuiltinType Rat []
  where
    VRatType = VBuiltinType Rat []

pattern VRawListType :: StandardNormType
pattern VRawListType <- VBuiltinType List []
  where
    VRawListType = VBuiltinType List []

pattern VListType :: StandardNormType -> StandardNormType
pattern VListType tElem <- VBuiltinType List [tElem]

pattern VVectorType :: StandardNormType -> StandardNormExpr -> StandardNormType
pattern VVectorType tElem dim <- VBuiltinType Vector [tElem, dim]
  where
    VVectorType tElem dim = VBuiltinType Vector [tElem, dim]

pattern VTensorType :: StandardNormType -> StandardNormType -> StandardNormType
pattern VTensorType tElem dims <- VFreeVar TensorIdent [ExplicitArg _ tElem, ExplicitArg _ dims]

--------------------------------------------------------------------------------
-- Instance constraints

data InstanceGoal = InstanceGoal
  { goalTelescope :: StandardTelescope,
    goalHead :: TypeClass,
    goalSpine :: StandardExplicitSpine
  }
  deriving (Show)

goalExpr :: InstanceGoal -> StandardNormExpr
goalExpr InstanceGoal {..} = VBuiltin (CType (StandardTypeClass goalHead)) goalSpine

data InstanceCandidate = InstanceCandidate
  { candidateExpr :: StandardExpr,
    candidateSolution :: StandardExpr
  }
  deriving (Show)

type instance
  WithContext InstanceCandidate =
    Contextualised InstanceCandidate StandardTypingBoundCtx

-----------------------------------------------------------------------------

-- * Types post type-checking

-----------------------------------------------------------------------------

type TypeCheckedBinder = DBBinder StandardBuiltin

type TypeCheckedArg = DBArg StandardBuiltin

type TypeCheckedExpr = DBExpr StandardBuiltin

type TypeCheckedType = DBExpr StandardBuiltin

type TypeCheckedDecl = DBDecl StandardBuiltin

type TypeCheckedProg = DBProg StandardBuiltin
