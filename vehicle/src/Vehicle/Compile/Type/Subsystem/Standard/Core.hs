{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Core where

import Data.Aeson (ToJSON)
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
  deriving (Eq, Ord, Show, Generic)

instance Pretty StandardBuiltinType where
  pretty = \case
    StandardBuiltinType t -> pretty t
    StandardTypeClass t -> pretty t
    StandardTypeClassOp t -> pretty t

instance Hashable StandardBuiltinType

instance Serialize StandardBuiltinType

instance ToJSON StandardBuiltinType

convertToNormalisableBuiltins :: Expr Ix Builtin -> Expr Ix StandardBuiltin
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

type StandardExpr = Expr Ix StandardBuiltin

type StandardBinder = Binder Ix StandardBuiltin

type StandardArg = Arg Ix StandardBuiltin

type StandardDecl = Decl Ix StandardBuiltin

type StandardProg = Prog Ix StandardBuiltin

type StandardType = StandardExpr

type StandardTelescope = Telescope Ix StandardBuiltin

type StandardTypingBoundCtx = TypingBoundCtx StandardBuiltin

-----------------------------------------------------------------------------
-- Norm expressions

type StandardNormExpr = Value StandardBuiltin

type StandardNormBinder = VBinder StandardBuiltin

type StandardNormArg = VArg StandardBuiltin

type StandardNormType = VType StandardBuiltin

type StandardSpine = Spine StandardBuiltin

type StandardExplicitSpine = ExplicitSpine StandardBuiltin

type StandardEnv = Env StandardBuiltin

type StandardNormDeclCtx = NormDeclCtx StandardBuiltin

-----------------------------------------------------------------------------
-- Glued expressions

type StandardGluedExpr = GluedExpr StandardBuiltin

type StandardGluedType = GluedType StandardBuiltin

type StandardGluedProg = GenericProg StandardGluedExpr

type StandardGluedDecl = GenericDecl StandardGluedExpr

type ImportedModules = [StandardGluedProg]

-----------------------------------------------------------------------------
-- Constraints

type StandardConstraintProgress = ConstraintProgress StandardBuiltin

type StandardTypeClassConstraint = TypeClassConstraint StandardBuiltin

type StandardUnificationConstraint = UnificationConstraint StandardBuiltin

type StandardConstraintContext = ConstraintContext StandardBuiltin

type StandardConstraint = Constraint StandardBuiltin

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
  where
    VTensorType tElem dims = VFreeVar TensorIdent [ExplicitArg mempty tElem, ExplicitArg mempty dims]

mkRatVectorAdd :: [StandardNormExpr] -> StandardExplicitSpine -> StandardNormExpr
mkRatVectorAdd = mkVectorOp (Add AddRat) StdAddVector

mkRatVectorSub :: [StandardNormExpr] -> StandardExplicitSpine -> StandardNormExpr
mkRatVectorSub = mkVectorOp (Sub SubRat) StdSubVector

mkVectorOp ::
  BuiltinFunction ->
  StdLibFunction ->
  [StandardNormExpr] ->
  StandardExplicitSpine ->
  StandardNormExpr
mkVectorOp baseOp libOp dims spine = case dims of
  [] -> VBuiltinFunction baseOp spine
  (d : ds) ->
    VFreeVar
      (identifierOf libOp)
      ( [ ImplicitArg p vecType,
          ImplicitArg p vecType,
          ImplicitArg p vecType,
          ImplicitArg p d,
          InstanceArg p (mkVectorOp baseOp libOp ds [])
        ]
          <> fmap (ExplicitArg p) spine
      )
    where
      p = mempty; vecType = VTensorType VRatType (mkVList ds)

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

type TypeCheckedBinder = Binder Ix StandardBuiltin

type TypeCheckedArg = Arg Ix StandardBuiltin

type TypeCheckedExpr = Expr Ix StandardBuiltin

type TypeCheckedType = Expr Ix StandardBuiltin

type TypeCheckedDecl = Decl Ix StandardBuiltin

type TypeCheckedProg = Prog Ix StandardBuiltin
