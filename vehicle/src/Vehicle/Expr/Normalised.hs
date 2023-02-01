module Vehicle.Expr.Normalised where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude.Contexts (BoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data NormExpr
  = VUniverse Universe
  | VLiteral Literal
  | VLam NormBinder Env DBExpr
  | VPi NormBinder NormExpr
  | VLVec [NormExpr] Spine
  | VMeta MetaID Spine
  | VFreeVar Identifier Spine
  | VBoundVar DBLevel Spine
  | VBuiltin Builtin Spine
  deriving (Eq, Show, Generic)

instance Serialize NormExpr

type NormArg = GenericArg NormExpr

type NormBinder = GenericBinder DBBinding NormType

type NormDecl = GenericDecl NormExpr

type NormProg = GenericDecl

-- | A normalised type
type NormType = NormExpr

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine = [NormArg]

type Env = BoundCtx NormExpr

mkNoOpEnv :: DBLevel -> Env
mkNoOpEnv boundCtxSize = [VBoundVar i [] | i <- reverse [0 .. boundCtxSize - 1]]

liftEnvOverBinder :: Env -> Env
liftEnvOverBinder = (VBoundVar 0 [] :)

-----------------------------------------------------------------------------
-- Patterns

pattern VBuiltinFunction :: BuiltinFunction -> Spine -> NormExpr
pattern VBuiltinFunction f spine = VBuiltin (BuiltinFunction f) spine

pattern VTypeUniverse :: UniverseLevel -> NormType
pattern VTypeUniverse l = VUniverse (TypeUniv l)

pattern VPolarityUniverse :: NormExpr
pattern VPolarityUniverse = VUniverse PolarityUniv

pattern VLinearityUniverse :: NormExpr
pattern VLinearityUniverse = VUniverse PolarityUniv

pattern VUnitLiteral :: NormExpr
pattern VUnitLiteral = VLiteral LUnit

pattern VBoolLiteral :: Bool -> NormExpr
pattern VBoolLiteral x = VLiteral (LBool x)

pattern VIndexLiteral :: Int -> Int -> NormExpr
pattern VIndexLiteral n x = VLiteral (LIndex n x)

pattern VNatLiteral :: Int -> NormExpr
pattern VNatLiteral x = VLiteral (LNat x)

pattern VIntLiteral :: Int -> NormExpr
pattern VIntLiteral x = VLiteral (LInt x)

pattern VRatLiteral :: Rational -> NormExpr
pattern VRatLiteral x = VLiteral (LRat x)

pattern VConstructor :: BuiltinConstructor -> [GenericArg NormExpr] -> NormExpr
pattern VConstructor c args = VBuiltin (Constructor c) args

pattern VLinearityExpr :: Linearity -> NormExpr
pattern VLinearityExpr l <- VConstructor (Linearity l) []
  where
    VLinearityExpr l = VConstructor (Linearity l) []

pattern VPolarityExpr :: Polarity -> NormExpr
pattern VPolarityExpr l <- VConstructor (Polarity l) []
  where
    VPolarityExpr l = VConstructor (Polarity l) []

pattern VAnnBoolType :: NormExpr -> NormExpr -> NormType
pattern VAnnBoolType lin pol <- VConstructor Bool [IrrelevantImplicitArg _ lin, IrrelevantImplicitArg _ pol]

pattern VBoolType :: NormType
pattern VBoolType <- VConstructor Bool []
  where
    VBoolType = VConstructor Bool []

pattern VIndexType :: NormType -> NormType
pattern VIndexType size <- VConstructor Index [ExplicitArg _ size]

pattern VNatType :: NormType
pattern VNatType <- VConstructor Nat []
  where
    VNatType = VConstructor Nat []

pattern VIntType :: NormType
pattern VIntType <- VConstructor Int []
  where
    VIntType = VConstructor Int []

pattern VAnnRatType :: NormExpr -> NormType
pattern VAnnRatType lin <- VConstructor Rat [IrrelevantImplicitArg _ lin]

pattern VRatType :: NormType
pattern VRatType <- VConstructor Rat []
  where
    VRatType = VConstructor Rat []

pattern VListType :: NormType -> NormType
pattern VListType tElem <- VConstructor List [ExplicitArg _ tElem]

pattern VVectorType :: NormType -> NormType -> NormType
pattern VVectorType tElem dim <- VConstructor Vector [ExplicitArg _ tElem, ExplicitArg _ dim]

pattern VTensorType :: NormType -> NormType -> NormType
pattern VTensorType tElem dims <- VFreeVar TensorIdent [ExplicitArg _ tElem, ExplicitArg _ dims]

mkVList :: NormType -> [NormExpr] -> NormExpr
mkVList tElem = foldr cons nil
  where
    p = mempty
    t = ImplicitArg p tElem
    nil = VConstructor Nil [t]
    cons y ys = VConstructor Cons [t, ExplicitArg p y, ExplicitArg p ys]

mkVLVec :: [NormExpr] -> NormExpr -> NormExpr
mkVLVec xs t = VLVec xs [ImplicitArg mempty t, InstanceArg mempty VUnitLiteral]

isNTypeUniverse :: NormExpr -> Bool
isNTypeUniverse (VUniverse TypeUniv {}) = True
isNTypeUniverse _ = False

isNPolarityUniverse :: NormExpr -> Bool
isNPolarityUniverse (VUniverse PolarityUniv {}) = True
isNPolarityUniverse _ = False

isNLinearityUniverse :: NormExpr -> Bool
isNLinearityUniverse (VUniverse LinearityUniv {}) = True
isNLinearityUniverse _ = False

isNAuxiliaryUniverse :: NormExpr -> Bool
isNAuxiliaryUniverse e = isNPolarityUniverse e || isNLinearityUniverse e

isMeta :: NormExpr -> Bool
isMeta VMeta {} = True
isMeta _ = False

getMeta :: NormExpr -> Maybe MetaID
getMeta (VMeta m _) = Just m
getMeta _ = Nothing

isBoolType :: NormExpr -> Bool
isBoolType (VConstructor Bool _) = True
isBoolType _ = False

isIndexType :: NormExpr -> Bool
isIndexType (VConstructor Index _) = True
isIndexType _ = False

isNatType :: NormExpr -> Bool
isNatType (VConstructor Nat _) = True
isNatType _ = False

isIntType :: NormExpr -> Bool
isIntType (VConstructor Int _) = True
isIntType _ = False

isRatType :: NormExpr -> Bool
isRatType (VConstructor Rat _) = True
isRatType _ = False

isListType :: NormExpr -> Bool
isListType (VConstructor List _) = True
isListType _ = False

isVectorType :: NormExpr -> Bool
isVectorType (VConstructor Vector _) = True
isVectorType _ = False

isBoundVar :: NormExpr -> Bool
isBoundVar VBoundVar {} = True
isBoundVar _ = False

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr = Glued
  { unnormalised :: DBExpr,
    normalised :: NormExpr
  }
  deriving (Show, Generic)

instance Serialize GluedExpr

instance HasProvenance GluedExpr where
  provenanceOf = provenanceOf . unnormalised

type GluedArg = GenericArg GluedExpr

type GluedType = GluedExpr

type GluedProg = GenericProg GluedExpr

type GluedDecl = GenericDecl GluedExpr

traverseNormalised :: Monad m => (NormExpr -> m NormExpr) -> GluedExpr -> m GluedExpr
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: Monad m => (DBExpr -> m DBExpr) -> GluedExpr -> m GluedExpr
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
