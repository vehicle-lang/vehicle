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
data NormExpr builtin
  = VUniverse Universe
  | VLiteral Literal
  | VLam (NormBinder builtin) (Env builtin) (Expr DBBinding DBIndexVar builtin)
  | VPi (NormBinder builtin) (NormExpr builtin)
  | VLVec [NormExpr builtin] (Spine builtin)
  | VMeta MetaID (Spine builtin)
  | VFreeVar Identifier (Spine builtin)
  | VBoundVar DBLevel (Spine builtin)
  | VBuiltin builtin (Spine builtin)
  deriving (Eq, Show, Generic)

instance Serialize builtin => Serialize (NormExpr builtin)

type NormArg builtin = GenericArg (NormExpr builtin)

type NormBinder builtin = GenericBinder DBBinding (NormType builtin)

type NormDecl builtin = GenericDecl (NormExpr builtin)

type NormProg builtin = GenericDecl builtin

-- | A normalised type
type NormType builtin = NormExpr builtin

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine builtin = [NormArg builtin]

type Env builtin = BoundCtx (NormExpr builtin)

mkNoOpEnv :: DBLevel -> Env builtin
mkNoOpEnv boundCtxSize = [VBoundVar i [] | i <- reverse [0 .. boundCtxSize - 1]]

liftEnvOverBinder :: Env builtin -> Env builtin
liftEnvOverBinder = (VBoundVar 0 [] :)

-----------------------------------------------------------------------------
-- Norm expressions with the basic set of builtins

type BasicNormExpr = NormExpr Builtin

type BasicNormBinder = NormBinder Builtin

type BasicNormArg = NormArg Builtin

type BasicNormType = NormType Builtin

type BasicSpine = Spine Builtin

type BasicEnv = Env Builtin

-----------------------------------------------------------------------------
-- Patterns

pattern VBuiltinFunction :: BuiltinFunction -> BasicSpine -> BasicNormExpr
pattern VBuiltinFunction f spine = VBuiltin (BuiltinFunction f) spine

pattern VTypeUniverse :: UniverseLevel -> NormType builtin
pattern VTypeUniverse l = VUniverse (TypeUniv l)

pattern VPolarityUniverse :: NormExpr builtin
pattern VPolarityUniverse = VUniverse PolarityUniv

pattern VLinearityUniverse :: NormExpr builtin
pattern VLinearityUniverse = VUniverse PolarityUniv

pattern VUnitLiteral :: NormExpr builtin
pattern VUnitLiteral = VLiteral LUnit

pattern VBoolLiteral :: Bool -> NormExpr builtin
pattern VBoolLiteral x = VLiteral (LBool x)

pattern VIndexLiteral :: Int -> Int -> NormExpr builtin
pattern VIndexLiteral n x = VLiteral (LIndex n x)

pattern VNatLiteral :: Int -> NormExpr builtin
pattern VNatLiteral x = VLiteral (LNat x)

pattern VIntLiteral :: Int -> NormExpr builtin
pattern VIntLiteral x = VLiteral (LInt x)

pattern VRatLiteral :: Rational -> NormExpr builtin
pattern VRatLiteral x = VLiteral (LRat x)

pattern VConstructor :: BuiltinConstructor -> BasicSpine -> BasicNormExpr
pattern VConstructor c args = VBuiltin (Constructor c) args

pattern VLinearityExpr :: Linearity -> BasicNormExpr
pattern VLinearityExpr l <- VConstructor (Linearity l) []
  where
    VLinearityExpr l = VConstructor (Linearity l) []

pattern VPolarityExpr :: Polarity -> BasicNormExpr
pattern VPolarityExpr l <- VConstructor (Polarity l) []
  where
    VPolarityExpr l = VConstructor (Polarity l) []

pattern VAnnBoolType :: BasicNormExpr -> BasicNormExpr -> BasicNormType
pattern VAnnBoolType lin pol <- VConstructor Bool [IrrelevantImplicitArg _ lin, IrrelevantImplicitArg _ pol]

pattern VBoolType :: BasicNormType
pattern VBoolType <- VConstructor Bool []
  where
    VBoolType = VConstructor Bool []

pattern VIndexType :: BasicNormType -> BasicNormType
pattern VIndexType size <- VConstructor Index [ExplicitArg _ size]

pattern VNatType :: BasicNormType
pattern VNatType <- VConstructor Nat []
  where
    VNatType = VConstructor Nat []

pattern VIntType :: BasicNormType
pattern VIntType <- VConstructor Int []
  where
    VIntType = VConstructor Int []

pattern VAnnRatType :: BasicNormExpr -> BasicNormType
pattern VAnnRatType lin <- VConstructor Rat [IrrelevantImplicitArg _ lin]

pattern VRatType :: BasicNormType
pattern VRatType <- VConstructor Rat []
  where
    VRatType = VConstructor Rat []

pattern VListType :: BasicNormType -> BasicNormType
pattern VListType tElem <- VConstructor List [ExplicitArg _ tElem]

pattern VVectorType :: BasicNormType -> BasicNormType -> BasicNormType
pattern VVectorType tElem dim <- VConstructor Vector [ExplicitArg _ tElem, ExplicitArg _ dim]

pattern VTensorType :: BasicNormType -> BasicNormType -> BasicNormType
pattern VTensorType tElem dims <- VFreeVar TensorIdent [ExplicitArg _ tElem, ExplicitArg _ dims]

mkVList :: BasicNormType -> [BasicNormExpr] -> BasicNormExpr
mkVList tElem = foldr cons nil
  where
    p = mempty
    t = ImplicitArg p tElem
    nil = VConstructor Nil [t]
    cons y ys = VConstructor Cons [t, ExplicitArg p y, ExplicitArg p ys]

mkVLVec :: [NormExpr builtin] -> NormExpr builtin -> NormExpr builtin
mkVLVec xs t = VLVec xs [ImplicitArg mempty t, InstanceArg mempty VUnitLiteral]

isNTypeUniverse :: NormExpr builtin -> Bool
isNTypeUniverse (VUniverse TypeUniv {}) = True
isNTypeUniverse _ = False

isNPolarityUniverse :: NormExpr builtin -> Bool
isNPolarityUniverse (VUniverse PolarityUniv {}) = True
isNPolarityUniverse _ = False

isNLinearityUniverse :: NormExpr builtin -> Bool
isNLinearityUniverse (VUniverse LinearityUniv {}) = True
isNLinearityUniverse _ = False

isNAuxiliaryUniverse :: NormExpr builtin -> Bool
isNAuxiliaryUniverse e = isNPolarityUniverse e || isNLinearityUniverse e

isMeta :: NormExpr builtin -> Bool
isMeta VMeta {} = True
isMeta _ = False

getMeta :: NormExpr builtin -> Maybe MetaID
getMeta (VMeta m _) = Just m
getMeta _ = Nothing

isBoolType :: BasicNormExpr -> Bool
isBoolType (VConstructor Bool _) = True
isBoolType _ = False

isIndexType :: BasicNormExpr -> Bool
isIndexType (VConstructor Index _) = True
isIndexType _ = False

isNatType :: BasicNormExpr -> Bool
isNatType (VConstructor Nat _) = True
isNatType _ = False

isIntType :: BasicNormExpr -> Bool
isIntType (VConstructor Int _) = True
isIntType _ = False

isRatType :: BasicNormExpr -> Bool
isRatType (VConstructor Rat _) = True
isRatType _ = False

isListType :: BasicNormExpr -> Bool
isListType (VConstructor List _) = True
isListType _ = False

isVectorType :: BasicNormExpr -> Bool
isVectorType (VConstructor Vector _) = True
isVectorType _ = False

isBoundVar :: BasicNormExpr -> Bool
isBoundVar VBoundVar {} = True
isBoundVar _ = False

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr = Glued
  { unnormalised :: DBExpr Builtin,
    normalised :: BasicNormExpr
  }
  deriving (Show, Generic)

instance Serialize GluedExpr

instance HasProvenance GluedExpr where
  provenanceOf = provenanceOf . unnormalised

type GluedArg = GenericArg GluedExpr

type GluedType = GluedExpr

type GluedProg = GenericProg GluedExpr

type GluedDecl = GenericDecl GluedExpr

traverseNormalised :: Monad m => (BasicNormExpr -> m BasicNormExpr) -> GluedExpr -> m GluedExpr
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: Monad m => (DBExpr Builtin -> m (DBExpr Builtin)) -> GluedExpr -> m GluedExpr
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
