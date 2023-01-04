module Vehicle.Expr.Normalised where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude.Contexts (BoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
--
-- TODO - make generic over WHNF.
data NormExpr
  = VUniverse Provenance Universe
  | VLiteral Provenance Literal
  | VLam Provenance NormBinder Env DBExpr
  | VPi Provenance NormBinder NormExpr
  | VLVec Provenance [NormExpr] Spine
  | VMeta Provenance MetaID Spine
  | VFreeVar Provenance Identifier Spine
  | VBoundVar Provenance DBLevel Spine
  | VBuiltin Provenance Builtin Spine
  deriving (Show, Generic)

instance ToJSON NormExpr

instance FromJSON NormExpr

instance HasProvenance NormExpr where
  provenanceOf = \case
    VUniverse p _ -> p
    VLiteral p _ -> p
    VLam p _ _ _ -> p
    VPi p _ _ -> p
    VLVec p _ _ -> p
    VMeta p _ _ -> p
    VFreeVar p _ _ -> p
    VBoundVar p _ _ -> p
    VBuiltin p _ _ -> p

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

liftEnvOverBinder :: Provenance -> Env -> Env
liftEnvOverBinder p = (VBoundVar p 0 [] :)

-----------------------------------------------------------------------------
-- Patterns

pattern VTypeUniverse :: Provenance -> UniverseLevel -> NormType
pattern VTypeUniverse p l = VUniverse p (TypeUniv l)

pattern VPolarityUniverse :: Provenance -> NormExpr
pattern VPolarityUniverse p = VUniverse p PolarityUniv

pattern VLinearityUniverse :: Provenance -> NormExpr
pattern VLinearityUniverse p = VUniverse p PolarityUniv

pattern VUnitLiteral :: Provenance -> NormExpr
pattern VUnitLiteral p = VLiteral p LUnit

pattern VBoolLiteral :: Provenance -> Bool -> NormExpr
pattern VBoolLiteral p x = VLiteral p (LBool x)

pattern VIndexLiteral :: Provenance -> Int -> Int -> NormExpr
pattern VIndexLiteral p n x = VLiteral p (LIndex n x)

pattern VNatLiteral :: Provenance -> Int -> NormExpr
pattern VNatLiteral p x = VLiteral p (LNat x)

pattern VIntLiteral :: Provenance -> Int -> NormExpr
pattern VIntLiteral p x = VLiteral p (LInt x)

pattern VRatLiteral :: Provenance -> Rational -> NormExpr
pattern VRatLiteral p x = VLiteral p (LRat x)

pattern VConstructor :: Provenance -> BuiltinConstructor -> [GenericArg NormExpr] -> NormExpr
pattern VConstructor p c args = VBuiltin p (Constructor c) args

pattern VLinearityExpr :: Provenance -> Linearity -> NormExpr
pattern VLinearityExpr p l <- VConstructor p (Linearity l) []
  where
    VLinearityExpr p l = VConstructor p (Linearity l) []

pattern VPolarityExpr :: Provenance -> Polarity -> NormExpr
pattern VPolarityExpr p l <- VConstructor p (Polarity l) []
  where
    VPolarityExpr p l = VConstructor p (Polarity l) []

pattern VAnnBoolType :: Provenance -> NormExpr -> NormExpr -> NormType
pattern VAnnBoolType p lin pol <- VConstructor p Bool [IrrelevantImplicitArg _ lin, IrrelevantImplicitArg _ pol]
  where
    VAnnBoolType p lin pol = VConstructor p Bool [IrrelevantImplicitArg p lin, IrrelevantImplicitArg p pol]

pattern VBoolType :: Provenance -> NormType
pattern VBoolType p <- VConstructor p Bool []
  where
    VBoolType p = VConstructor p Bool []

pattern VIndexType :: Provenance -> NormType -> NormType
pattern VIndexType p size <- VConstructor p Index [ExplicitArg _ size]
  where
    VIndexType p size = VConstructor p Index [ExplicitArg p size]

pattern VNatType :: Provenance -> NormType
pattern VNatType p <- VConstructor p Nat []
  where
    VNatType p = VConstructor p Nat []

pattern VIntType :: Provenance -> NormType
pattern VIntType p <- VConstructor p Int []
  where
    VIntType p = VConstructor p Int []

pattern VAnnRatType :: Provenance -> NormExpr -> NormType
pattern VAnnRatType p lin <- VConstructor p Rat [IrrelevantImplicitArg _ lin]
  where
    VAnnRatType p lin = VConstructor p Rat [IrrelevantImplicitArg p lin]

pattern VRatType :: Provenance -> NormType
pattern VRatType p <- VConstructor p Rat []
  where
    VRatType p = VConstructor p Rat []

pattern VListType :: Provenance -> NormType -> NormType
pattern VListType p tElem <- VConstructor p List [ExplicitArg _ tElem]
  where
    VListType p tElem = VConstructor p List [ExplicitArg p tElem]

pattern VVectorType :: Provenance -> NormType -> NormType -> NormType
pattern VVectorType p tElem dim <- VConstructor p Vector [ExplicitArg _ tElem, ExplicitArg _ dim]
  where
    VVectorType p tElem dim = VConstructor p Vector [ExplicitArg p tElem, ExplicitArg p dim]

pattern VTensorType :: Provenance -> NormType -> NormType -> NormType
pattern VTensorType p tElem dims <- VFreeVar p TensorIdent [ExplicitArg _ tElem, ExplicitArg _ dims]
  where
    VTensorType p tElem dims = VFreeVar p TensorIdent [ExplicitArg p tElem, ExplicitArg p dims]

mkNList :: Provenance -> NormType -> [NormExpr] -> NormExpr
mkNList p tElem = foldr cons nil
  where
    t = ImplicitArg p tElem
    nil = VConstructor p Nil [t]
    cons y ys = VConstructor p Cons [t, ExplicitArg p y, ExplicitArg p ys]

mkVLVec :: Provenance -> [NormExpr] -> NormExpr -> NormExpr
mkVLVec p xs t = VLVec p xs [ImplicitArg p t, InstanceArg p (VUnitLiteral p)]

isNTypeUniverse :: NormExpr -> Bool
isNTypeUniverse (VUniverse _ TypeUniv {}) = True
isNTypeUniverse _ = False

isNPolarityUniverse :: NormExpr -> Bool
isNPolarityUniverse (VUniverse _ PolarityUniv {}) = True
isNPolarityUniverse _ = False

isNLinearityUniverse :: NormExpr -> Bool
isNLinearityUniverse (VUniverse _ LinearityUniv {}) = True
isNLinearityUniverse _ = False

isNAuxiliaryUniverse :: NormExpr -> Bool
isNAuxiliaryUniverse e = isNPolarityUniverse e || isNLinearityUniverse e

isMeta :: NormExpr -> Bool
isMeta VMeta {} = True
isMeta _ = False

getMeta :: NormExpr -> Maybe MetaID
getMeta (VMeta _ m _) = Just m
getMeta _ = Nothing

isBoolType :: NormExpr -> Bool
isBoolType (VConstructor _ Bool _) = True
isBoolType _ = False

isIndexType :: NormExpr -> Bool
isIndexType (VConstructor _ Index _) = True
isIndexType _ = False

isNatType :: NormExpr -> Bool
isNatType (VConstructor _ Nat _) = True
isNatType _ = False

isIntType :: NormExpr -> Bool
isIntType (VConstructor _ Int _) = True
isIntType _ = False

isRatType :: NormExpr -> Bool
isRatType (VConstructor _ Rat _) = True
isRatType _ = False

isListType :: NormExpr -> Bool
isListType (VConstructor _ List _) = True
isListType _ = False

isVectorType :: NormExpr -> Bool
isVectorType (VConstructor _ Vector _) = True
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

instance ToJSON GluedExpr

instance FromJSON GluedExpr

instance HasProvenance GluedExpr where
  provenanceOf = provenanceOf . unnormalised

type GluedType = GluedExpr

type GluedProg = GenericProg GluedExpr

type GluedDecl = GenericDecl GluedExpr

traverseNormalised :: Monad m => (NormExpr -> m NormExpr) -> GluedExpr -> m GluedExpr
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: Monad m => (DBExpr -> m DBExpr) -> GluedExpr -> m GluedExpr
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
