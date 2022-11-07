module Vehicle.Compile.Normalise.NormExpr where

import Vehicle.Compile.Prelude

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
--
-- TODO - make generic over WHNF.
data NormExpr
  = VUniverse Provenance Universe
  | VLiteral  Provenance Literal
  | VLam      Provenance NormBinder Env CheckedExpr
  | VPi       Provenance NormBinder NormExpr
  | VLVec     Provenance [NormExpr]
  | VMeta     Provenance Meta Spine
  | VVar      Provenance DBVar Spine
  | VBuiltin  Provenance Builtin Spine
  deriving (Show)

instance HasProvenance NormExpr where
  provenanceOf = \case
    VUniverse p _     -> p
    VLiteral  p _     -> p
    VLam      p _ _ _ -> p
    VPi       p _ _   -> p
    VLVec     p _     -> p
    VMeta     p _ _   -> p
    VVar      p _ _   -> p
    VBuiltin  p _ _   -> p

type NormArg = GenericArg NormExpr
type NormBinder = GenericBinder DBBinding NormType

-- | A normalised type
type NormType = NormExpr

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine = [NormArg]

type Env = [NormExpr]

-----------------------------------------------------------------------------
-- Patterns

pattern VTypeUniverse :: Provenance -> UniverseLevel -> NormType
pattern VTypeUniverse p l = VUniverse p (TypeUniv l)

pattern VPolarityUniverse :: Provenance -> NormExpr
pattern VPolarityUniverse p = VUniverse p PolarityUniv

pattern VLinearityUniverse :: Provenance -> NormExpr
pattern VLinearityUniverse p = VUniverse p PolarityUniv


pattern VUnitLit :: Provenance -> NormExpr
pattern VUnitLit p = VLiteral p LUnit

pattern VBoolLit :: Provenance -> Bool -> NormExpr
pattern VBoolLit p x = VLiteral p (LBool x)

-- pattern VIndexLit :: Provenance -> Int -> NormExpr
-- pattern VIndexLit p x <- VLiteral p (LIndex _ x)

pattern VNatLit :: Provenance -> Int -> NormExpr
pattern VNatLit p x = VLiteral p (LNat x)

pattern VIntLit :: Provenance -> Int -> NormExpr
pattern VIntLit p x = VLiteral p (LInt x)

pattern VRatLit :: Provenance -> Rational -> NormExpr
pattern VRatLit p x = VLiteral p (LRat x)

pattern VConstructor :: Provenance -> BuiltinConstructor -> [GenericArg NormExpr] -> NormExpr
pattern VConstructor p c args = VBuiltin p (Constructor c) args

pattern VLinearity :: Provenance -> Linearity -> NormExpr
pattern VLinearity p l <- VConstructor p (Linearity l) []
  where VLinearity p l =  VConstructor p (Linearity l) []

pattern VPolarity :: Provenance -> Polarity -> NormExpr
pattern VPolarity p l <- VConstructor p (Polarity l) []
  where VPolarity p l =  VConstructor p (Polarity l) []

pattern VAnnBoolType :: Provenance -> NormExpr -> NormExpr -> NormType
pattern VAnnBoolType p lin pol <- VConstructor p Bool [IrrelevantImplicitArg _ lin, IrrelevantImplicitArg _ pol]
  where VAnnBoolType p lin pol =  VConstructor p Bool [IrrelevantImplicitArg p lin, IrrelevantImplicitArg p pol]

pattern VIndexType :: Provenance -> NormType -> NormType
pattern VIndexType p size <- VConstructor p Index [ExplicitArg _ size]
  where VIndexType p size =  VConstructor p Index [ExplicitArg p size]

pattern VNatType :: Provenance -> NormType
pattern VNatType p <- VConstructor p Nat []
  where VNatType p =  VConstructor p Nat []

pattern VIntType :: Provenance -> NormType
pattern VIntType p <- VConstructor p Int []
  where VIntType p =  VConstructor p Int []

pattern VAnnRatType :: Provenance -> NormExpr -> NormType
pattern VAnnRatType p lin <- VConstructor p Rat [IrrelevantImplicitArg _ lin]
  where VAnnRatType p lin =  VConstructor p Rat [IrrelevantImplicitArg p lin]

pattern VRatType :: Provenance -> NormType
pattern VRatType p <- VConstructor p Rat []
  where VRatType p =  VConstructor p Rat []

pattern VListType :: Provenance -> NormType -> NormType
pattern VListType p tElem <- VConstructor p List [ExplicitArg _ tElem]
  where VListType p tElem =  VConstructor p List [ExplicitArg p tElem]

pattern VVectorType :: Provenance -> NormType -> NormType -> NormType
pattern VVectorType p tElem dim <- VConstructor p Vector [ExplicitArg _ tElem, ExplicitArg _ dim]
  where VVectorType p tElem dim =  VConstructor p Vector [ExplicitArg p tElem, ExplicitArg p dim]

pattern VTensorType :: Provenance -> NormType -> NormType -> NormType
pattern VTensorType p tElem dims <- VBuiltin p Tensor [ExplicitArg _ tElem, ExplicitArg _ dims]
  where VTensorType p tElem dims =  VBuiltin p Tensor [ExplicitArg p tElem, ExplicitArg p dims]

mkNList :: Provenance -> NormType -> [NormExpr] -> NormExpr
mkNList p tElem = foldr cons nil
  where
    t         = ExplicitArg p tElem
    nil       = VConstructor p Nil [t]
    cons y ys = VConstructor p Cons [t, ExplicitArg p y, ExplicitArg p ys]

isNTypeUniverse :: NormExpr -> Bool
isNTypeUniverse (VUniverse _ TypeUniv{}) = True
isNTypeUniverse _                        = False

isNPolarityUniverse :: NormExpr -> Bool
isNPolarityUniverse (VUniverse _ PolarityUniv{}) = True
isNPolarityUniverse _                            = False

isNLinearityUniverse :: NormExpr -> Bool
isNLinearityUniverse (VUniverse _ LinearityUniv{}) = True
isNLinearityUniverse _                             = False

isNAuxiliaryUniverse :: NormExpr -> Bool
isNAuxiliaryUniverse e = isNPolarityUniverse e || isNLinearityUniverse e


isMeta :: NormExpr -> Bool
isMeta VMeta{} = True
isMeta _       = False

isBoolType :: NormExpr -> Bool
isBoolType (VConstructor _ Bool _) = True
isBoolType _                       = False

isIndexType :: NormExpr -> Bool
isIndexType (VConstructor _ Index _) = True
isIndexType _                        = False

isNatType :: NormExpr -> Bool
isNatType (VConstructor _ Nat _) = True
isNatType _                      = False

isIntType :: NormExpr -> Bool
isIntType (VConstructor _ Int _) = True
isIntType _                      = False

isRatType :: NormExpr -> Bool
isRatType (VConstructor _ Rat _) = True
isRatType _                      = False

isListType :: NormExpr -> Bool
isListType (VConstructor _ List _) = True
isListType _                       = False

isVectorType :: NormExpr -> Bool
isVectorType (VConstructor _ Vector _) = True
isVectorType _                         = False

isBoundVar :: NormExpr -> Bool
isBoundVar (VVar _ (Bound _) _) = True
isBoundVar _                    = False

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr = Glued
  { unnormalised :: CheckedExpr
  , normalised   :: NormExpr
  } deriving (Show)

type GluedType = GluedExpr

type GluedProg = GenericProg GluedExpr
type GluedDecl = GenericDecl GluedExpr

instance HasProvenance GluedExpr where
  provenanceOf = provenanceOf . unnormalised
