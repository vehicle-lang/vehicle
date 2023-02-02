module Vehicle.Expr.Normalised where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude.Contexts (BoundCtx)
import Vehicle.Expr.DeBruijn
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
-- Patterns

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

isNMeta :: NormExpr builtin -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

getNMeta :: NormExpr builtin -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr builtin = Glued
  { unnormalised :: DBExpr builtin,
    normalised :: NormExpr builtin
  }
  deriving (Show, Generic)

instance Serialize builtin => Serialize (GluedExpr builtin)

instance HasProvenance (GluedExpr builtin) where
  provenanceOf = provenanceOf . unnormalised

type GluedArg builtin = GenericArg (GluedExpr builtin)

type GluedType builtin = GluedExpr builtin

type GluedProg builtin = GenericProg (GluedExpr builtin)

type GluedDecl builtin = GenericDecl (GluedExpr builtin)

traverseNormalised :: Monad m => (NormExpr builtin -> m (NormExpr builtin)) -> GluedExpr builtin -> m (GluedExpr builtin)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: Monad m => (DBExpr builtin -> m (DBExpr builtin)) -> GluedExpr builtin -> m (GluedExpr builtin)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
