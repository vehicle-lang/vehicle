module Vehicle.Expr.Normalised where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Compile.Prelude.Contexts (BoundCtx)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data NormExpr types
  = VUniverse UniverseLevel
  | VLam (NormBinder types) (Env types) (NormalisableExpr types)
  | VPi (NormBinder types) (NormExpr types)
  | VMeta MetaID (Spine types)
  | VFreeVar Identifier (Spine types)
  | VBoundVar Lv (Spine types)
  | VBuiltin (NormalisableBuiltin types) (ExplicitSpine types)
  deriving (Eq, Show, Generic)

instance (Serialize types) => Serialize (NormExpr types)

type NormArg types = GenericArg (NormExpr types)

type NormBinder types = GenericBinder () (NormType types)

type NormDecl types = GenericDecl (NormExpr types)

type NormProg types = GenericDecl types

-- | A normalised type
type NormType types = NormExpr types

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine types = [NormArg types]

-- | A spine type for builtins which enforces the invariant that they should
-- only ever depend computationally on their explicit arguments.
type ExplicitSpine types = [NormExpr types]

type Env types = BoundCtx (Maybe Name, NormExpr types)

extendEnv :: GenericBinder binder expr -> NormExpr types -> Env types -> Env types
extendEnv binder value = ((nameOf binder, value) :)

extendEnvOverBinder :: GenericBinder binder expr -> Env types -> Env types
extendEnvOverBinder binder env =
  extendEnv binder (VBoundVar (Lv $ length env) []) env

-----------------------------------------------------------------------------
-- Patterns

pattern VTypeUniverse :: UniverseLevel -> NormType types
pattern VTypeUniverse l = VUniverse l

pattern VBuiltinFunction :: BuiltinFunction -> ExplicitSpine types -> NormExpr types
pattern VBuiltinFunction f spine = VBuiltin (CFunction f) spine

pattern VConstructor :: BuiltinConstructor -> ExplicitSpine types -> NormExpr types
pattern VConstructor c args = VBuiltin (CConstructor c) args

pattern VNullaryConstructor :: BuiltinConstructor -> NormExpr types
pattern VNullaryConstructor c <- VConstructor c []
  where
    VNullaryConstructor c = VConstructor c []

pattern VUnitLiteral :: NormExpr types
pattern VUnitLiteral = VNullaryConstructor LUnit

pattern VBoolLiteral :: Bool -> NormExpr types
pattern VBoolLiteral x = VNullaryConstructor (LBool x)

pattern VIndexLiteral :: Int -> NormExpr types
pattern VIndexLiteral x = VNullaryConstructor (LIndex x)

pattern VNatLiteral :: Int -> NormExpr types
pattern VNatLiteral x = VNullaryConstructor (LNat x)

pattern VIntLiteral :: Int -> NormExpr types
pattern VIntLiteral x = VNullaryConstructor (LInt x)

pattern VRatLiteral :: Rational -> NormExpr types
pattern VRatLiteral x = VNullaryConstructor (LRat x)

pattern VVecLiteral :: [NormExpr types] -> NormExpr types
pattern VVecLiteral xs <- VConstructor (LVec _) xs
  where
    VVecLiteral xs = VConstructor (LVec (length xs)) xs

pattern VNil :: NormExpr types
pattern VNil = VNullaryConstructor Nil

pattern VCons :: [NormExpr types] -> NormExpr types
pattern VCons xs = VConstructor Cons xs

mkVList :: [NormExpr types] -> NormExpr types
mkVList = foldr cons nil
  where
    nil = VConstructor Nil []
    cons y ys = VConstructor Cons [y, ys]

mkVLVec :: [NormExpr types] -> NormExpr types
mkVLVec xs = VConstructor (LVec (length xs)) xs

isNTypeUniverse :: NormExpr types -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: NormExpr types -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

getNMeta :: NormExpr types -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr types = Glued
  { unnormalised :: NormalisableExpr types,
    normalised :: NormExpr types
  }
  deriving (Show, Generic)

instance (Serialize types) => Serialize (GluedExpr types)

instance HasProvenance (GluedExpr types) where
  provenanceOf = provenanceOf . unnormalised

type GluedArg types = GenericArg (GluedExpr types)

type GluedType types = GluedExpr types

type GluedProg types = GenericProg (GluedExpr types)

type GluedDecl types = GenericDecl (GluedExpr types)

traverseNormalised :: (Monad m) => (NormExpr types -> m (NormExpr types)) -> GluedExpr types -> m (GluedExpr types)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: (Monad m) => (NormalisableExpr types -> m (NormalisableExpr types)) -> GluedExpr types -> m (GluedExpr types)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
