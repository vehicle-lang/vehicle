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
data Value types
  = VUniverse UniverseLevel
  | VLam (VBinder types) (Env types) (NormalisableExpr types)
  | VPi (VBinder types) (Value types)
  | VMeta MetaID (Spine types)
  | VFreeVar Identifier (Spine types)
  | VBoundVar Lv (Spine types)
  | VBuiltin (NormalisableBuiltin types) (ExplicitSpine types)
  deriving (Eq, Show, Generic)

instance (Serialize types) => Serialize (Value types)

type VArg types = GenericArg (Value types)

type VBinder types = GenericBinder (VType types)

type VDecl types = GenericDecl (Value types)

type VProg types = GenericDecl types

-- | A normalised type
type VType types = Value types

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine types = [VArg types]

-- | A spine type for builtins which enforces the invariant that they should
-- only ever depend computationally on their explicit arguments.
type ExplicitSpine types = [Value types]

type Env types = BoundCtx (Maybe Name, Value types)

extendEnv :: GenericBinder expr -> Value types -> Env types -> Env types
extendEnv binder value = ((nameOf binder, value) :)

extendEnvOverBinder :: GenericBinder expr -> Env types -> Env types
extendEnvOverBinder binder env =
  extendEnv binder (VBoundVar (Lv $ length env) []) env

-----------------------------------------------------------------------------
-- Patterns

pattern VTypeUniverse :: UniverseLevel -> VType types
pattern VTypeUniverse l = VUniverse l

pattern VBuiltinFunction :: BuiltinFunction -> ExplicitSpine types -> Value types
pattern VBuiltinFunction f spine = VBuiltin (CFunction f) spine

pattern VConstructor :: BuiltinConstructor -> ExplicitSpine types -> Value types
pattern VConstructor c args = VBuiltin (CConstructor c) args

pattern VNullaryConstructor :: BuiltinConstructor -> Value types
pattern VNullaryConstructor c <- VConstructor c []
  where
    VNullaryConstructor c = VConstructor c []

pattern VUnitLiteral :: Value types
pattern VUnitLiteral = VNullaryConstructor LUnit

pattern VBoolLiteral :: Bool -> Value types
pattern VBoolLiteral x = VNullaryConstructor (LBool x)

pattern VIndexLiteral :: Int -> Value types
pattern VIndexLiteral x = VNullaryConstructor (LIndex x)

pattern VNatLiteral :: Int -> Value types
pattern VNatLiteral x = VNullaryConstructor (LNat x)

pattern VIntLiteral :: Int -> Value types
pattern VIntLiteral x = VNullaryConstructor (LInt x)

pattern VRatLiteral :: Rational -> Value types
pattern VRatLiteral x = VNullaryConstructor (LRat x)

pattern VVecLiteral :: [Value types] -> Value types
pattern VVecLiteral xs <- VConstructor (LVec _) xs
  where
    VVecLiteral xs = VConstructor (LVec (length xs)) xs

pattern VNil :: Value types
pattern VNil = VNullaryConstructor Nil

pattern VCons :: [Value types] -> Value types
pattern VCons xs = VConstructor Cons xs

mkVList :: [Value types] -> Value types
mkVList = foldr cons nil
  where
    nil = VConstructor Nil []
    cons y ys = VConstructor Cons [y, ys]

mkVLVec :: [Value types] -> Value types
mkVLVec xs = VConstructor (LVec (length xs)) xs

isNTypeUniverse :: Value types -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: Value types -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

getNMeta :: Value types -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr types = Glued
  { unnormalised :: NormalisableExpr types,
    normalised :: Value types
  }
  deriving (Show, Generic)

instance (Serialize types) => Serialize (GluedExpr types)

instance HasProvenance (GluedExpr types) where
  provenanceOf = provenanceOf . unnormalised

type GluedArg types = GenericArg (GluedExpr types)

type GluedType types = GluedExpr types

type GluedProg types = GenericProg (GluedExpr types)

type GluedDecl types = GenericDecl (GluedExpr types)

traverseNormalised :: (Monad m) => (Value types -> m (Value types)) -> GluedExpr types -> m (GluedExpr types)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: (Monad m) => (NormalisableExpr types -> m (NormalisableExpr types)) -> GluedExpr types -> m (GluedExpr types)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
