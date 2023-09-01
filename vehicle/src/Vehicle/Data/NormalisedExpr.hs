module Vehicle.Data.NormalisedExpr where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.DeBruijn
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data Value builtin
  = VUniverse UniverseLevel
  | VLam (VBinder builtin) (Env builtin) (Expr Ix builtin)
  | VPi (VBinder builtin) (Value builtin)
  | VMeta MetaID (Spine builtin)
  | VFreeVar Identifier (Spine builtin)
  | VBoundVar Lv (Spine builtin)
  | VBuiltin builtin (Spine builtin)
  deriving (Eq, Show, Generic)

instance (Serialize builtin) => Serialize (Value builtin)

type VArg builtin = GenericArg (Value builtin)

type VBinder builtin = GenericBinder (VType builtin)

type VDecl builtin = GenericDecl (Value builtin)

type VProg builtin = GenericDecl builtin

-- | A normalised type
type VType builtin = Value builtin

-----------------------------------------------------------------------------
-- Spines and environments

-- | A list of arguments for an application that cannot be normalised.
type Spine builtin = [VArg builtin]

type Env builtin = GenericBoundCtx (GenericBinder (Value builtin))

extendEnv :: VBinder builtin -> Value builtin -> Env builtin -> Env builtin
extendEnv binder value = (fmap (const value) binder :)

extendEnvOverBinder :: VBinder builtin -> Env builtin -> Env builtin
extendEnvOverBinder binder env =
  extendEnv binder (VBoundVar (Lv $ length env) []) env

boundContextToEnv :: BoundCtx builtin -> Env builtin
boundContextToEnv ctx = do
  let levels = reverse (fmap Lv [0 .. length ctx - 1])
  zipWith (\level binder -> fmap (const $ VBoundVar level []) binder) levels ctx

-----------------------------------------------------------------------------
-- Patterns

isNTypeUniverse :: Value builtin -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: Value builtin -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

getNMeta :: Value builtin -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr builtin = Glued
  { unnormalised :: Expr Ix builtin,
    normalised :: Value builtin
  }
  deriving (Show, Generic)

instance (Serialize builtin) => Serialize (GluedExpr builtin)

instance HasProvenance (GluedExpr builtin) where
  provenanceOf = provenanceOf . unnormalised

type GluedArg builtin = GenericArg (GluedExpr builtin)

type GluedType builtin = GluedExpr builtin

type GluedProg builtin = GenericProg (GluedExpr builtin)

type GluedDecl builtin = GenericDecl (GluedExpr builtin)

traverseNormalised :: (Monad m) => (Value builtin -> m (Value builtin)) -> GluedExpr builtin -> m (GluedExpr builtin)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised :: (Monad m) => (Expr Ix builtin -> m (Expr Ix builtin)) -> GluedExpr builtin -> m (GluedExpr builtin)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
