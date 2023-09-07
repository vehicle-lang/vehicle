{-# LANGUAGE StandaloneDeriving #-}

module Vehicle.Data.NormalisedExpr where

import Data.Kind qualified as Kind (Type)
import GHC.Generics
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.DeBruijn
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

data NFStrategy = NF | WHNF

data Body :: NFStrategy -> Kind.Type -> Kind.Type where
  NFBody :: Value 'NF builtin -> Body 'NF builtin
  WHNFBody :: Env 'WHNF builtin -> Expr Ix builtin -> Body 'WHNF builtin

deriving instance (Eq builtin) => Eq (Body stategy builtin)

deriving instance (Show builtin) => Show (Body stategy builtin)

-- Wish we didn't have to define this manually but the deriving doesn't
-- seem to work with GADTs.
instance (Generic builtin) => Generic (Body 'WHNF builtin) where
  type
    Rep (Body 'WHNF builtin) =
      Rep (Env 'WHNF builtin)
        :*: Rep (Expr Ix builtin)

  to (u :*: v) = WHNFBody (to u) (to v)
  from (WHNFBody env expr) = from env :*: from expr

-- instance Eq (Body 'NF builtin) where

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data Value (strategy :: NFStrategy) builtin
  = VUniverse UniverseLevel
  | VMeta MetaID (Spine strategy builtin)
  | VFreeVar Identifier (Spine strategy builtin)
  | VBoundVar Lv (Spine strategy builtin)
  | VBuiltin builtin (Spine strategy builtin)
  | VLam (VBinder strategy builtin) (Body strategy builtin)
  | VPi (VBinder strategy builtin) (Value strategy builtin)
  deriving (Eq, Show, Generic)

type VArg strategy builtin = GenericArg (Value strategy builtin)

type VBinder strategy builtin = GenericBinder (Value strategy builtin)

-- | A list of arguments for an application that cannot be normalised.
type Spine strategy builtin = [VArg strategy builtin]

-----------------------------------------------------------------------------
-- Environments

type Env strategy builtin = GenericBoundCtx (VBinder strategy builtin)

extendEnv ::
  VBinder strategy builtin ->
  Value strategy builtin ->
  Env strategy builtin ->
  Env strategy builtin
extendEnv binder value = (fmap (const value) binder :)

extendEnvOverBinder ::
  VBinder strategy builtin ->
  Env strategy builtin ->
  Env strategy builtin
extendEnvOverBinder binder env =
  extendEnv binder (VBoundVar (Lv $ length env) []) env

boundContextToEnv ::
  BoundCtx builtin ->
  Env strategy builtin
boundContextToEnv ctx = do
  let levels = reverse (fmap Lv [0 .. length ctx - 1])
  zipWith (\level binder -> fmap (const $ VBoundVar level []) binder) levels ctx

-----------------------------------------------------------------------------
-- WHNF

type WHNFValue builtin = Value 'WHNF builtin

type WHNFEnv builtin = Env 'WHNF builtin

type WHNFType builtin = WHNFValue builtin

type WHNFArg builtin = VArg 'WHNF builtin

type WHNFBinder builtin = VBinder 'WHNF builtin

type WHNFSpine builtin = Spine 'WHNF builtin

-----------------------------------------------------------------------------
-- NF

type NFValue builtin = Value 'NF builtin

type NFEnv builtin = Env 'NF builtin

type NFType builtin = NFValue builtin

type NFArg builtin = VArg 'NF builtin

type NFBinder builtin = VBinder 'NF builtin

type NFSpine builtin = Spine 'NF builtin

-----------------------------------------------------------------------------
-- Patterns

isNTypeUniverse :: Value strategy builtin -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: Value strategy builtin -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

getNMeta :: Value strategy builtin -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr builtin = Glued
  { unnormalised :: Expr Ix builtin,
    normalised :: WHNFValue builtin
  }
  deriving (Show, Generic)

instance HasProvenance (GluedExpr builtin) where
  provenanceOf = provenanceOf . unnormalised

type GluedArg builtin = GenericArg (GluedExpr builtin)

type GluedType builtin = GluedExpr builtin

type GluedProg builtin = GenericProg (GluedExpr builtin)

type GluedDecl builtin = GenericDecl (GluedExpr builtin)

traverseNormalised ::
  (Monad m) =>
  (WHNFValue builtin -> m (WHNFValue builtin)) ->
  GluedExpr builtin ->
  m (GluedExpr builtin)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised ::
  (Monad m) =>
  (Expr Ix builtin -> m (Expr Ix builtin)) ->
  GluedExpr builtin ->
  m (GluedExpr builtin)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n
