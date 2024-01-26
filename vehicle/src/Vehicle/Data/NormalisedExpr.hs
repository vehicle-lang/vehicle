{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Data.NormalisedExpr where

import Control.Monad (void)
import Data.Kind qualified as Kind (Type)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.DeBruijn
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- Normalised expressions

data NFStrategy = NF | WHNF

data Body :: NFStrategy -> Kind.Type -> Kind.Type where
  NFBody :: Value 'NF builtin -> Body 'NF builtin
  WHNFBody :: BoundEnv 'WHNF builtin -> Expr Ix builtin -> Body 'WHNF builtin

deriving instance (Eq builtin) => Eq (Body stategy builtin)

deriving instance (Show builtin) => Show (Body stategy builtin)

-- Wish we didn't have to define this manually but the deriving doesn't
-- seem to work with GADTs.
instance (Generic builtin) => Generic (Body 'WHNF builtin) where
  type
    Rep (Body 'WHNF builtin) =
      Rep (BoundEnv 'WHNF builtin)
        :*: Rep (Expr Ix builtin)

  to (u :*: v) = WHNFBody (to u) (to v)
  from (WHNFBody env expr) = from env :*: from expr

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
-- Bound environments

-- | Represents a variable's value in the environment.
data BoundEnvValue strategy builtin
  = -- | The variable has no known concrete value.
    Bound
  | -- | The variable has a known value.
    Defined (Value strategy builtin)
  deriving (Show, Eq, Generic)

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
type EnvEntry strategy builtin = (GenericBinder (), BoundEnvValue strategy builtin)

isBoundEntry :: EnvEntry strategy builtin -> Bool
isBoundEntry (_binder, value) = case value of
  Bound {} -> True
  Defined {} -> False

type BoundEnv strategy builtin = GenericBoundCtx (EnvEntry strategy builtin)

emptyBoundEnv :: BoundEnv strategy builtin
emptyBoundEnv = mempty

mkDefaultEnvEntry :: Name -> BoundEnvValue strategy builtin -> EnvEntry strategy builtin
mkDefaultEnvEntry name value = (Binder mempty displayForm Explicit Relevant (), value)
  where
    displayForm = BinderDisplayForm (OnlyName name) True

extendEnvWithBound ::
  GenericBinder expr ->
  BoundEnv strategy builtin ->
  BoundEnv strategy builtin
extendEnvWithBound binder env = (void binder, Bound) : env

extendEnvWithDefined ::
  Value strategy builtin ->
  GenericBinder expr ->
  BoundEnv strategy builtin ->
  BoundEnv strategy builtin
extendEnvWithDefined value binder env = (void binder, Defined value) : env

boundContextToEnv ::
  BoundCtx builtin ->
  BoundEnv strategy builtin
boundContextToEnv = fmap (\binder -> (void binder, Bound))

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv strategy builtin -> GenericBoundCtx (Value strategy builtin)
cheatEnvToValues = fmap envEntryToValue
  where
    envEntryToValue :: EnvEntry strategy builtin -> Value strategy builtin
    envEntryToValue (binder, value) = do
      let name = VFreeVar (Identifier StdLib (fromMaybe "_" (nameOf binder) <> " ="))
      name
        [ Arg mempty Explicit Relevant $ case value of
            Bound -> VFreeVar (Identifier StdLib "_") mempty
            Defined x -> x
        ]

-----------------------------------------------------------------------------
-- WHNF

type WHNFValue builtin = Value 'WHNF builtin

type WHNFType builtin = WHNFValue builtin

type WHNFArg builtin = VArg 'WHNF builtin

type WHNFBinder builtin = VBinder 'WHNF builtin

type WHNFSpine builtin = Spine 'WHNF builtin

type WHNFDecl builtin = GenericDecl (WHNFValue builtin)

type WHNFBoundEnv builtin = BoundEnv 'WHNF builtin

-----------------------------------------------------------------------------
-- NF

type NFValue builtin = Value 'NF builtin

type NFEnv builtin = BoundEnv 'NF builtin

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
