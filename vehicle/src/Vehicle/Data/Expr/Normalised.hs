module Vehicle.Data.Expr.Normalised where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.DeBruijn
import Vehicle.Syntax.AST

-----------------------------------------------------------------------------
-- WHNF closures

-- | Closures for weak-head normal-form.
data WHNFClosure builtin = WHNFClosure (BoundEnv (WHNFClosure builtin) builtin) (Expr Ix builtin)
  deriving (Eq, Show, Generic)

-----------------------------------------------------------------------------
-- NF closures

newtype NFClosure builtin = NFClosure (Value (NFClosure builtin) builtin)
  deriving (Eq, Show, Generic)

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data Value closure builtin
  = VUniverse !UniverseLevel
  | VMeta !MetaID !(Spine closure builtin)
  | VFreeVar !Identifier !(Spine closure builtin)
  | VBoundVar !Lv !(Spine closure builtin)
  | VBuiltin !builtin !(Spine closure builtin)
  | VLam !(VBinder closure builtin) !closure
  | VPi !(VBinder closure builtin) !(Value closure builtin)
  deriving (Eq, Show, Generic)

type VArg closure builtin = GenericArg (Value closure builtin)

type VBinder closure builtin = GenericBinder (Value closure builtin)

type VDecl closure builtin = GenericDecl (Value closure builtin)

type VProg closure builtin = GenericProg (Value closure builtin)

-- | A list of arguments for an application that cannot be normalised.
type Spine closure builtin = [VArg closure builtin]

traverseSpine ::
  (Monad m) =>
  (Value strategy1 builtin1 -> m (Value strategy2 builtin2)) ->
  Spine strategy1 builtin1 ->
  m (Spine strategy2 builtin2)
traverseSpine f = traverse (traverse f)

-----------------------------------------------------------------------------
-- Bound environments

-- | Represents a variable's value in the environment.
data BoundEnvValue closure builtin
  = -- | The variable has no known concrete value.
    Bound
  | -- | The variable has a known value.
    Defined (Value closure builtin)
  deriving (Show, Eq, Generic)

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
type EnvEntry closure builtin = (GenericBinder (), BoundEnvValue closure builtin)

isBoundEntry :: EnvEntry closure builtin -> Bool
isBoundEntry (_binder, value) = case value of
  Bound {} -> True
  Defined {} -> False

type BoundEnv closure builtin = GenericBoundCtx (EnvEntry closure builtin)

emptyBoundEnv :: BoundEnv closure builtin
emptyBoundEnv = mempty

mkDefaultEnvEntry :: Name -> BoundEnvValue closure builtin -> EnvEntry closure builtin
mkDefaultEnvEntry name value = (Binder mempty displayForm Explicit Relevant (), value)
  where
    displayForm = BinderDisplayForm (OnlyName name) True

-- | Note that the `ctxSize` must come from the current context and not a
-- bound environment as the environment that the term was originally normalised
-- in may not be the same size as the current context.
extendEnvWithBound ::
  Lv ->
  GenericBinder expr ->
  BoundEnv closure builtin ->
  BoundEnv closure builtin
extendEnvWithBound ctxSize = extendEnvWithDefined (VBoundVar ctxSize [])

extendEnvWithDefined ::
  Value closure builtin ->
  GenericBinder expr ->
  BoundEnv closure builtin ->
  BoundEnv closure builtin
extendEnvWithDefined value binder env = (void binder, Defined value) : env

boundContextToEnv ::
  BoundCtx builtin ->
  BoundEnv closure builtin
boundContextToEnv = fmap (\binder -> (void binder, Bound))

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv closure builtin -> GenericBoundCtx (Value closure builtin)
cheatEnvToValues = fmap envEntryToValue
  where
    envEntryToValue :: EnvEntry closure builtin -> Value closure builtin
    envEntryToValue (binder, value) = do
      let name = VFreeVar (Identifier StdLib (fromMaybe "_" (nameOf binder) <> " ="))
      name
        [ Arg mempty Explicit Relevant $ case value of
            Bound -> VFreeVar (Identifier StdLib "_") mempty
            Defined x -> x
        ]

-----------------------------------------------------------------------------
-- WHNF

type WHNFBoundEnvValue builtin = BoundEnvValue (WHNFClosure builtin) builtin

type WHNFValue builtin = Value (WHNFClosure builtin) builtin

type WHNFType builtin = WHNFValue builtin

type WHNFArg builtin = VArg (WHNFClosure builtin) builtin

type WHNFBinder builtin = VBinder (WHNFClosure builtin) builtin

type WHNFSpine builtin = Spine (WHNFClosure builtin) builtin

type WHNFDecl builtin = GenericDecl (WHNFValue builtin)

type WHNFBoundEnv builtin = BoundEnv (WHNFClosure builtin) builtin

-----------------------------------------------------------------------------
-- NF

type NFValue builtin = Value (NFClosure builtin) builtin

type NFEnv builtin = BoundEnv (NFClosure builtin) builtin

type NFType builtin = NFValue builtin

type NFArg builtin = VArg (NFClosure builtin) builtin

type NFBinder builtin = VBinder (NFClosure builtin) builtin

type NFSpine builtin = Spine (NFClosure builtin) builtin

-----------------------------------------------------------------------------
-- Patterns

isNTypeUniverse :: Value closure builtin -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: Value closure builtin -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

isVBoundVar :: Value closure builtin -> Bool
isVBoundVar = \case
  VBoundVar {} -> True
  _ -> False

getNMeta :: Value closure builtin -> Maybe MetaID
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
