module Vehicle.Data.Code.Value where

import Control.Monad (void)
import Data.Map (Map)
import Data.Map.Ordered (OMap)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Expr (Expr)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DeBruijn
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Prelude

-----------------------------------------------------------------------------
-- WHNF closures

-- | Closures for weak-head normal-form.
data Closure builtin = Closure (BoundEnv builtin) (Expr builtin)
  deriving (Show, Generic)

-----------------------------------------------------------------------------
-- Normalised expressions

-- | A normalised expression. Internal invariant is that it should always be
-- well-typed.
data Value builtin
  = VUniverse !UniverseLevel
  | VMeta !MetaID !(Spine builtin)
  | VFreeVar !Identifier !(Spine builtin)
  | VBoundVar !Lv !(Spine builtin)
  | VBuiltin !builtin !(Spine builtin)
  | VLam !(VBinder builtin) !(Closure builtin)
  | VPi !(VBinder builtin) !(Closure builtin)
  | VRecord Identifier !(OMap FieldName (Value builtin))
  | VRecordAcc !(Value builtin) !(Identifier, FieldName)
  deriving (Show, Generic)

type VType builtin = Value builtin

type VArg builtin = GenericArg (Value builtin)

type VBinder builtin = GenericBinder (Value builtin)

type VDecl builtin = GenericDecl (Value builtin)

type VProg builtin = GenericProg (Value builtin)

-- | A list of arguments for an application that cannot be normalised.
type Spine builtin = [VArg builtin]

traverseSpine :: (Monad m) => (Value builtin1 -> m (Value builtin2)) -> Spine builtin1 -> m (Spine builtin2)
traverseSpine f = traverse (traverse f)

-----------------------------------------------------------------------------
-- Bound environments

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
type EnvEntry builtin = (GenericBinder (), Value builtin)

type BoundEnv builtin = GenericBoundCtx (EnvEntry builtin)

emptyBoundEnv :: BoundEnv builtin
emptyBoundEnv = mempty

-- | Note that the `ctxSize` must come from the current context and not a
-- bound environment as the environment that the term was originally normalised
-- in may not be the same size as the current context.
extendEnvWithBound ::
  Lv ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithBound ctxSize = extendEnvWithDefined (VBoundVar ctxSize [])

extendEnvWithDefined ::
  Value builtin ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithDefined value binder env = (void binder, value) : env

boundContextToEnv :: BoundCtx expr -> BoundEnv builtin
boundContextToEnv ctx = do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (\(binder, lv) -> (void binder, VBoundVar lv [])) numberedCtx

boundEnvToCtx :: BoundEnv builtin -> NamedBoundCtx
boundEnvToCtx env = toNamedBoundCtx (fmap fst env)

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv builtin -> GenericBoundCtx (Value builtin)
cheatEnvToValues = fmap envEntryToValue
  where
    envEntryToValue :: EnvEntry builtin -> Value builtin
    envEntryToValue (binder, value) = do
      let ident = stdlibIdentifier (fromMaybe "_" (nameOf binder) <> " =")
      VFreeVar ident [explicit value]

type FreeEnv builtin = Map Identifier (VDecl builtin)

-----------------------------------------------------------------------------
-- Patterns

isNTypeUniverse :: Value builtin -> Bool
isNTypeUniverse VUniverse {} = True
isNTypeUniverse _ = False

isNMeta :: Value builtin -> Bool
isNMeta VMeta {} = True
isNMeta _ = False

isVBoundVar :: Value builtin -> Bool
isVBoundVar = \case
  VBoundVar {} -> True
  _ -> False

getNMeta :: Value builtin -> Maybe MetaID
getNMeta (VMeta m _) = Just m
getNMeta _ = Nothing

-----------------------------------------------------------------------------
-- Glued expressions

-- | A pair of an unnormalised and normalised expression.
data GluedExpr builtin = Glued
  { unnormalised :: Expr builtin,
    normalised :: Value builtin
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
  (Value builtin -> m (Value builtin)) ->
  GluedExpr builtin ->
  m (GluedExpr builtin)
traverseNormalised f (Glued u n) = Glued u <$> f n

traverseUnnormalised ::
  (Monad m) =>
  (Expr builtin -> m (Expr builtin)) ->
  GluedExpr builtin ->
  m (GluedExpr builtin)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n

-----------------------------------------------------------------------------
-- Instances

instance (HasBuiltinConstructor Value) where
  accessBuiltinC =
    Access
      { getExpr = \case
          VBuiltin b spine -> Just (b, spine)
          _ -> Nothing,
        mkExpr = uncurry VBuiltin
      }
