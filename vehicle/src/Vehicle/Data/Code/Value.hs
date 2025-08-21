module Vehicle.Data.Code.Value
  ( Closure (..),
    Value (..),
    VType,
    VArg,
    VBinder,
    VDecl,
    VProg,
    Spine,
    traverseSpine,
    getNMeta,
    BoundEnv (..),
    EnvEntry (..),
    lookupIxInEnv,
    extendEnvWithBound,
    extendEnvWithDefined,
    boundContextToEnv,
    namedBoundContextToEnv,
    cheatEnvToValues,
    boundEnvToCtx,
    traverseEnv,
    traverseEnv_,
    finalCtxSize,
    FreeEnv,
    emptyBoundEnv,
    GluedExpr (..),
    GluedType,
    envEntryToValue,
  )
where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
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
data EnvEntry builtin
  = Bound (Value builtin)
  | Unbound Lv
  deriving (Show)

envEntryToValue :: EnvEntry builtin -> Value builtin
envEntryToValue = \case
  Bound value -> value
  Unbound lv -> VBoundVar lv []

traverseEnvEntry_ :: (Monad m) => (Value builtin -> m ()) -> EnvEntry builtin -> m ()
traverseEnvEntry_ f = \case
  Bound v -> f v
  Unbound {} -> return ()

traverseEnvEntry :: (Monad m) => (Value builtin -> m (Value builtin)) -> EnvEntry builtin -> m (EnvEntry builtin)
traverseEnvEntry f = \case
  Bound v -> Bound <$> f v
  Unbound lv -> return $ Unbound lv

isUnbound :: EnvEntry builtin -> Bool
isUnbound = \case
  Unbound {} -> True
  _ -> False

newtype BoundEnv builtin = BoundEnv
  { unBoundEnv :: GenericBoundCtx (GenericBinder (), EnvEntry builtin)
  }
  deriving (Show)

emptyBoundEnv :: BoundEnv builtin
emptyBoundEnv = BoundEnv mempty

lookupIxInEnv :: BoundEnv builtin -> Ix -> Value builtin
lookupIxInEnv (BoundEnv env) i = envEntryToValue $ snd $ lookupIxInBoundCtx i env

-- | Note that the `ctxSize` must come from the current context and not a
-- bound environment as the environment that the term was originally normalised
-- in may not be the same size as the current context.
extendEnvWithBound ::
  Lv ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithBound ctxSize binder (BoundEnv env) =
  BoundEnv $ (void binder, Unbound ctxSize) : env

extendEnvWithDefined ::
  Value builtin ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithDefined value binder (BoundEnv env) =
  BoundEnv $ (void binder, Bound value) : env

boundContextToEnv :: BoundCtx expr -> BoundEnv builtin
boundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap void Unbound) numberedCtx

namedBoundContextToEnv :: NamedBoundCtx -> BoundEnv builtin
namedBoundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap (mkExplicitBinder ()) Unbound) numberedCtx

boundEnvToCtx :: BoundEnv builtin -> NamedBoundCtx
boundEnvToCtx (BoundEnv env) = toNamedBoundCtx (fmap fst env)

finalCtxSize :: BoundEnv builtin -> Lv
finalCtxSize (BoundEnv env) = Lv $ length $ filter (\(_, v) -> isUnbound v) env

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv builtin -> GenericBoundCtx (Value builtin)
cheatEnvToValues (BoundEnv env) = fmap entryToValue env
  where
    entryToValue :: (GenericBinder (), EnvEntry builtin) -> Value builtin
    entryToValue (binder, value) = do
      let ident = stdlibIdentifier (fromMaybe "_" (nameOf binder) <> " =")
      let arg = explicit $ envEntryToValue value
      VFreeVar ident [arg]

type FreeEnv builtin = Map Identifier (VDecl builtin)

traverseEnv_ :: (Monad m) => (Value builtin -> m ()) -> BoundEnv builtin -> m ()
traverseEnv_ f (BoundEnv env) = traverse_ (\(_, v) -> traverseEnvEntry_ f v) env

traverseEnv :: (Monad m) => (Value builtin -> m (Value builtin)) -> BoundEnv builtin -> m (BoundEnv builtin)
traverseEnv f (BoundEnv env) = BoundEnv <$> traverse (\(u, v) -> (u,) <$> traverseEnvEntry f v) env

-----------------------------------------------------------------------------
-- Patterns

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

type GluedType builtin = GluedExpr builtin

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
