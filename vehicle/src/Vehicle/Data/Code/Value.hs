module Vehicle.Data.Code.Value
  ( Closure (..),
    Value (..),
    VType,
    VArg,
    VBinder,
    VTelescope,
    VRecordFields,
    VDecl,
    VModule,
    VProg,
    VDims,
    Spine,
    getNMeta,
    BoundEnv (..),
    lookupIxInEnv,
    extendEnvWithBound,
    extendEnvWithDefined,
    boundContextToEnv,
    namedBoundContextToEnv,
    cheatEnvToValues,
    boundEnvToCtx,
    traverseEnv,
    traverseEnv_,
    relabelLvInValue,
    FreeEnv,
    emptyBoundEnv,
    GluedExpr (..),
    GluedType,
    DimensionedTensorValue (..),
  )
where

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map.Ordered (OMap)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Vehicle.Data.AST.Expr.Scoped (Expr)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Data.Variable.Bound.Context.Core
import Vehicle.Data.Variable.Bound.Context.Generic.Core
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Index (Ix)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

-----------------------------------------------------------------------------
-- WHNF closures

-- | Closures for weak-head normal-form.
--
-- * 'ExprClosure': body is source 'Expr' with free Ix refs into the captured 'BoundEnv'.
-- * 'ValueClosure': body is a normalised 'Value' with the binder as a free 'VBoundVar'
--   at the given absolute 'Lv'; outer free vars use their actual NBE Lvs.
data Closure builtin
  = ExprClosure (BoundEnv builtin) (Expr builtin)
  | ValueClosure !Lv (Value builtin)
  deriving (Show, Generic, Eq, Ord)

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
  | VRecord (VType builtin) !(VRecordFields builtin)
  | VRecordAcc !(VType builtin) !(Value builtin) !FieldName !(Spine builtin)
  deriving (Show, Generic, Eq, Ord)

type VType builtin = Value builtin

type VArg builtin = GenericArg (Value builtin)

type VBinder builtin = GenericBinder (Value builtin)

type VTelescope builtin = GenericTelescope (Value builtin)

type VRecordFields builtin = OMap FieldName (Value builtin)

type VDecl builtin = GenericDecl (Value builtin)

type VModule builtin = GenericModule (Value builtin)

type VProg builtin = GenericModule (Value builtin)

type VDims builtin = Value builtin

-- | A list of arguments for an application that cannot be normalised.
type Spine builtin = [VArg builtin]

----------------------------------------------------------------------------
-- Bound environments

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
type EnvEntry builtin = Value builtin

unbound :: Lv -> EnvEntry builtin
unbound lv = VBoundVar lv []

newtype BoundEnv builtin = BoundEnv
  { unBoundEnv :: GenericBoundCtx (GenericBinder (), EnvEntry builtin)
  }
  deriving (Show, Eq, Ord)

emptyBoundEnv :: BoundEnv builtin
emptyBoundEnv = BoundEnv mempty

lookupIxInEnv :: BoundEnv builtin -> Ix -> Value builtin
lookupIxInEnv (BoundEnv env) i = snd $ lookupIxInBoundCtx i env

-- | Note that the `ctxSize` must come from the current context and not a
-- bound environment as the environment that the term was originally normalised
-- in may not be the same size as the current context.
extendEnvWithBound ::
  Lv ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithBound ctxSize binder (BoundEnv env) =
  BoundEnv $ (void binder, unbound ctxSize) : env

extendEnvWithDefined ::
  Value builtin ->
  GenericBinder expr ->
  BoundEnv builtin ->
  BoundEnv builtin
extendEnvWithDefined value binder (BoundEnv env) =
  BoundEnv $ (void binder, value) : env

boundContextToEnv :: BoundCtx expr -> BoundEnv builtin
boundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap void unbound) numberedCtx

namedBoundContextToEnv :: NamedBoundCtx -> BoundEnv builtin
namedBoundContextToEnv ctx = BoundEnv $ do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (bimap (\n -> mkExplicitBinder () (fmap (mempty,) n)) unbound) numberedCtx

boundEnvToCtx :: BoundEnv builtin -> NamedBoundCtx
boundEnvToCtx (BoundEnv env) = toNamedBoundCtx (fmap fst env)

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv builtin -> GenericBoundCtx (Value builtin)
cheatEnvToValues (BoundEnv env) = fmap entryToValue env
  where
    entryToValue :: (GenericBinder (), EnvEntry builtin) -> Value builtin
    entryToValue (binder, value) = do
      let ident = stdlibIdentifier (fromMaybe "_" (nameOf binder) <> " =")
      let arg = explicit value
      VFreeVar ident [arg]

----------------------------------------------------------------------------
-- Free environments

type FreeEnv builtin = Map Identifier (VDecl builtin)

traverseEnv_ :: (Monad m) => (Value builtin -> m ()) -> BoundEnv builtin -> m ()
traverseEnv_ f (BoundEnv env) = traverse_ (\(_, v) -> f v) env

traverseEnv :: (Monad m) => (Value builtin -> m (Value builtin)) -> BoundEnv builtin -> m (BoundEnv builtin)
traverseEnv f (BoundEnv env) = BoundEnv <$> traverse (\(u, v) -> (u,) <$> f v) env

-- | Rename every `VBoundVar srcLv` to `VBoundVar dstLv` inside a 'Value',
-- skipping under inner closures that shadow `srcLv`. Pure because VBoundVar
-- substitution can't unblock builtin reductions.
relabelLvInValue :: forall builtin. Lv -> Lv -> Value builtin -> Value builtin
relabelLvInValue srcLv dstLv = go
  where
    go :: Value builtin -> Value builtin
    go = \case
      VBoundVar lv spine
        | lv == srcLv -> VBoundVar dstLv (map (fmap go) spine)
        | otherwise -> VBoundVar lv (map (fmap go) spine)
      VBuiltin b spine -> VBuiltin b (map (fmap go) spine)
      VFreeVar n spine -> VFreeVar n (map (fmap go) spine)
      VMeta m spine -> VMeta m (map (fmap go) spine)
      VRecord typ fields -> VRecord (go typ) (fmap go fields)
      VRecordAcc typ rec field spine ->
        VRecordAcc (go typ) (go rec) field (map (fmap go) spine)
      VLam binder closure -> VLam (fmap go binder) (relabelClosure closure)
      VPi binder closure -> VPi (fmap go binder) (relabelClosure closure)
      VUniverse u -> VUniverse u

    relabelClosure :: Closure builtin -> Closure builtin
    relabelClosure = \case
      ExprClosure (BoundEnv env) body ->
        ExprClosure (BoundEnv (map (fmap go) env)) body
      ValueClosure innerLv innerBody
        | innerLv == srcLv -> ValueClosure innerLv innerBody
        | otherwise -> ValueClosure innerLv (go innerBody)

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
-- Dimensioned values

-- | Because there are no dependent types in Haskell, we cannot create
-- type-classes over tensor values with a given dimension. Hence we need
-- to wrap them in this ugly type-class that stores the dimensions internally.
data DimensionedTensorValue builtin = TensorValue
  { tensorValueDims :: VDims builtin,
    tensorValue :: Value builtin
  }
  deriving (Show, Eq, Ord)

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

instance HasLambdaConstructor Value Closure where
  accessLamC =
    Access
      { getExpr = \case
          VLam binder closure -> Just (binder, closure)
          _ -> Nothing,
        mkExpr = uncurry VLam
      }
