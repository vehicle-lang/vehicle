module Vehicle.Data.Code.Value where

import Control.Monad (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard (BuiltinFunction)
import Vehicle.Data.Code.Expr (Expr)
import Vehicle.Data.Code.Interface
import Vehicle.Data.DeBruijn
import Vehicle.Data.Universe (UniverseLevel)
import Vehicle.Prelude

-----------------------------------------------------------------------------
-- WHNF closures

-- | Closures for weak-head normal-form.
data Closure builtin = Closure (BoundEnv builtin) (Expr builtin)
  deriving (Eq, Show, Generic)

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
  | VPi !(VBinder builtin) !(Value builtin)
  deriving (Eq, Show, Generic)

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

instance (BuiltinHasStandardTypes builtin) => HasStandardTypes (Value builtin) where
  mkType _p b = VBuiltin (mkBuiltinType b)
  getType e = case e of
    VBuiltin b args -> case getBuiltinType b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasStandardData builtin) => HasStandardData (Value builtin) where
  mkFunction _p b = VBuiltin (mkBuiltinFunction b)
  getFunction e = case e of
    VBuiltin b args -> case getBuiltinFunction b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

  mkConstructor _p b = VBuiltin (mkBuiltinConstructor b)
  getConstructor e = case e of
    VBuiltin b args -> case getBuiltinConstructor b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

  mkFreeVar _p = VFreeVar
  getFreeVar = \case
    VFreeVar ident args -> Just (mempty, ident, args)
    _ -> Nothing

  getTypeClassOp e = case e of
    VBuiltin b args -> case getBuiltinTypeClassOp b of
      Just op -> Just (mempty, op, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasBoolLiterals builtin) => HasBoolLits (Value builtin) where
  getBoolLit = \case
    VBuiltin (getBoolBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkBoolLit _p b = VBuiltin (mkBoolBuiltinLit b) []

instance (BuiltinHasIndexLiterals builtin) => HasIndexLits (Value builtin) where
  getIndexLit e = case e of
    VBuiltin (getIndexBuiltinLit -> Just n) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit _p x = VBuiltin (mkIndexBuiltinLit x) mempty

instance (BuiltinHasNatLiterals builtin) => HasNatLits (Value builtin) where
  getNatLit e = case e of
    VBuiltin (getNatBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkNatLit _p x = VBuiltin (mkNatBuiltinLit x) mempty

instance (BuiltinHasRatLiterals builtin) => HasRatLits (Value builtin) where
  getRatLit e = case e of
    VBuiltin (getRatBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkRatLit _p x = VBuiltin (mkRatBuiltinLit x) mempty

instance (BuiltinHasRatType builtin) => HasRatType (Value builtin) where
  getRatType e = case e of
    VBuiltin (isRatBuiltinType -> True) [] -> Just mempty
    _ -> Nothing
  mkRatType _p = VBuiltin mkRatBuiltinType []

instance (BuiltinHasVecLiterals builtin) => HasStandardVecLits (Value builtin) where
  getHomoVector = \case
    VBuiltin (getVecBuiltinLit -> Just {}) (t : xs) -> Just (t, xs)
    _ -> Nothing
  mkHomoVector t xs = VBuiltin (mkVecBuiltinLit (length xs)) (t : xs)

instance (BuiltinHasVecType builtin) => HasVecType (Value builtin) where
  getVectorType e = case e of
    VBuiltin (isVecBuiltinType -> True) [t, n] -> Just (mempty, t, n)
    _ -> Nothing
  mkVectorType _p t n = VBuiltin mkVecBuiltinType [t, n]

instance (BuiltinHasListLiterals builtin) => HasStandardListLits (Value builtin) where
  getNil = \case
    VBuiltin (isBuiltinNil -> True) [t] -> Just (mempty, t)
    _ -> Nothing
  mkNil t = VBuiltin mkBuiltinNil [t]

  getCons = \case
    VBuiltin (isBuiltinCons -> True) [t, x, xs] -> Just (mempty, t, x, xs)
    _ -> Nothing
  mkCons t x xs = VBuiltin mkBuiltinCons [t, x, xs]

instance (BuiltinHasRatTensor builtin) => HasRatTensors (Value builtin) where
  getRatTensorOp e = case e of
    VBuiltin (getRatTensorBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkRatTensorOp op = VBuiltin (mkRatTensorBuiltin op)

instance (BuiltinHasBoolTensor builtin) => HasBoolTensors (Value builtin) where
  getBoolTensorOp e = case e of
    VBuiltin (getBoolTensorBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkBoolTensorOp op = VBuiltin (mkBoolTensorBuiltin op)

instance (BuiltinHasDimensionTypes builtin) => HasDimensionTypes (Value builtin) where
  getDimensionTypeOp e = case e of
    VBuiltin (getDimensionTypeBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkDimensionTypeOp op = VBuiltin (mkDimensionTypeBuiltin op)

instance (BuiltinHasDimensionData builtin) => HasDimensionData (Value builtin) where
  getDimensionDataOp e = case e of
    VBuiltin (getDimensionDataBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkDimensionDataOp op = VBuiltin (mkDimensionDataBuiltin op)

--------------------------------------------------------------------------------
-- Value Function patterns

-- TODO this should really be removed.
pattern VBuiltinFunction :: (BuiltinHasStandardData builtin) => BuiltinFunction -> Spine builtin -> Value builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args
