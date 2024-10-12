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
data WHNFClosure builtin = WHNFClosure (BoundEnv (WHNFClosure builtin) builtin) (Expr builtin)
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

type VType closure builtin = Value closure builtin

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

-- | The information stored for each variable in the environment. We choose
-- to store the binder as it's a convenient mechanism for passing through
-- name, relevance for pretty printing and debugging.
type EnvEntry closure builtin = (GenericBinder (), Value closure builtin)

type BoundEnv closure builtin = GenericBoundCtx (EnvEntry closure builtin)

emptyBoundEnv :: BoundEnv closure builtin
emptyBoundEnv = mempty

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
extendEnvWithDefined value binder env = (void binder, value) : env

boundContextToEnv :: BoundCtx expr -> BoundEnv closure builtin
boundContextToEnv ctx = do
  let numberedCtx = zip ctx (reverse [0 .. Lv (length ctx - 1)])
  fmap (\(binder, lv) -> (void binder, VBoundVar lv [])) numberedCtx

-- | Converts an environment to set of values suitable for printing
cheatEnvToValues :: BoundEnv closure builtin -> GenericBoundCtx (Value closure builtin)
cheatEnvToValues = fmap envEntryToValue
  where
    envEntryToValue :: EnvEntry closure builtin -> Value closure builtin
    envEntryToValue (binder, value) = do
      let ident = stdlibIdentifier (fromMaybe "_" (nameOf binder) <> " =")
      VFreeVar ident [explicit value]

type FreeEnv closure builtin = Map Identifier (VDecl closure builtin)

-----------------------------------------------------------------------------
-- WHNF

type WHNFValue builtin = Value (WHNFClosure builtin) builtin

type WHNFType builtin = WHNFValue builtin

type WHNFArg builtin = VArg (WHNFClosure builtin) builtin

type WHNFBinder builtin = VBinder (WHNFClosure builtin) builtin

type WHNFSpine builtin = Spine (WHNFClosure builtin) builtin

type WHNFDecl builtin = GenericDecl (WHNFValue builtin)

type WHNFProg builtin = GenericProg (WHNFValue builtin)

type WHNFBoundEnv builtin = BoundEnv (WHNFClosure builtin) builtin

type WHNFFreeEnv builtin = FreeEnv (WHNFClosure builtin) builtin

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
  { unnormalised :: Expr builtin,
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
  (Expr builtin -> m (Expr builtin)) ->
  GluedExpr builtin ->
  m (GluedExpr builtin)
traverseUnnormalised f (Glued u n) = Glued <$> f u <*> pure n

-----------------------------------------------------------------------------
-- Instances

instance (BuiltinHasStandardTypes builtin) => HasStandardTypes (Value closure builtin) where
  mkType _p b = VBuiltin (mkBuiltinType b)
  getType e = case e of
    VBuiltin b args -> case getBuiltinType b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (BuiltinHasStandardData builtin) => HasStandardData (Value closure builtin) where
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

instance (BuiltinHasBoolLiterals builtin) => HasBoolLits (Value closure builtin) where
  getBoolLit = \case
    VBuiltin (getBoolBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkBoolLit _p b = VBuiltin (mkBoolBuiltinLit b) []

instance (BuiltinHasIndexLiterals builtin) => HasIndexLits (Value closure builtin) where
  getIndexLit e = case e of
    VBuiltin (getIndexBuiltinLit -> Just n) [] -> Just (mempty, n)
    _ -> Nothing
  mkIndexLit _p x = VBuiltin (mkIndexBuiltinLit x) mempty

instance (BuiltinHasNatLiterals builtin) => HasNatLits (Value closure builtin) where
  getNatLit e = case e of
    VBuiltin (getNatBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkNatLit _p x = VBuiltin (mkNatBuiltinLit x) mempty

instance (BuiltinHasRatLiterals builtin) => HasRatLits (Value closure builtin) where
  getRatLit e = case e of
    VBuiltin (getRatBuiltinLit -> Just b) [] -> Just (mempty, b)
    _ -> Nothing
  mkRatLit _p x = VBuiltin (mkRatBuiltinLit x) mempty

instance (BuiltinHasRatType builtin) => HasRatType (Value closure builtin) where
  getRatType e = case e of
    VBuiltin (isRatBuiltinType -> True) [] -> Just mempty
    _ -> Nothing
  mkRatType _p = VBuiltin mkRatBuiltinType []

instance (BuiltinHasVecLiterals builtin) => HasStandardVecLits (Value closure builtin) where
  getHomoVector = \case
    VBuiltin (getVecBuiltinLit -> Just {}) (t : xs) -> Just (t, xs)
    _ -> Nothing
  mkHomoVector t xs = VBuiltin (mkVecBuiltinLit (length xs)) (t : xs)

instance (BuiltinHasVecType builtin) => HasVecType (Value closure builtin) where
  getVectorType e = case e of
    VBuiltin (isVecBuiltinType -> True) [t, n] -> Just (mempty, t, n)
    _ -> Nothing
  mkVectorType _p t n = VBuiltin mkVecBuiltinType [t, n]

instance (BuiltinHasListLiterals builtin) => HasStandardListLits (Value closure builtin) where
  getNil = \case
    VBuiltin (isBuiltinNil -> True) [t] -> Just (mempty, t)
    _ -> Nothing
  mkNil t = VBuiltin mkBuiltinNil [t]

  getCons = \case
    VBuiltin (isBuiltinCons -> True) [t, x, xs] -> Just (mempty, t, x, xs)
    _ -> Nothing
  mkCons t x xs = VBuiltin mkBuiltinCons [t, x, xs]

instance (BuiltinHasRatTensor builtin) => HasRatTensors (Value closure builtin) where
  getRatTensorOp e = case e of
    VBuiltin (getRatTensorBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkRatTensorOp op = VBuiltin (mkRatTensorBuiltin op)

instance (BuiltinHasBoolTensor builtin) => HasBoolTensors (Value closure builtin) where
  getBoolTensorOp e = case e of
    VBuiltin (getBoolTensorBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkBoolTensorOp op = VBuiltin (mkBoolTensorBuiltin op)

instance (BuiltinHasDimensionTypes builtin) => HasDimensionTypes (Value closure builtin) where
  getDimensionTypeOp e = case e of
    VBuiltin (getDimensionTypeBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkDimensionTypeOp op = VBuiltin (mkDimensionTypeBuiltin op)

instance (BuiltinHasDimensionData builtin) => HasDimensionData (Value closure builtin) where
  getDimensionDataOp e = case e of
    VBuiltin (getDimensionDataBuiltin -> Just op) args -> Just (op, args)
    _ -> Nothing
  mkDimensionDataOp op = VBuiltin (mkDimensionDataBuiltin op)

--------------------------------------------------------------------------------
-- WHNFValue Function patterns

-- TODO this should really be removed.
pattern VBuiltinFunction :: (BuiltinHasStandardData builtin) => BuiltinFunction -> Spine closure builtin -> Value closure builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args
