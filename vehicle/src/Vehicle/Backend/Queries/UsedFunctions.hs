module Vehicle.Backend.Queries.UsedFunctions
  ( UsedFunctionsCtx,
    UsedFunctionsInfo,
    getUsedFunctions,
    getUsedFunctionsCtx,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet (singleton)
import Data.Map qualified as Map (lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.DeBruijn (dbLevelToIndex)
import Vehicle.Data.NormalisedExpr

--------------------------------------------------------------------------------
-- Builtin and free variable tracking

type UsedFunctionsCtx = GenericFreeCtx UsedFunctionsInfo

type UsedFunctionsInfo = (HashSet BuiltinFunction, Set Identifier)

getUsedFunctions ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  Expr Ix Builtin ->
  UsedFunctionsInfo
getUsedFunctions freeCtx boundCtx expr = case expr of
  Universe {} -> mempty
  Meta {} -> mempty
  Hole {} -> mempty
  Pi {} -> mempty
  BoundVar _ v -> getUsedVarsBoundVar boundCtx v
  Builtin _ b -> getUsedFunctionsBuiltin b
  FreeVar _ ident -> getUsedFunctionsFreeVar freeCtx ident
  App _ fun args -> foldr ((<>) . getUsedFunctions freeCtx boundCtx . argExpr) (getUsedFunctions freeCtx boundCtx fun) args
  Let _ e1 _binder e2 -> getUsedFunctions freeCtx boundCtx e1 <> getUsedFunctions freeCtx boundCtx e2
  Lam _ _binder e -> getUsedFunctions freeCtx (mempty : boundCtx) e

getUsedNormFunctions ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  WHNFValue Builtin ->
  UsedFunctionsInfo
getUsedNormFunctions freeCtx boundCtx expr = case expr of
  VPi {} -> mempty
  VUniverse {} -> mempty
  VMeta {} -> mempty
  VLam _ (WHNFBody env body) -> do
    let envInfo = getUsedFunctionsEnv freeCtx boundCtx env
    let bodyInfo = getUsedFunctions freeCtx (mempty : boundCtx) body
    envInfo <> bodyInfo
  VBoundVar v spine -> do
    let varInfo = getUsedVarsBoundVar boundCtx (dbLevelToIndex (Lv (length boundCtx)) v)
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    varInfo <> spineInfo
  VFreeVar ident spine -> do
    let identInfo = getUsedFunctionsFreeVar freeCtx ident
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    identInfo <> spineInfo
  VBuiltin b spine -> do
    let builtinInfo = getUsedFunctionsBuiltin b
    let spineInfo = getUsedFunctionsSpine freeCtx boundCtx spine
    builtinInfo <> spineInfo

getUsedFunctionsBuiltin ::
  Builtin ->
  UsedFunctionsInfo
getUsedFunctionsBuiltin = \case
  BuiltinFunction f -> (HashSet.singleton f, mempty)
  _ -> mempty

getUsedFunctionsFreeVar ::
  GenericFreeCtx UsedFunctionsInfo ->
  Identifier ->
  UsedFunctionsInfo
getUsedFunctionsFreeVar freeCtx ident =
  (mempty, Set.singleton ident) <> fromMaybe mempty (Map.lookup ident freeCtx)

getUsedVarsBoundVar ::
  GenericBoundCtx UsedFunctionsInfo ->
  Ix ->
  UsedFunctionsInfo
getUsedVarsBoundVar boundCtx ix =
  fromMaybe mempty (lookupIx boundCtx ix)

getUsedFunctionsSpine ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  WHNFSpine Builtin ->
  UsedFunctionsInfo
getUsedFunctionsSpine freeCtx boundCtx =
  foldMap (getUsedNormFunctions freeCtx boundCtx . argExpr)

getUsedFunctionsEnv ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  WHNFEnv Builtin ->
  UsedFunctionsInfo
getUsedFunctionsEnv freeCtx boundCtx =
  foldMap (getUsedFunctionsEnvEntry freeCtx boundCtx)

getUsedFunctionsEnvEntry ::
  GenericFreeCtx UsedFunctionsInfo ->
  GenericBoundCtx UsedFunctionsInfo ->
  EnvEntry 'WHNF Builtin ->
  UsedFunctionsInfo
getUsedFunctionsEnvEntry freeCtx boundCtx (_binder, value) = case value of
  Bound {} -> mempty
  Defined v -> getUsedNormFunctions freeCtx boundCtx v

getUsedFunctionsCtx ::
  GenericFreeCtx UsedFunctionsInfo ->
  WHNFEnv Builtin ->
  GenericBoundCtx UsedFunctionsInfo
getUsedFunctionsCtx freeCtx =
  foldr (\u v -> getUsedFunctionsEnvEntry freeCtx v u : v) mempty
