module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter)
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Expr.Normalised
import Vehicle.Syntax.Builtin (Builtin)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Context

type NetworkContext = Map Name NetworkContextInfo

--------------------------------------------------------------------------------
-- Resource contexts

type InferableParameterEntry = (DeclProvenance, ExternalResource, Int)

type InferableParameterContext = Map Identifier (Provenance, GluedType Builtin, Maybe InferableParameterEntry)

type ExplicitParameterContext = Map Identifier (WHNFValue Builtin)

--------------------------------------------------------------------------------
-- The resource monad

type MonadReadResources m =
  ( MonadIO m,
    MonadExpandResources m,
    MonadFreeContext Builtin m,
    MonadWriter (FreeCtx Builtin) m
  )

type MonadExpandResources m =
  ( MonadCompile m,
    MonadReader Resources m,
    MonadState (NetworkContext, InferableParameterContext, ExplicitParameterContext) m
  )

getExplicitParameterContext ::
  (MonadExpandResources m) =>
  m ExplicitParameterContext
getExplicitParameterContext = gets (\(_, _, w) -> w)

getInferableParameterContext ::
  (MonadExpandResources m) =>
  m InferableParameterContext
getInferableParameterContext = gets (\(_, v, _) -> v)

isInferableParameter ::
  (MonadExpandResources m) =>
  Identifier ->
  m Bool
isInferableParameter ident =
  Map.member ident <$> getInferableParameterContext

noteInferableParameter ::
  (MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  GluedType Builtin ->
  m ()
noteInferableParameter p ident paramType =
  modify (\(u, v, w) -> (u, Map.insert ident (p, paramType, Nothing) v, w))

noteExplicitParameter ::
  (MonadExpandResources m) =>
  Identifier ->
  WHNFValue Builtin ->
  m ()
noteExplicitParameter ident value =
  modify (\(u, v, w) -> (u, v, Map.insert ident value w))

addPossibleInferableParameterSolution ::
  (MonadExpandResources m) =>
  Identifier ->
  Provenance ->
  GluedType Builtin ->
  InferableParameterEntry ->
  m ()
addPossibleInferableParameterSolution ident p declType entry =
  modify (\(u, v, w) -> (u, Map.insert ident (p, declType, Just entry) v, w))

addNetworkType ::
  (MonadExpandResources m) =>
  Identifier ->
  NetworkContextInfo ->
  m ()
addNetworkType ident details =
  modify (\(u, v, w) -> (Map.insert (nameOf ident) details u, v, w))
