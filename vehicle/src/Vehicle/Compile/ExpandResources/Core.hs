module Vehicle.Compile.ExpandResources.Core where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter)
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin (Builtin)

--------------------------------------------------------------------------------
-- Context

type NetworkContext = Map Name (FilePath, NetworkType)

--------------------------------------------------------------------------------
-- Resource contexts

type InferableParameterEntry = (DeclProvenance, ExternalResource, Int)

type InferableParameterContext = Map Identifier (Either Provenance InferableParameterEntry)

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
  m ()
noteInferableParameter p ident =
  modify (\(u, v, w) -> (u, Map.insert ident (Left p) v, w))

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
  InferableParameterEntry ->
  m ()
addPossibleInferableParameterSolution ident entry =
  modify (\(u, v, w) -> (u, Map.insert ident (Right entry) v, w))

addNetworkType ::
  (MonadExpandResources m) =>
  Identifier ->
  (FilePath, NetworkType) ->
  m ()
addNetworkType ident details =
  modify (\(u, v, w) -> (Map.insert (nameOf ident) details u, v, w))
