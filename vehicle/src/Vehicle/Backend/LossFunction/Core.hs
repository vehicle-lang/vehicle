module Vehicle.Backend.LossFunction.Core where

import Control.Monad.Reader (MonadReader (..))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Backend.Prelude (DifferentiableLogicID)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (FreeEnv)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Loss.Core (MixedClosure)
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Expr.Normalised (BoundEnv, Spine, VBinder, VDecl, Value, WHNFClosure (..))
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..))

--------------------------------------------------------------------------------
-- Closures

-- NOTE that MixedClosure is not quotable as you can't convert the standard
-- closure to loss builtins.

type MixedBoundEnv = BoundEnv MixedClosure LossBuiltin

type MixedFreeEnv = FreeEnv MixedClosure LossBuiltin

type MixedLossValue = Value MixedClosure LossBuiltin

type MixedLossSpine = Spine MixedClosure LossBuiltin

type MixedLossBinder = VBinder MixedClosure LossBuiltin

type MixedLossDecl = VDecl MixedClosure LossBuiltin

--------------------------------------------------------------------------------
-- Monad

type MonadLossCtx =
  ( DifferentialLogicImplementation,
    DeclProvenance,
    FreeEnv (WHNFClosure Builtin) Builtin,
    MixedFreeEnv,
    GenericBoundCtx MixedLossBinder
  )

type MonadLoss m =
  ( MonadCompile m,
    MonadReader MonadLossCtx m
  )

-- | Standard library operations that we don't want to normalise
-- as we need them present to convert into tensors.
preservedStdLibOps :: Set Identifier
preservedStdLibOps =
  Set.fromList
    [ identifierOf StdForeachIndex
    ]

isPreservedStdLibOp :: GenericDecl expr -> Bool
isPreservedStdLibOp decl = identifierOf decl `Set.member` preservedStdLibOps

getLogic :: (MonadLoss m) => m DifferentialLogicImplementation
getLogic = do
  (logic, _, _, _, _) <- ask
  return logic

getDeclProvenance :: (MonadLoss m) => m DeclProvenance
getDeclProvenance = do
  (_, prov, _, _, _) <- ask
  return prov

getStandardFreeEnvWithoutHidden :: (MonadLoss m) => m (FreeEnv (WHNFClosure Builtin) Builtin)
getStandardFreeEnvWithoutHidden = do
  (_, _, env, _, _) <- ask
  return $ Map.map (\d -> if isPreservedStdLibOp d then convertToPostulate d else d) env

getLossFreeEnvWithHidden :: (MonadLoss m) => m (FreeEnv MixedClosure LossBuiltin)
getLossFreeEnvWithHidden =
  return mempty

{-
do
(_, _, _, env, _) <- ask
return env
-}
getLossFreeEnvWithoutHidden :: (MonadLoss m) => m (FreeEnv MixedClosure LossBuiltin)
getLossFreeEnvWithoutHidden = Map.map (\d -> if isPreservedStdLibOp d then convertToPostulate d else d) <$> getLossFreeEnvWithHidden

getNamedBoundCtx :: (MonadLoss m) => m NamedBoundCtx
getNamedBoundCtx = do
  (_, _, _, _, ctx) <- ask
  return $ fmap nameOf ctx

getCurrentLv :: (MonadLoss m) => m Lv
getCurrentLv = Lv . length <$> getNamedBoundCtx

addLossBinderToContext :: (MonadLoss m) => MixedLossBinder -> m a -> m a
addLossBinderToContext binder cont = do
  local (\(logic, declProv, standardEnv, freeEnv, ctx) -> (logic, declProv, standardEnv, freeEnv, binder : ctx)) cont

--------------------------------------------------------------------------------
-- Implementation

data DifferentialLogicImplementation = DifferentialLogicImplementation
  { logicID :: DifferentiableLogicID,
    translateBool :: MixedLossValue,
    translateTrue :: MixedLossValue,
    translateFalse :: MixedLossValue,
    translateAnd :: MixedLossValue,
    translateOr :: MixedLossValue,
    translateNot :: MixedLossValue,
    translateImplies :: MixedLossValue,
    translateLe :: MixedLossValue,
    translateLt :: MixedLossValue,
    translateGe :: MixedLossValue,
    translateGt :: MixedLossValue,
    translateEq :: MixedLossValue,
    translateNeq :: MixedLossValue
  }
