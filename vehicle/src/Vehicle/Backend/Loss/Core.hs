module Vehicle.Backend.Loss.Core where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Vehicle.Backend.Loss.Logics
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core (Builtin (..))
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.Value (Value (..))
import Vehicle.Data.Variable.Bound.Context.Tensor.Class (MonadTensorBoundContext)
import Vehicle.Data.Variable.Bound.Context.Tensor.Instance (TensorBoundContextT, runFreshTensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

type MonadLossCtx =
  ( DeclProvenance,
    CompiledDifferentiableLogic
  )

type MonadLogic m =
  ( MonadCompile m,
    MonadReader MonadLossCtx m,
    MonadFreeContext Builtin m,
    MonadTensorBoundContext m
  )

runMonadLogicT ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  CompiledDifferentiableLogic ->
  DeclProvenance ->
  TensorBoundContextT (ReaderT MonadLossCtx m) a ->
  m a
runMonadLogicT logic origin action =
  runReaderT (runFreshTensorBoundContextT action) (origin, logic)

getLogic :: (MonadLogic m) => m DifferentiableLogicImplementation
getLogic = do
  (_, (_, logic)) <- ask
  return logic

getDeclProvenance :: (MonadLogic m) => m DeclProvenance
getDeclProvenance = do
  (prov, _) <- ask
  return prov

getLogicField :: (MonadLogic m) => TensorDifferentiableLogicField -> m (Value LossBuiltin)
getLogicField field = do
  logic <- getLogic
  lookupLogicField field logic

lookupLogicField :: (MonadCompile m, Ord field, Pretty field) => field -> Map field value -> m value
lookupLogicField field logic = do
  case Map.lookup field logic of
    Nothing -> compilerDeveloperError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
    Just value -> return value

currentPass :: Doc a
currentPass = "loss compilation"
