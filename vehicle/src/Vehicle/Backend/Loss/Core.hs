{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.Core where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Prettyprinter
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.ForcedValue (GenericThunk (..), Thunk, emptyBoundEnv)
import Vehicle.Data.DifferentiableLogic
import Vehicle.Data.Variable.Bound.Context.Tensor.Class (MonadTensorBoundContext)
import Vehicle.Data.Variable.Bound.Context.Tensor.Instance (TensorBoundContextT, runFreshTensorBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))

--------------------------------------------------------------------------------
-- MonadLogic

type LossCtx mode =
  ( DeclProvenance,
    DifferentiableLogicID,
    DifferentiableLogicImplementation mode
  )

type MonadLogicCore mode m =
  ( MonadLogger m,
    MonadReader (LossCtx mode) m,
    MonadFreeContext Builtin m,
    MonadTensorBoundContext m
  )

type MonadLogic mode m =
  ( MonadLogicCore mode m,
    MonadError CompileError m,
    MonadFreeContext (LossBuiltin mode) m
  )

runMonadLogicT ::
  (MonadLogger m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation mode ->
  DeclProvenance ->
  TensorBoundContextT (ReaderT (LossCtx mode) m) a ->
  m a
runMonadLogicT logicID logic declProv action = do
  runReaderT (runFreshTensorBoundContextT action) (declProv, logicID, logic)

getLogic :: (MonadLogic mode m) => m (DifferentiableLogicImplementation mode)
getLogic = do
  (_, _, logic) <- ask
  return logic

getDeclProvenance :: (MonadLogic mode m) => m DeclProvenance
getDeclProvenance = do
  (prov, _, _) <- ask
  return prov

getLogicField :: (MonadLogic mode m) => TensorDifferentiableLogicField -> m (Expr (LossBuiltin mode))
getLogicField field = lookupLogicField field <$> getLogic

getLogicFieldValue :: (MonadLogic mode m) => TensorDifferentiableLogicField -> m (Thunk (LossBuiltin mode))
getLogicFieldValue field = Unforced emptyBoundEnv <$> getLogicField field

lookupLogicField :: (Ord field, Pretty field) => field -> Map field value -> value
lookupLogicField field logic = case Map.lookup field logic of
  Nothing -> developerError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
  Just value -> value

--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

unexpectedOperation :: (MonadLogic mode m) => UnAnnDoc -> m b
unexpectedOperation = unexpectedExprError currentPass

currentPass :: Doc a
currentPass = "loss translation"
