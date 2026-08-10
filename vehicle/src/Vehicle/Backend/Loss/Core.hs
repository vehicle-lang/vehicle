{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Loss.Core where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..), MonadTrans (..), ReaderT (..))
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
import Vehicle.Data.Variable.Free.Context.Instance

--------------------------------------------------------------------------------
-- MonadLogic

type LossCtx =
  ( DeclProvenance,
    DifferentiableLogicID,
    DifferentiableLogicImplementation
  )

type MonadLogicCore m =
  ( MonadLogger m,
    MonadReader LossCtx m,
    MonadFreeContext Builtin m,
    MonadTensorBoundContext m
  )

type MonadLogic m =
  ( MonadLogicCore m,
    MonadError CompileError m,
    MonadFreeContext LossBuiltin m,
    MonadFreeContext Builtin m
  )

runMonadLogicT ::
  (MonadLogger m) =>
  DifferentiableLogicID ->
  DifferentiableLogicImplementation ->
  DeclProvenance ->
  TensorBoundContextT (ReaderT LossCtx m) a ->
  m a
runMonadLogicT logicID logic declProv action = do
  runReaderT (runFreshTensorBoundContextT action) (declProv, logicID, logic)

getLogic :: (MonadLogic m) => m DifferentiableLogicImplementation
getLogic = do
  (_, _, logic) <- ask
  return logic

getDeclProvenance :: (MonadLogic m) => m DeclProvenance
getDeclProvenance = do
  (prov, _, _) <- ask
  return prov

getLogicField :: (MonadLogic m) => TensorDifferentiableLogicField -> m (Expr LossBuiltin)
getLogicField field = lookupLogicField field <$> getLogic

getLogicFieldValue :: (MonadLogic m) => TensorDifferentiableLogicField -> m (Thunk LossBuiltin)
getLogicFieldValue field = Unforced emptyBoundEnv <$> getLogicField field

lookupLogicField :: (Ord field, Pretty field) => field -> Map field value -> value
lookupLogicField field logic = case Map.lookup field logic of
  Nothing -> developerError $ "Non-compiled logic field" <+> quotePretty field <+> "found"
  Just value -> value

--------------------------------------------------------------------------------
-- Other
--------------------------------------------------------------------------------

unsupportedOperation :: (MonadLogic m, MonadError CompileError m) => UnAnnDoc -> m b
unsupportedOperation op = do
  prov <- getDeclProvenance
  throwError $ UnsupportedLossOperation prov op

unexpectedOperation :: (MonadLogic m) => UnAnnDoc -> m b
unexpectedOperation = unexpectedExprError currentPass

missingLogicError :: (MonadCompile m) => [Name] -> DifferentiableLogicID -> m a
missingLogicError names = \case
  BuiltinLogic name -> developerError $ "No logic record found for builtin logic" <+> quotePretty name
  CustomLogic name -> throwError $ UnknownDifferentiableLogic name names

currentPass :: Doc a
currentPass = "loss translation"

-- This is a massive hack and we should get this fixed when we sort out the normalisation story.
instance (MonadFreeContext Builtin m) => MonadFreeContext Builtin (FreeContextT LossBuiltin m) where
  addDeclEntryToContext = mapFreeContextT . addDeclEntryToContext
  getFreeCtx = lift . getFreeCtx
  getDeclEntry proxy = lift . getDeclEntry proxy
