module Vehicle.Backend.Solver.QueryCompilation.Core where

import Control.Monad.Reader
import Data.Map qualified as Map
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor.Class (MonadReadableTensorBoundContext)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude

type MonadQueryCompilation m =
  ( MonadReader (PropertyMetaData, GlobalCtx) m,
    MonadReadableTensorBoundContext m,
    MonadReadableNameContext m,
    MonadCompile m
  )

lookupCorrespondingOutputVar ::
  (MonadQueryCompilation m) =>
  NetworkInputTensorVariable ->
  m NetworkOutputTensorVariable
lookupCorrespondingOutputVar inputVar = do
  (_, GlobalCtx {..}) <- ask
  case Map.lookup inputVar networkTensorVariables of
    Just outputVar -> return outputVar
    Nothing -> do
      varName <- prettyFriendlyInCtx inputVar
      developerError ("Network input var" <+> squotes varName <+> "has no corresponding output variable")

getNetworkApplications :: (MonadQueryCompilation m) => m NetworkApplications
getNetworkApplications = do
  (_, ctx) <- ask
  return $ networkApplications ctx
