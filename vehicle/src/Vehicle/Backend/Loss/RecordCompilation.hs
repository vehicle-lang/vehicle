-- | Record-handling cases for the loss backend, split out from
-- 'Vehicle.Backend.Loss.LossCompilation'.
module Vehicle.Backend.Loss.RecordCompilation
  ( wrapQuantifyRecordForLoss,
  )
where

import Vehicle.Backend.Loss.Core
import Vehicle.Compile.Normalise.NBE qualified as NBE
import Vehicle.Compile.Normalise.Quote (unnormaliseInCtx)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Builtin.Standard.Scoping (constructFromTensorFreeVar, constructTensorisableDims)
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Bound.Context.Name (getFreshTensorBinderName)
import Vehicle.Data.Variable.Bound.Context.Name.Class (getNameContext)
import Vehicle.Data.Variable.Free.Context (getRecordFields, getRecordProvenance)

--------------------------------------------------------------------------------
-- Record quantifier compilation

-- | Mirror of 'Solver.wrapQuantifyRecord' for FM extraction: replaces the
-- record binder with a flat tensor binder and reconstructs the record in the
-- body via '_<Name>FromTensor'.
wrapQuantifyRecordForLoss ::
  (MonadLogic m) =>
  QuantifyRecordArgs (Value Builtin) (Closure Builtin) ->
  m (QuantifyRatTensorArgs (Value Builtin) (Closure Builtin))
wrapQuantifyRecordForLoss QuantifyRecordArgs {..} = do
  namedCtx <- getNameContext
  recordTypeIdent <- case toTypeValue quantifyRecordType of
    VFreeTypeVar v _spine -> pure v
    _ -> developerError "Record binder is not of expected format."

  recordQLam <- unnormaliseInCtx $ VLam quantifyRecordBinder quantifyRecordBody
  fields <- getRecordFields recordTypeIdent
  let shape = constructTensorisableDims fields
  let dims = mkDims shape

  let Closure boundEnv _body = quantifyRecordBody
  tensorType <- NBE.eval namedCtx boundEnv $ fromDSL mempty $ tTensor tRat (toDSL dims)
  normalisedDims <- NBE.eval namedCtx boundEnv dims
  let tensorBinder = mkExplicitBinder tensorType (Just (mempty, getFreshTensorBinderName namedCtx))

  let tensorBoundVar = explicit $ BoundVar mempty 0
  recordTypeProv <- getRecordProvenance recordTypeIdent
  let fromTensorExpr = App (constructFromTensorFreeVar recordTypeIdent recordTypeProv) [tensorBoundVar]

  let nestedBody = App recordQLam [Arg Explicit Relevant fromTensorExpr]
  return $ QuantifyRatTensorArgs normalisedDims tensorBinder (Closure boundEnv nestedBody)
