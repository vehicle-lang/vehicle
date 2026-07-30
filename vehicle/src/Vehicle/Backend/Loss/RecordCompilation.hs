module Vehicle.Backend.Loss.RecordCompilation
  ( wrapQuantifyRecordForLoss,
  )
where

import Vehicle.Backend.Loss.Core
import Vehicle.Compile.Normalise.Force (forceThunk)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Scope.Records (constructFromTensorFreeVar)
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Variable.Bound.Context.Name (getFreshTensorBinderName)
import Vehicle.Data.Variable.Bound.Context.Name.Class (getNameContext)
import Vehicle.Data.Variable.Free.Context (getRecordFields, getRecordProvenance)

-- | Loss-side counterpart of wrapQuantifyRecord in
--  Vehicle.Backend.Solver.UserVariableElimination, which wraps the binder &
--  body of a record quantifier in a tensor quantifier
--  e.g. given Pair has fields { a : Real, b : Real }
--  forall (r : Pair) . (body)
--  becomes
--  forall (_t0 : tensor Real [2]) . (body (_PairFromTensor _t0))
wrapQuantifyRecordForLoss ::
  (MonadLogic m) =>
  QuantifyRecordArgs (Thunk Builtin) (Closure Builtin) ->
  m (QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin))
wrapQuantifyRecordForLoss QuantifyRecordArgs {..} = do
  namedCtx <- getNameContext
  forcedType <- forceThunk quantifyRecordType
  recordTypeIdent <- case forcedType of
    VFreeVar v _spine -> pure v
    _ -> developerError "Record binder is not of expected format."

  let recordQLam = unnormalise (boundCtxLv namedCtx) $ VLam quantifyRecordBinder quantifyRecordBody
  fields <- getRecordFields recordTypeIdent
  shape <- getTensorRecordShape fields
  let dims = Forced $ mkDims shape

  let Closure boundEnv _body = quantifyRecordBody
  let tensorType = Forced $ ITensorType (Forced IRatType) dims
  let tensorBinder = mkExplicitBinder tensorType (Just (mempty, getFreshTensorBinderName namedCtx))

  let tensorBoundVar = explicit $ BoundVar mempty 0
  recordTypeProv <- getRecordProvenance recordTypeIdent
  let fromTensorExpr = App (FreeVar recordTypeProv $ constructFromTensorFreeVar recordTypeIdent) [tensorBoundVar]

  let nestedBody = App recordQLam [Arg Explicit Relevant fromTensorExpr]
  return $ QuantifyRatTensorArgs dims tensorBinder (Closure boundEnv nestedBody)
