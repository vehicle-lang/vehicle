-- | Record-handling cases for the loss backend, split out from
-- 'Vehicle.Backend.Loss.LossCompilation'.
module Vehicle.Backend.Loss.RecordCompilation
  ( -- * Dispatcher inlining
    isDispatcherIdent,
    inlineRecordDispatchers,

    -- * Compilation of record forms
    convertRecordType,
    convertRecord,
    convertRecordAcc,

    -- * Record quantifier compilation
    wrapQuantifyRecordForLoss,
  )
where

import Data.Text (isInfixOf, isPrefixOf)
import Vehicle.Backend.Loss.Core
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.NBE (evalApp)
import Vehicle.Compile.Normalise.NBE qualified as NBE
import Vehicle.Compile.Normalise.Quote (unnormaliseInCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Builtin.Standard (Builtin)
import Vehicle.Data.Builtin.Standard.Scoping (constructFromTensorFreeVar, constructTensorisableDims)
import Vehicle.Data.Code.DSL
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Bound.Context.Name (NamedBoundCtx, getFreshTensorBinderName)
import Vehicle.Data.Variable.Bound.Context.Name.Class (getNameContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, getRecordFields, getRecordProvenance)

lookupIdentValue ::
  (MonadFreeContext Builtin m) =>
  Identifier ->
  m (Value Builtin)
lookupIdentValue = NBE.lookupIdentValue

--------------------------------------------------------------------------------
-- Dispatcher inlining

-- | Recognises scope-checker-synthesised @tensor-record helpers by their name.
-- The producer ('createTensorRecordConversionFunctions' in
-- "Vehicle.Data.Builtin.Standard.Scoping") builds bare 'Text' identifiers
-- with no typed marker, so naming convention is the only signal available.
isDispatcherIdent :: Identifier -> Bool
isDispatcherIdent ident =
  let n = nameOf ident
   in "_" `isPrefixOf` n
        && ( "ToTensor" `isInfixOf` n
               || "FromTensor" `isInfixOf` n
               || "Has" `isInfixOf` n
           )

-- | Force-inline synthesised '_<R>ToTensor', '_<R>FromTensor' and the
-- per-typeclass dictionaries that NBE leaves stuck. Recurses on the result.
inlineRecordDispatchers ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  NamedBoundCtx ->
  Value Builtin ->
  m (Value Builtin)
inlineRecordDispatchers ctx value = case value of
  VFreeVar ident spine | isDispatcherIdent ident -> do
    body <- lookupIdentValue ident
    applied <- evalApp ctx body spine
    inlineRecordDispatchers ctx applied
  _ -> return value

--------------------------------------------------------------------------------
-- Record-type compilation

convertRecordType ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  Identifier ->
  m (Value LossBuiltin)
convertRecordType ident =
  return $
    VBuiltin
      (LossBuiltinType RecordType)
      [explicit (VFreeVar ident [])]

--------------------------------------------------------------------------------
-- Record value compilation

convertRecord ::
  (Monad m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  VType Builtin ->
  VRecordFields Builtin ->
  m (Value LossBuiltin)
convertRecord convertValue recordType fields = do
  fields' <- traverse convertValue fields
  return $ VRecord (recordTypeMarker recordType) fields'

recordTypeMarker :: VType Builtin -> VType LossBuiltin
recordTypeMarker = \case
  VFreeVar ident _ ->
    VBuiltin (LossBuiltinType RecordType) [explicit (VFreeVar ident [])]
  typ ->
    developerError $
      "recordTypeMarker: expected a record-typed VFreeVar, got"
        <+> prettyVerbose typ

convertRecordAcc ::
  (Monad m) =>
  (Value Builtin -> m (Value LossBuiltin)) ->
  VType Builtin ->
  Value Builtin ->
  FieldName ->
  Spine Builtin ->
  m (Value LossBuiltin)
convertRecordAcc convertValue recordType recordVal field spine = do
  record' <- convertValue recordVal
  spine' <- traverse (traverse convertValue) spine
  return $ VRecordAcc (recordTypeMarker recordType) record' field spine'

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
