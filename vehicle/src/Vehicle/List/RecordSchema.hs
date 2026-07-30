module Vehicle.List.RecordSchema
  ( listRecordSchemas,
    extractRecordSchemas,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Backend.Loss.JSON (JDecl (..), JFieldType (..))
import Vehicle.Backend.Loss.JSON qualified as J
import Vehicle.Backend.Prelude (writeResultToFile)
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Builtin (getDimsExprs)
import Vehicle.Compile.Normalise.Force (eval, forceThunk)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Core (BuiltinConstructor (..), BuiltinType (..))
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class (MonadNameContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclEntryToContext, runFreshFreeContextT)

listRecordSchemas ::
  (MonadCompile m, MonadStdIO m) =>
  Maybe FilePath ->
  Prog Builtin ->
  m ()
listRecordSchemas outputFile typedProg = do
  jprog <- extractRecordSchemas typedProg
  writeResultToFile Nothing outputFile (prettyAsJSON jprog)

extractRecordSchemas :: (MonadCompile m) => Prog Builtin -> m J.JProg
extractRecordSchemas (Main decls) =
  runFreshFreeContextT (Proxy @Builtin) $ runFreshNameBoundContextT $ J.Main <$> goDecls decls
  where
    goDecls [] = return []
    goDecls (d : ds) = do
      mSchema <- declToSchema d
      rest <- addDeclEntryToContext d (goDecls ds)
      return $ maybe rest (: rest) mSchema

declToSchema ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Decl Builtin ->
  m (Maybe JDecl)
declToSchema = \case
  DefRecord p ident anns _telescope fields _ops
    | isAnnotatedAsTensor anns -> do
        fieldSchemas <- traverse convertSchemaField fields
        return $ Just $ DefRecordSchema p (nameOf ident) fieldSchemas
  _ -> return Nothing

convertSchemaField ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  (FieldName, Type Builtin) ->
  m (Name, JFieldType)
convertSchemaField (fieldName, fieldType) = do
  ft <- convertJFieldType fieldType
  return (nameOf fieldName, ft)

convertJFieldType ::
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Type Builtin ->
  m JFieldType
convertJFieldType typ = do
  typVal <- eval emptyBoundEnv typ
  case typVal of
    VBuiltin (BuiltinType RatType) _ -> return FieldScalarReal
    VBuiltin (BuiltinType TensorType) (fmap argExpr -> [_elemTyp, dimsValue]) -> do
      dims <- extractDims dimsValue
      return $ case dims of
        [] -> FieldScalarReal
        _ -> FieldTensorReal dims
    VFreeVar refIdent _ -> return $ FieldRecordRef (nameOf refIdent)
    _ -> developerError $ "Unsupported @tensor record field type:" <+> prettyVerbose typVal

extractDims :: (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) => Thunk Builtin -> m [Either Int Name]
extractDims dimsValue = do
  dims <- getDimsExprs dimsValue
  case dims of
    Left v -> developerError $ "Unexpected dims spine in @tensor record field:" <+> prettyVerbose v
    Right ds -> traverse extractOneDim ds

extractOneDim :: (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) => Thunk Builtin -> m (Either Int Name)
extractOneDim thunk = do
  value <- forceThunk thunk
  case value of
    VBuiltin (BuiltinConstructor (NatLiteral n)) _ -> return $ Left n
    VFreeVar ident _ -> return $ Right (nameOf ident)
    v -> developerError $ "Unexpected dimension expression in @tensor record field:" <+> prettyVerbose v
