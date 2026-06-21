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
import Vehicle.Compile.Normalise.NBE (eval, evalDecl)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Core (BuiltinConstructor (..), BuiltinType (..))
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Code.Value (Value (..), emptyBoundEnv)
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
  runFreshFreeContextT (Proxy @Builtin) $ J.Main <$> goDecls decls
  where
    goDecls [] = return []
    goDecls (d : ds) = do
      mSchema <- declToSchema d
      normDecl <- evalDecl d
      rest <- addDeclEntryToContext normDecl (goDecls ds)
      return $ maybe rest (: rest) mSchema

declToSchema ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  Decl Builtin ->
  m (Maybe JDecl)
declToSchema = \case
  DefRecord p ident anns _telescope fields
    | isAnnotatedAsTensor anns -> do
        fieldSchemas <- traverse convertSchemaField fields
        return $ Just $ DefRecordSchema p (nameOf ident) fieldSchemas
  _ -> return Nothing

convertSchemaField ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  (FieldName, Type Builtin) ->
  m (Name, JFieldType)
convertSchemaField (fieldName, fieldType) = do
  ft <- convertJFieldType fieldType
  return (nameOf fieldName, ft)

convertJFieldType ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  Type Builtin ->
  m JFieldType
convertJFieldType typ = do
  typVal <- eval mempty emptyBoundEnv typ
  case typVal of
    VBuiltin (BuiltinType RatType) _ -> return JFieldScalarReal
    VBuiltin (BuiltinType TensorType) (fmap argExpr -> [_elemTyp, dimsValue]) -> do
      dims <- extractDims dimsValue
      return $ case dims of
        [] -> JFieldScalarReal
        _ -> JFieldTensorReal dims
    VFreeVar refIdent _ -> return $ JFieldRecordRef (nameOf refIdent)
    _ -> developerError $ "Unsupported @tensor record field type:" <+> prettyVerbose typVal

extractDims :: (MonadCompile m) => Value Builtin -> m [Either Int Name]
extractDims = go
  where
    go = \case
      VBuiltin (BuiltinConstructor Nil) _ -> return []
      VBuiltin (BuiltinConstructor Cons) (fmap argExpr -> [_, d, rest]) -> do
        dim <- extractOneDim d
        ds <- go rest
        return (dim : ds)
      v -> developerError $ "Unexpected dims spine in @tensor record field:" <+> prettyVerbose v

extractOneDim :: (MonadCompile m) => Value Builtin -> m (Either Int Name)
extractOneDim = \case
  VBuiltin (BuiltinConstructor (NatLiteral n)) _ -> return $ Left n
  VFreeVar ident _ -> return $ Right (nameOf ident)
  VBoundVar _ _ -> return $ Right "_"
  v -> developerError $ "Unexpected dimension expression in @tensor record field:" <+> prettyVerbose v
