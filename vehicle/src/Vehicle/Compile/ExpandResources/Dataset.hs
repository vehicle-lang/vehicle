module Vehicle.Compile.ExpandResources.Dataset
  ( parseDataset,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import System.FilePath (takeExtension)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset.IDX (readIDX)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.TypedValue (TypeValue (..), toTypeValue)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface.Patterns
import Vehicle.Data.Variable.Bound.Context.Name.Instance (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Free.Context (getRecordFields)

--------------------------------------------------------------------------------
-- Dataset parsing

parseDataset ::
  (MonadIO m, MonadExpandResources m) =>
  DeclProvenance ->
  Type Builtin ->
  FilePath ->
  m (Thunk Builtin)
parseDataset decl@(ident, _) expectedType filePath = do
  datasetType <- getDatasetType $ Unforced emptyBoundEnv expectedType
  logDebug MinDetail $ "Reading" <+> squotes (pretty ident)
  value <- case takeExtension filePath of
    ".idx" -> readIDX filePath decl expectedType datasetType
    ext -> throwError $ UnsupportedResourceFormat decl Dataset ext
  return value

getDatasetType ::
  (MonadExpandResources m) =>
  UnforcedType Builtin ->
  m (DatasetType (Thunk Builtin))
getDatasetType typ = do
  forcedType <- runFreshNameBoundContextT $ forceThunk typ
  case toTypeValue forcedType of
    VListType tElem -> DatasetListType <$> getDatasetType tElem
    VVectorType tElem dim -> DatasetVectorType <$> getDatasetType tElem <*> pure dim
    VTensorType tElem dims -> DatasetTensorType <$> getDatasetElementType tElem <*> pure dims
    VTypeFreeVar ident spine -> case spine of
      _ : _ -> do
        -- let substFieldDeclarations = fmap (calculateRarameterisedRecordFieldType _ _) fieldDeclarations
        developerError "Parameterised dataset types not yet supported"
      [] -> do
        fieldDeclarations <- getRecordFields ident
        fieldsTypes <- traverseRecordFields (getDatasetType . Unforced emptyBoundEnv) fieldDeclarations
        return $ DatasetRecordType ident fieldsTypes
    _ -> DatasetElementType <$> getDatasetElementType (Forced forcedType)

getDatasetElementType ::
  (MonadExpandResources m) =>
  UnforcedType Builtin ->
  m (DatasetElementType (Thunk Builtin))
getDatasetElementType tElem = do
  forcedTElem <- runFreshNameBoundContextT $ forceThunk tElem
  case forcedTElem of
    IRatType -> return DatasetRealType
    INatType -> return DatasetNatType
    IIndexType size -> return $ DatasetIndexType size
    _ -> resourceTypingError "dataset element" forcedTElem
