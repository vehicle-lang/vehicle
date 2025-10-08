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
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Dataset parsing

parseDataset ::
  (MonadIO m, MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  m (Value Builtin)
parseDataset decl@(ident, _) expectedType filePath = do
  logDebug MinDetail $ "Reading" <+> squotes (pretty ident)
  value <- case takeExtension filePath of
    ".idx" -> readIDX filePath decl expectedType
    ext -> throwError $ UnsupportedResourceFormat decl Dataset ext
  return value
