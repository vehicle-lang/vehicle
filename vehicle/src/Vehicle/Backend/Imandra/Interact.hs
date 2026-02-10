module Vehicle.Backend.Imandra.Interact
  ( writeImandraFile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Prelude.Logging

writeImandraFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeImandraFile = writeResultToFileWide (Just imandraOutputFormat)

imandraOutputFormat :: ExternalOutputFormat
imandraOutputFormat =
  ExternalOutputFormat
    { formatName = "Imandra",
      formatVersion = Nothing,
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }
