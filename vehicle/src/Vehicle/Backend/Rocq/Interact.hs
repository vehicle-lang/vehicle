module Vehicle.Backend.Rocq.Interact
  ( writeRocqFile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Version (makeVersion)
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Prelude.Logging

writeRocqFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeRocqFile = writeResultToFile (Just rocqOutputFormat)

rocqOutputFormat :: ExternalOutputFormat
rocqOutputFormat =
  ExternalOutputFormat
    { formatName = "Rocq",
      formatVersion = Just $ makeVersion [9, 0, 0],
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }
