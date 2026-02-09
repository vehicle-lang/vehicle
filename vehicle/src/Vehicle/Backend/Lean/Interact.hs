module Vehicle.Backend.Lean.Interact
  ( writeLeanFile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Version (makeVersion)
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Prelude.Logging

writeLeanFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeLeanFile = writeResultToFile (Just leanOutputFormat)

leanOutputFormat :: ExternalOutputFormat
leanOutputFormat =
  ExternalOutputFormat
    { formatName = "Lean",
      formatVersion = Just $ makeVersion [4, 0, 0],
      commentStyle = Line "--",
      emptyLines = True
    }
