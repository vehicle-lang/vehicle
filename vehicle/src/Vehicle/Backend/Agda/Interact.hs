module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Version (makeVersion)
import Vehicle.Backend.Prelude
import Vehicle.Prelude

writeAgdaFile ::
  (MonadLogger m, MonadIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeAgdaFile = writeResultToFile (Just agdaOutputFormat)

agdaOutputFormat :: ExternalOutputFormat
agdaOutputFormat =
  ExternalOutputFormat
    { formatName = "Agda",
      formatVersion = Just $ makeVersion [2, 6, 2],
      commentToken = "--"
    }
