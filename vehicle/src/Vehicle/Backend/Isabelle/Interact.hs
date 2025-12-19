module Vehicle.Backend.Isabelle.Interact
  ( writeIsabelleFile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Version (makeVersion)
import Vehicle.Backend.Prelude
import Vehicle.Prelude
import Vehicle.Prelude.Logging

writeIsabelleFile ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  Maybe FilePath ->
  Doc a ->
  m ()
writeIsabelleFile = writeResultToFile (Just isabelleOutputFormat)

isabelleOutputFormat :: ExternalOutputFormat
isabelleOutputFormat =
  ExternalOutputFormat
    { formatName = "Isabelle",
      formatVersion = Just $ makeVersion [2024],
      commentStyle = Block "(*" "*)",
      emptyLines = True
    }
