module Vehicle.Backend.JSON.Interact
  ( writeJSONFile,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, unpack)
import Vehicle.Backend.Prelude
import Vehicle.Prelude (MonadLogger, Pretty (..))

writeJSONFile ::
  (MonadLogger m, MonadIO m) =>
  Maybe FilePath ->
  Text ->
  m ()
writeJSONFile outputFile json = writeResultToFile Nothing outputFile (pretty $ unpack json)
