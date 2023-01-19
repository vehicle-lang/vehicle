module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Vehicle.Backend.Prelude
import Vehicle.Prelude

writeAgdaFile :: MonadIO m => Maybe FilePath -> Doc a -> m ()
writeAgdaFile = writeResultToFile AgdaBackend
