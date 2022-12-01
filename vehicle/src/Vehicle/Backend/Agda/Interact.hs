module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Prelude

import Control.Monad.IO.Class (MonadIO)

writeAgdaFile :: MonadIO m => Maybe FilePath -> Doc a -> m ()
writeAgdaFile = writeResultToFile AgdaBackend
