module Vehicle.Backend.VNNLib.Interact
  ( writeVNNLibQueryFiles
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Backend.VNNLib.Core
import Control.Monad (forM_)

writeVNNLibQueryFiles :: Maybe FilePath -> [VNNLibProperty] -> IO ()
writeVNNLibQueryFiles filepath properties = forM_ properties $ \property -> do
  writeResultToFile VNNLibBackend filepath (doc property)