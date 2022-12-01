module Vehicle.Prelude.IO
  ( vehicleFileExtension
  , vehicleObjectFileExtension
  , vehicleProofCacheFileExtension
  , removeFileIfExists
  , fatalError
  , programOutput
  ) where

import Control.Exception (catch, throwIO)
-- import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Prettyprinter (Doc)
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)

vehicleFileExtension :: String
vehicleFileExtension = ".vcl"

vehicleObjectFileExtension :: String
vehicleObjectFileExtension = vehicleFileExtension <> "o"

vehicleProofCacheFileExtension :: String
vehicleProofCacheFileExtension = vehicleFileExtension <> "p"

--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: MonadIO m => Doc a -> m b
fatalError message = liftIO $ do
  hPrint stderr message
  exitFailure

programOutput :: MonadIO m => Doc a -> m ()
programOutput message = liftIO $ print message
