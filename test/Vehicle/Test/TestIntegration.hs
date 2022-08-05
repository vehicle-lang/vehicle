
import Test.Tasty
import Control.Monad.Reader (runReader)
import GHC.IO.Encoding
import System.Environment

import Vehicle.Test.CompileMode.Golden qualified as Golden
import Vehicle.Test.Utils (MonadTest, filepathTests)
import System.Directory (findExecutable)

-- Can't figure out how to get this passed in via the command-line *sadness*
testLogLevel :: Int
testLogLevel = 0

main :: IO ()
main = do
  vehicleExecutablePath <- findExecutable "vehicle"
  case vehicleExecutablePath of
    Nothing -> error "Please install Vehicle using `cabal install` before running the \"integration test suite\""
    Just{}  -> return ()

  print "WARNING: these tests use the version of Vehicle currently installed. \
      \ Run `cabal install` to install the latest version."

  defaultMain tests

tests :: TestTree
tests = do
  testGroup "IntegrationTests"
    [ Golden.integrationTests
    ]