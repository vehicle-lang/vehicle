
import Control.Monad.Reader (runReader)
import GHC.IO.Encoding
import System.Environment
import Test.Tasty

import Vehicle.Prelude
import Vehicle.Test.CheckMode qualified as CheckMode (functionalityTests)
import Vehicle.Test.CompileMode qualified as CompileMode (functionalityTests)
import Vehicle.Test.Utils (MonadTest, filepathTests)

-- Can't figure out how to get this passed in via the command-line *sadness*
testLogLevel :: LoggingLevel
testLogLevel =
  NoDetail
  -- MaxDetail

timeOutSeconds :: Integer
timeOutSeconds = 1000

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain (runReader tests testLogLevel)

tests :: MonadTest m => m TestTree
tests = localOption (mkTimeout (timeOutSeconds * 1000000)) . testGroup "Tests" <$> sequence
    [ CompileMode.functionalityTests
    , return CheckMode.functionalityTests
    , return miscTests
    ]

miscTests :: TestTree
miscTests = testGroup "Misc"
  [ filepathTests
  ]
