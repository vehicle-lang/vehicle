import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.HUnit
import Vehicle.Frontend.Parse (parseFile)

main :: IO Counts
main = runTestTT . TestList $
  [ TestLabel "parser" parserTests
  ]

-- * Test parser for Vehicle Frontend

parserTests :: Test
parserTests = TestList
  [ TestLabel "examples" (runFileTests pFileTests)
  ]

pFileTests :: [FilePath]
pFileTests =
  [ "examples/andGate.vcl"
  , "examples/mnist.vcl"
  , "examples/shortestPath.vcl"
  ]

-- ** Helper functions

runFileTests :: [FilePath] -> Test
runFileTests files = TestList [ TestLabel file (TestCase (void $ parseFile file)) | file <- files ]

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
