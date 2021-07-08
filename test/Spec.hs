import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.HUnit
import System.Exit (exitFailure)

import Vehicle.Frontend.Parse (parseFile)
import Vehicle.Core.AST as VC
import Vehicle.Frontend.AST as VF
import Vehicle.Error
import Vehicle.Core.Compile (compile)
import Vehicle.Frontend.Elaborate

main :: IO Counts
main = runTestTT . TestList $
  [ TestLabel "parser" parserTests
  , TestLabel "compile" compileTests
  ]

-- * Test parser for Vehicle Frontend

fileTests :: [FilePath]
fileTests =
  [ "examples/andGate.vcl"
  , "examples/mnist.vcl"
  , "examples/shortestPath.vcl"
  ]

parserTests :: Test
parserTests = TestList
  [ TestLabel "examples" (runParseFileTests fileTests)
  ]

compileTests :: Test
compileTests = TestList
  [ TestLabel "examples" (runCompileFileTests fileTests)
  ]

-- ** Helper functions

runParseFileTests :: [FilePath] -> Test
runParseFileTests files = TestList [ TestLabel file (TestCase (void $ parseFile file)) | file <- files ]

runCompileFileTests :: [FilePath] -> Test
runCompileFileTests files = TestList [ TestLabel file (TestCase (void $ parseAndCompile file)) | file <- files]

parseAndCompile :: FilePath -> IO VC.OutputProg
parseAndCompile filePath = do
  prog <- parseFile filePath
  coreProg <- fromEitherIO $ runElab (elab prog)
  fromEitherIO $ compile coreProg

fromEitherIO :: MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print (details err); exitFailure
fromEitherIO (Right x)  = return x

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
