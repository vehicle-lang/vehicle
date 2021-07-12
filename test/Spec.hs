{-# LANGUAGE ImportQualifiedPost #-}

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Test.HUnit
import System.Exit (exitFailure)

import Vehicle

main :: IO Counts
main = runTestTT . TestList $
  [ TestLabel "agda" agdaTests
  --, TestLabel "AA" compileTests
  ]

-- * Test parser for Vehicle Frontend

fileTests :: [FilePath]
fileTests =
  [ "examples/simple.vcl"
  -- , "examples/andGate.vcl"
  -- , "examples/mnist.vcl"
  --, "examples/shortestPath.vcl"
  ]
{-
parserTests :: Test
parserTests = TestList
  [ TestLabel "examples" (runParseFileTests fileTests)
  ]
-}

agdaTests :: Test
agdaTests = TestList
  [ TestLabel "examples" (runAgdaFileTests fileTests)
  ]

-- ** Helper functions

runFileTests :: (String -> IO a) -> [FilePath] -> Test
runFileTests test files = TestList [ TestLabel file (TestCase (void $ test file)) | file <- files ]

-- runParseFileTests :: [FilePath] -> Test
-- runParseFileTests = runFileTests parseFile

runAgdaFileTests :: [FilePath] -> Test
runAgdaFileTests = runFileTests compileToAgda

compileToAgda :: FilePath -> IO ()
compileToAgda filePath = run $ Options
  { showHelp     = False
  , showVersion  = False
  , inputLang    = Frontend
  , inputFile    = Just filePath
  , outputTarget = Just $ ITP Agda
  , outputFile   = Nothing
  }
