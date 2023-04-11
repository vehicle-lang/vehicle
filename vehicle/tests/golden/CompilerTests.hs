module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultMain)
import Vehicle.Test.Golden (TestFilter, makeTestTreeFromDirectoryRecursive)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive "Compiler" isCompilerTest testDirectory
  defaultMain testTree

isCompilerTest :: TestFilter
isCompilerTest s = s /= "MarabouVerify"
