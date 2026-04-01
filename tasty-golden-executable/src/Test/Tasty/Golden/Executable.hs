module Test.Tasty.Golden.Executable
  ( SomeOption (..),
    AllowlistExternals (..),
    External (..),
    Ignore (..),
    IgnoreFiles (..),
    makeTestTreeFromFile,
    makeTestTreeFromDirectoryRecursive,
  )
where

import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NonEmpty
import General.Extra.Option (SomeOption (..), someLocalOptions)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Tasty (testGroup)
import Test.Tasty.Golden.Executable.Runner ()
import Test.Tasty.Golden.Executable.TestSpec (TestSpec (..))
import Test.Tasty.Golden.Executable.TestSpec.External (AllowlistExternals (..), External (..))
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore (..), IgnoreFiles (..))
import Test.Tasty.Golden.Executable.TestSpecs (TestSpecs (..), readTestSpecsFile, testSpecsFileName)
import Test.Tasty.Providers (TestName, TestTree, singleTest)

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: FilePath -> IO Bool
isTestSpecFile path = (takeFileName path == testSpecsFileName &&) <$> doesFileExist path

-- | Read a test specification and return a TestTree.
makeTestTreeFromFile :: [SomeOption] -> FilePath -> IO [TestTree]
makeTestTreeFromFile testOptions testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  return
    [ someLocalOptions testOptions (singleTest (testSpecName testSpec) testSpec)
      | testSpec <- NonEmpty.toList testSpecs
    ]

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: [SomeOption] -> TestName -> FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testOptions testGroupLabel testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each test.json file in the current directory:
  testTreesFromHere <-
    -- Filter directory entries to only test specifications
    filterM (isTestSpecFile . (testDirectory </>)) testDirectoryEntries
      -- Make test trees
      >>= traverse
        ( \testSpecFileName ->
            makeTestTreeFromFile testOptions (testDirectory </> testSpecFileName)
        )
      <&> concat

  -- Construct test trees for all subdirectories:
  testTreesFromFurther <-
    -- Filter directory entries to only test specifications:
    filterM (doesDirectoryExist . (testDirectory </>)) testDirectoryEntries
      -- Make test trees for each subdirectory:
      >>= traverse
        ( \subDirectoryName ->
            let testSubDirectory = testDirectory </> subDirectoryName
             in makeTestTreeFromDirectoryRecursive testOptions subDirectoryName testSubDirectory
        )

  -- Combine all test trees:
  let result = testGroup testGroupLabel (testTreesFromHere <> testTreesFromFurther)

  return result
