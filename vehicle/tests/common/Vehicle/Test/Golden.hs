{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Vehicle.Test.Golden
  ( makeTestTreesFromFile
  , makeTestTreeFromDirectoryRecursive
  )
  where
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Process (CreateProcess (..), shell, readCreateProcessWithExitCode)
import Test.Tasty (TestName, TestTree, testGroup)
import Vehicle.Test.Golden.Extra (SomeOption, someLocalOptions, listFilesRecursive, createDirectoryRecursive)
import Vehicle.Test.Golden.TestSpec (TestOutput (..), TestSpec,
                                     TestSpecs (TestSpecs), readTestSpecsFile, testSpecOptions, testSpecName, readGoldenFiles, testSpecDiffTestOutput, writeGoldenFiles, testSpecNeeds, testSpecRun)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist, copyFile)
import Control.Monad (filterM, forM, forM_)
import System.FilePath ((</>), takeFileName, takeDirectory, makeRelative)
import Data.Functor ((<&>))
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf (printf)
import System.IO.Temp (withSystemTempDirectory)

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: TestName -> FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testGroupLabel testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each .test.json file in the current directory:
  testTreesFromHere <-
    return testDirectoryEntries
      -- Filter directory entries to only test specifications
      >>= filterM (isTestSpecFile . (testDirectory </>))
      -- Make test trees
      >>= traverse (\testSpecFileName ->
            let testSpecFile = testDirectory </> testSpecFileName in
                makeTestTreesFromFile (testDirectory </> testSpecFileName))
      <&> concatMap NonEmpty.toList

  -- Construct test trees for all subdirectories:
  testTreesFromFurther <-
    return testDirectoryEntries
      -- Filter directory entries to only test specifications:
      >>= filterM (doesDirectoryExist . (testDirectory </>))
      -- Make test trees for each subdirectory:
      >>= traverse (\subDirectoryName ->
            let testSubDirectory = testDirectory </> subDirectoryName in
              makeTestTreeFromDirectoryRecursive subDirectoryName testSubDirectory)

  -- Combine all test trees:
  return $ testGroup testGroupLabel (testTreesFromHere <> testTreesFromFurther)

-- | Read a test specification and return a TestTree.
makeTestTreesFromFile :: FilePath -> IO (NonEmpty TestTree)
makeTestTreesFromFile testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  return $ toTestTree testSpecFile <$> testSpecs

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: FilePath -> IO Bool
isTestSpecFile path = (takeFileName path == "test.json" &&) <$> doesFileExist path

-- | Convert a test specifications to a test tree.
toTestTree :: FilePath -> TestSpec -> TestTree
toTestTree testSpecFile testSpec = someLocalOptions testOptions testTree
  where
    testOptions :: [SomeOption]
    testOptions = testSpecOptions testSpec

    testDirectory :: FilePath
    testDirectory = takeDirectory testSpecFile

    testTree :: TestTree
    testTree = goldenTest testName readGolden runTest compareTestOuput updateGolden
      where
        testName         = testSpecName testSpec
        readGolden       = readGoldenFiles testDirectory testSpec
        compareTestOuput = testSpecDiffTestOutput testSpec
        updateGolden     = writeGoldenFiles testDirectory testSpec
        runTest          = do
          -- Create a temporary directory:
          let tempDirectoryNameTemplate = printf "vehicle-test-%s" (testSpecName testSpec)
          withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
            -- Copy over all needed files:
            forM_ (testSpecNeeds testSpec) $ \neededFile -> do
              createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
              copyFile (testDirectory </> neededFile) (tempDirectory </> neededFile)
            -- Run the command in the specified directory:
            let cmdSpec = (shell $ testSpecRun testSpec) {cwd = Just tempDirectory}
            (_exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
            let testOutputStdout = Text.pack stdoutString
            let testOutputStderr = Text.pack stderrString
            testOutputFiles <-
              fmap HashMap.fromList $ do
                filePaths <-
                  fmap (makeRelative tempDirectory)
                    <$> listFilesRecursive tempDirectory
                forM filePaths $ \filePath -> do
                  fileContents <- Text.readFile $ tempDirectory </> filePath
                  return (filePath, fileContents)
            return TestOutput{..}
