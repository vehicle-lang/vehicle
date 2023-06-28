module Vehicle.Test.Golden
  ( makeTestTreesFromFile,
    makeTestTreeFromDirectoryRecursive,
    SomeOption (..),
    ignoreFileOption,
    ignoreFileOptionIngredient,
    ignoreLineOption,
    ignoreLineOptionIngredient,
    externalOptionIngredient,
  )
where

import Control.Exception (throw)
import Control.Monad (filterM, forM, when)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory
  ( copyFile,
    createDirectory,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getDirectoryContents,
    listDirectory,
  )
import System.FilePath
  ( makeRelative,
    takeDirectory,
    takeFileName,
    (</>),
  )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty (TestName, TestTree, askOption, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf (printf)
import Vehicle.Test.Golden.Extra
  ( SomeOption (..),
    createDirectoryRecursive,
    listFilesRecursive,
    someLocalOptions,
  )
import Vehicle.Test.Golden.TestSpec
  ( TestOutput (..),
    TestSpec,
    TestSpecs (TestSpecs),
    readGoldenFiles,
    readTestSpecsFile,
    testSpecExternal,
    testSpecIgnore,
    testSpecIgnoreTestOutput,
    testSpecIsEnabled,
    testSpecName,
    testSpecNeeds,
    testSpecOptions,
    testSpecRun,
    writeGoldenFiles,
  )
import Vehicle.Test.Golden.TestSpec.External (ExternalOnlyOption (..), ExternalOption (..), externalOptionIngredient)
import Vehicle.Test.Golden.TestSpec.Ignore (Ignore (..), IgnoreFile, IgnoreFileOption (..), IgnoreLine, IgnoreLineOption (..), ignoreFileOption, ignoreFileOptionIngredient, ignoreLineOption, ignoreLineOptionIngredient)
import Vehicle.Test.Golden.TestSpec.Ignore qualified as Ignore

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: [SomeOption] -> TestName -> FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testOptions testGroupLabel testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each .test.json file in the current directory:
  testTreesFromHere <-
    -- Filter directory entries to only test specifications
    filterM (isTestSpecFile . (testDirectory </>)) testDirectoryEntries
      -- Make test trees
      >>= traverse
        ( \testSpecFileName ->
            makeTestTreesFromFile testOptions (testDirectory </> testSpecFileName)
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

-- | Read a test specification and return a TestTree.
makeTestTreesFromFile :: [SomeOption] -> FilePath -> IO [TestTree]
makeTestTreesFromFile testOptions testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  let enabledTestSpec = filter testSpecIsEnabled $ NonEmpty.toList testSpecs
  return $ toTestTree testOptions testSpecFile <$> enabledTestSpec

-- | Test whether all required external dependencies are allowed.
testSpecExternalAllowed :: ExternalOnlyOption -> ExternalOption -> TestSpec -> Bool
testSpecExternalAllowed (ExternalOnlyOption externalOnly) (ExternalOption allowedExternals) testSpec
  | externalOnly = neededExternals == allowedExternals
  | otherwise = neededExternals `Set.isSubsetOf` allowedExternals
  where
    neededExternals = Set.fromList (testSpecExternal testSpec)

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: FilePath -> IO Bool
isTestSpecFile path = (takeFileName path == "test.json" &&) <$> doesFileExist path

-- | Convert a test specifications to a test tree.
toTestTree :: [SomeOption] -> FilePath -> TestSpec -> TestTree
toTestTree testOptions testSpecFile testSpec =
  someLocalOptions (testOptions <> testSpecOptions testSpec) $
    askOption $ \(IgnoreLineOption optionIgnoreLines) ->
      askOption $ \(IgnoreFileOption optionIgnoreFiles) ->
        let testSpecIgnoreLines :: [IgnoreLine]
            testSpecIgnoreLines = maybe [] ignoreLines (testSpecIgnore testSpec)
            testSpecIgnoreFiles :: [IgnoreFile]
            testSpecIgnoreFiles = maybe [] ignoreFiles (testSpecIgnore testSpec)
            updatedTestSpec :: TestSpec
            updatedTestSpec =
              testSpec
                { testSpecIgnore =
                    Just
                      Ignore
                        { ignoreLines = testSpecIgnoreLines <> optionIgnoreLines,
                          ignoreFiles = testSpecIgnoreFiles <> optionIgnoreFiles
                        }
                }
         in toTestTreeHelper testSpecFile updatedTestSpec

toTestTreeHelper :: FilePath -> TestSpec -> TestTree
toTestTreeHelper testSpecFile testSpec = testTree
  where
    testDirectory :: FilePath
    testDirectory = takeDirectory testSpecFile

    testTree :: TestTree
    testTree =
      askOption $ \external ->
        askOption $ \externalOnly ->
          if testSpecExternalAllowed externalOnly external testSpec
            then goldenTest testName readGolden runTest compareTestOutput updateGolden
            else testGroup testName []
      where
        testName = testSpecName testSpec
        readGolden = readGoldenFiles testDirectory testSpec
        compareTestOutput = testSpecIgnoreTestOutput testSpec
        updateGolden = writeGoldenFiles testDirectory testSpec
        runTest = do
          -- Create a temporary directory:
          let tempDirectoryNameTemplate = printf "vehicle-test-%s" (testSpecName testSpec)
          withSystemTempDirectory tempDirectoryNameTemplate $ \tempDirectory -> do
            -- Copy over all needed files:
            let neededFiles = Set.fromList $ testSpecNeeds testSpec
            allCopiedFiles <- forM (Set.toList neededFiles) $ \neededFile -> do
              createDirectoryRecursive (tempDirectory </> takeDirectory neededFile)
              let originalFilePath = testDirectory </> neededFile
              let finalFilePath = tempDirectory </> neededFile
              copyRecursively originalFilePath finalFilePath
            let copiedFiles = Set.fromList (drop (length tempDirectory + 1) <$> concat allCopiedFiles)

            -- Run the command in the specified directory:
            let cmdSpec = (shell $ testSpecRun testSpec) {cwd = Just tempDirectory}
            (_exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""

            -- Gather the outputs
            let testOutputStdout = Text.pack stdoutString
            let testOutputStderr = Text.pack stderrString
            let ignoreFiles = maybe [] Ignore.ignoreFiles (testSpecIgnore testSpec)
            testOutputFiles <- HashMap.fromList <$> getTestOutputFiles ignoreFiles copiedFiles tempDirectory

            return TestOutput {..}

copyRecursively :: FilePath -> FilePath -> IO [FilePath]
copyRecursively src dst = do
  whenM (not <$> doesPathExist src) $
    throw (userError $ "test source file '" <> src <> "' does not exist")

  isDirectory <- doesDirectoryExist src
  if isDirectory
    then do
      createDirectory dst
      content <- getDirectoryContents src
      let xs = filter (`notElem` ([".", ".."] :: [FilePath])) content
      copiedFiles <- forM xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        copyRecursively srcPath dstPath
      return $ concat copiedFiles
    else do
      copyFile src dst
      return [dst]
  where
    whenM s r = s >>= flip when r

getTestOutputFiles :: [IgnoreFile] -> Set FilePath -> FilePath -> IO [(FilePath, Text)]
getTestOutputFiles ignoreFilePatterns copiedFiles tempDirectory = do
  absoluteFilePaths <- listFilesRecursive tempDirectory
  let shouldIgnore filePath = any (`Ignore.matchFile` filePath) ignoreFilePatterns
  let outputFilePaths =
        absoluteFilePaths
          <&> makeRelative tempDirectory
          & filter (\filePath -> not $ Set.member filePath copiedFiles || shouldIgnore filePath)
  forM outputFilePaths $ \filePath -> do
    fileContents <- Text.readFile $ tempDirectory </> filePath
    return (filePath, fileContents)
