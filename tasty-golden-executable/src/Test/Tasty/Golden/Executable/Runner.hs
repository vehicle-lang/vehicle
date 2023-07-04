{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Tasty.Golden.Executable.Runner where

import Control.Exception (Exception, throw)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch (..), MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..), execWriterT)
import Data.Algorithm.Diff (getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO qualified as LazyIO
import Data.Traversable (for)
import General.Extra (boolToMaybe)
import General.Extra.Diff (isBoth, mapDiff)
import General.Extra.File (createDirectoryRecursive)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.Exit qualified as ExitCode
import System.FilePath (isAbsolute, isExtensionOf, stripExtension, (<.>), (</>))
import System.IO (IOMode (..), hFileSize, withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern, addExtension, glob, match)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (TextPattern, strikeOut)
import Test.Tasty.Providers (TestName, testFailed)
import Test.Tasty.Runners (Result)
import Text.Printf (printf)

data TestEnvironment = TestEnvironment
  { testName :: TestName,
    testNeeds :: [FilePath],
    testDirectory :: FilePath,
    tempDirectory :: FilePath
  }

-- | Monad for running tests in an isolated environment.
newtype TestT m a = TestT {unTest :: StateT TestEnvironment m a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

-- | Alias for @'TestT' 'IO' a@.
type TestIO a = TestT IO a

testEnvironment :: (Monad m) => TestT m TestEnvironment
testEnvironment = TestT get

-- | Raised when the needed file for a test is not found.
newtype NeededFilesError = NeededFilesError {neededFilesNotFound :: [FilePath]}
  deriving (Show, Semigroup)

neededFileNotFound :: FilePath -> NeededFilesError
neededFileNotFound file = NeededFilesError [file]

instance Exception NeededFilesError

handleNeededFilesError :: NeededFilesError -> IO Result
handleNeededFilesError NeededFilesError {..} =
  return $
    testFailed $
      printf "Could not find needed files: %s" $
        intercalate ", " (show <$> neededFilesNotFound)

-- | Raised when the golden file for a test is not found.
newtype GoldenFilesError = GoldenFilesError {goldenFilesNotFound :: [FilePattern]}
  deriving (Show, Semigroup)

goldenFileNotFound :: FilePattern -> GoldenFilesError
goldenFileNotFound filePattern = GoldenFilesError [filePattern]

instance Exception GoldenFilesError

handleGoldenFilesError :: GoldenFilesError -> IO Result
handleGoldenFilesError GoldenFilesError {..} =
  return $
    testFailed $
      printf "Could not find golden files: %s" $
        intercalate ", " (show <$> goldenFilesNotFound)

-- | Raised when the test run does not produce an expected file or does not expect a produced file,
--   or when the produced file differs from the expected file.
data ProducedFilesError = ProducedFilesError
  { expectedFilesNotProduced :: [FilePath],
    producedFilesNotExpected :: [FilePath],
    producedAndExpectedDiffs :: Map FilePath Diff
  }
  deriving (Show)

instance Semigroup ProducedFilesError where
  (<>) :: ProducedFilesError -> ProducedFilesError -> ProducedFilesError
  e1 <> e2 =
    ProducedFilesError
      { expectedFilesNotProduced = expectedFilesNotProduced e1 <> expectedFilesNotProduced e2,
        producedFilesNotExpected = producedFilesNotExpected e1 <> producedFilesNotExpected e2,
        producedAndExpectedDiffs = producedAndExpectedDiffs e1 <> producedAndExpectedDiffs e2
      }

expectedFileNotProduced :: FilePath -> ProducedFilesError
expectedFileNotProduced file = ProducedFilesError [file] [] Map.empty

producedFileNotExpected :: FilePath -> ProducedFilesError
producedFileNotExpected file = ProducedFilesError [] [file] Map.empty

producedAndExpectedDiffer :: FilePath -> Diff -> ProducedFilesError
producedAndExpectedDiffer file diff = ProducedFilesError [] [] (Map.singleton file diff)

instance Exception ProducedFilesError

handleProducedFilesError :: ProducedFilesError -> IO Result
handleProducedFilesError ProducedFilesError {..} = do
  return . testFailed . unlines . catMaybes $
    [ boolToMaybe (null expectedFilesNotProduced) $
        printf "Did not produce expected files: %s" $
          intercalate ", " (show <$> expectedFilesNotProduced),
      boolToMaybe (null expectedFilesNotProduced) $
        printf "Did not expect produced files: %s" $
          intercalate ", " (show <$> producedFilesNotExpected),
      boolToMaybe (null producedAndExpectedDiffs) $
        unlines . flip foldMap (Map.assocs producedAndExpectedDiffs) $ \(file, diff) ->
          return $ printf "Expected and produced files differ for %s:\n%s" file (show diff)
    ]

data Diff = Diff {prettyDiff :: String} | NoDiff
  deriving (Show)

instance Exception Diff

-- | Raised when the test run exits with a non-zero exit code.
newtype ExitFailure = ExitFailure Int
  deriving (Show)

instance Exception ExitFailure

handleExitFailure :: ExitFailure -> IO Result
handleExitFailure (ExitFailure code) =
  return $
    testFailed $
      printf "Test terminated with exit code %d" code

-- | Create a temporary directory to execute the test.
runTestIO :: FilePath -> TestName -> TestIO r -> IO r
runTestIO testDirectory testName (TestT testIO) = do
  withSystemTempDirectory testName $ \tempDirectory -> do
    createDirectoryRecursive tempDirectory
    evalStateT testIO TestEnvironment {testNeeds = [], ..}

-- | Copy the needed files over to the temporary directory.
copyTestNeeds :: [FilePath] -> TestIO ()
copyTestNeeds neededFiles = TestT $ do
  TestEnvironment {..} <- get
  -- Register the 'neededFiles' with the environment.
  put TestEnvironment {testNeeds = neededFiles <> testNeeds, ..}
  lift $ do
    maybeError <- execWriterT $ do
      for_ neededFiles $ \neededFile -> do
        let neededFileTarget = tempDirectory </> neededFile
        -- Try to find the needed file in the test directory
        let neededFileSource = testDirectory </> neededFile
        neededFileExists <- lift $ doesFileExist (testDirectory </> neededFile)
        -- Try to find the needed file in the test directory as a golden file
        let neededFileSourceAsGolden = neededFileSource <.> "golden"
        neededFileExistsAsGolden <- lift $ doesFileExist neededFileSourceAsGolden
        -- If either is successful, copy the file. Otherwise, throw an error.
        if
          | neededFileExists -> lift $ copyFile neededFileSource neededFileTarget
          | neededFileExistsAsGolden -> lift $ copyFile neededFileSourceAsGolden neededFileTarget
          | otherwise -> tell $ Just $ neededFileNotFound neededFile
    -- If errors were raised, throw them.
    maybe (return ()) throw maybeError

-- | Run the test command in the temporary directory.
runTestRun :: String -> TestIO (Lazy.Text, Lazy.Text)
runTestRun cmd = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    -- Run the test command
    let cmdSpec = (shell cmd) {cwd = Just tempDirectory}
    (exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
    -- If the exit code is zero, return the stdout and stderr. Otherwise, throw an error.
    case exitCode of
      ExitCode.ExitSuccess -> return (Lazy.pack stdoutString, Lazy.pack stderrString)
      ExitCode.ExitFailure code -> throw $ ExitFailure code

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStdout :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> TestIO ()
diffStdout maybeLooseEq actual = do
  golden <- readGoldenStdout
  lift $ diffText (shortCircuitWithEq maybeLooseEq) golden actual

-- | Read the golden file for the standard output.
readGoldenStdout :: TestIO Lazy.Text
readGoldenStdout = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stdoutGoldenFile = testDirectory </> testName <.> "out.golden"
    stdoutGoldenFileExists <- doesFileExist stdoutGoldenFile
    if stdoutGoldenFileExists
      then LazyIO.readFile stdoutGoldenFile
      else return Lazy.empty

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStderr :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> TestIO ()
diffStderr maybeLooseEq actual = do
  golden <- readGoldenStderr
  lift $ diffText (shortCircuitWithEq maybeLooseEq) golden actual

-- | Read the golden file for the standard error.
readGoldenStderr :: TestIO Lazy.Text
readGoldenStderr = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stderrGoldenFile = testDirectory </> testName <.> "err.golden"
    stderrGoldenFileExists <- doesFileExist stderrGoldenFile
    if stderrGoldenFileExists
      then LazyIO.readFile stderrGoldenFile
      else return ""

-- | Find the files produced by the test.
diffTestProduced :: Maybe (Text -> Text -> Bool) -> [FilePattern] -> [FilePattern] -> TestIO ()
diffTestProduced maybeLooseEq testProduces testIgnores = do
  TestEnvironment {testDirectory, tempDirectory} <- testEnvironment
  let shortCircuitLooseEq = shortCircuitWithEq maybeLooseEq
  -- Find the golden and actual files:
  goldenFiles <- findTestProducesGolden testProduces
  actualFiles <- findTestProducesActual testIgnores
  maybeError <- execWriterT $ do
    -- Assert that all golden files end with .golden:
    for_ goldenFiles $ \goldenFile ->
      unless ("golden" `isExtensionOf` goldenFile) $
        fail $
          printf "found golden file without .golden extension: %s" goldenFile
    -- Compute sets of files:
    let goldenFileSet = Set.fromList (mapMaybe (stripExtension "golden") goldenFiles)
    let actualFileSet = Set.fromList actualFiles
    -- Test for files which were expected but not produced:
    let expectedFilesNotProduced = Set.toAscList $ Set.difference goldenFileSet actualFileSet
    for_ expectedFilesNotProduced $ tell . Just . expectedFileNotProduced
    -- Test for files which were produced but not expected:
    let producedFilesNotExpected = Set.toAscList $ Set.difference goldenFileSet actualFileSet
    for_ producedFilesNotExpected $ tell . Just . producedFileNotExpected
    -- Diff the files which were produced and expected:
    for_ (Set.toAscList $ Set.intersection goldenFileSet actualFileSet) $ \file -> do
      let goldenFile = testDirectory </> file <.> "golden"
      let actualFile = tempDirectory </> file
      catch (lift $ lift $ diffFile shortCircuitLooseEq goldenFile actualFile) $ \diff ->
        tell $ Just $ producedAndExpectedDiffer actualFile diff

  -- If errors were raised, throw them.
  maybe (return ()) throw maybeError

-- | Find the actual files produced by the test command.
findTestProducesActual :: [FilePattern] -> TestIO [FilePath]
findTestProducesActual testIgnoreFiles = TestT $ do
  TestEnvironment {..} <- get
  tempFiles <- lift $ listDirectory tempDirectory
  -- Assert that the file paths are relative.
  for_ tempFiles $ \file -> do
    when (isAbsolute file) $
      fail $
        printf "glob: found absolute path %s" file
  -- Filter out the files copied by 'copyTestNeeds' or matched by 'testIgnoreFiles'.
  let tempFilesProducedAndNotIgnored =
        [ file
          | file <- tempFiles,
            file `notElem` testNeeds,
            and [not (ignore `match` file) | ignore <- testIgnoreFiles]
        ]
  return tempFilesProducedAndNotIgnored

-- | Find the golden files for the files produced by the test command.
findTestProducesGolden :: [FilePattern] -> TestIO [FilePath]
findTestProducesGolden testProduces = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    (filesByPattern, maybeError) <-
      runWriterT $ do
        filesByPattern <-
          for testProduces $ \testProduce -> do
            filesForPattern <- lift $ glob [addExtension testProduce "golden"] testDirectory
            -- If the pattern does not result in any matches, throw an error.
            when (null filesForPattern) $
              tell $
                Just $
                  goldenFileNotFound testProduce
            -- Assert that the file paths are relative.
            for_ filesForPattern $ \file ->
              when (isAbsolute file) $
                fail $
                  printf "glob: found absolute path %s" file
            return filesForPattern
        return $ concat filesByPattern
    -- If errors were raised, throw them.
    maybe (return filesByPattern) throw maybeError

fileSizeCutOff :: Integer
fileSizeCutOff = 1000

-- | Compare two files.
--
-- NOTE: The loose equality must extend equality.
diffFile :: (Text -> Text -> Bool) -> FilePath -> FilePath -> IO ()
diffFile eq golden actual = do
  withFile golden ReadMode $ \goldenHandle -> do
    goldenSize <- hFileSize goldenHandle
    goldenContents <- LazyIO.hGetContents goldenHandle
    withFile actual ReadMode $ \actualHandle -> do
      actualSize <- hFileSize actualHandle
      actualContents <- LazyIO.hGetContents actualHandle
      if max goldenSize actualSize < fileSizeCutOff
        then diffText eq goldenContents actualContents
        else when (goldenContents /= actualContents) $ throw NoDiff

-- | Compare two texts.
--
-- NOTE: The loose equality must extend equality.
diffText :: (Text -> Text -> Bool) -> Lazy.Text -> Lazy.Text -> IO ()
diffText eq golden actual = do
  -- Lazily split the golden and actual texts into lines
  let goldenLines = Lazy.toStrict <$> Lazy.lines golden
  let actualLines = Lazy.toStrict <$> Lazy.lines actual
  -- Compute the diff
  let groupedDiff = getGroupedDiffBy eq goldenLines actualLines
  -- If both files are the same, the diff should be just "Both":
  unless (all isBoth groupedDiff) $
    throw $
      Diff (ppDiff $ mapDiff (Text.unpack <$>) <$> groupedDiff)
  return ()

-- | Make a loose equality which ignores text matching the provided text patterns.
makeLooseEq :: [TextPattern] -> Text -> Text -> Bool
makeLooseEq patterns golden actual = strikeOutAll golden == strikeOutAll actual
  where
    strikeOutAll line = foldr strikeOut line patterns

-- | Make a loose equality which short-circuits using equality.
shortCircuitWithEq :: (Eq a) => Maybe (a -> a -> Bool) -> a -> a -> Bool
shortCircuitWithEq maybeEq x y = x == y || maybe False (\eq -> x `eq` y) maybeEq
