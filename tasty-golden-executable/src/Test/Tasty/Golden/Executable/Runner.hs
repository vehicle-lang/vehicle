{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Tasty.Golden.Executable.Runner where

import Control.Exception (throw)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch (..), MonadMask, MonadThrow, handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Class (modify)
import Control.Monad.State.Strict (MonadState (..), StateT (..), evalStateT, execStateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.State.Strict qualified as Strict (liftListen, liftPass)
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..), execWriterT)
import Data.Algorithm.Diff (getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)
import Data.Foldable (Foldable (..), for_)
import Data.Functor ((<&>))
import Data.List qualified as List (findIndices, splitAt, uncons)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.IO qualified as LazyIO
import Data.Traversable (for)
import General.Extra.Diff (isBoth, mapDiff)
import General.Extra.File (createDirectoryRecursive, listFilesRecursive, writeFileChanged)
import General.Extra.NonEmpty qualified as NonEmpty (appendList, prependList, singleton)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (isAbsolute, isExtensionOf, makeRelative, stripExtension, takeDirectory, takeExtension, (<.>), (</>))
import System.IO (IOMode (..), hFileSize, withFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), readCreateProcessWithExitCode, shell)
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.Error
import Test.Tasty.Golden.Executable.TestSpec (TestSpec (..))
import Test.Tasty.Golden.Executable.TestSpec.Accept (Accept (..))
import Test.Tasty.Golden.Executable.TestSpec.External (AllowlistExternals (..), External)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern, addExtension, asLiteral, glob, match)
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore (..), IgnoreFiles (..), IgnoreLines (..))
import Test.Tasty.Golden.Executable.TestSpec.SizeOnly (SizeOnlyExtensions, toSizeOnlyExtensionsSet)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (strikeOut)
import Test.Tasty.Golden.Executable.TestSpecs (TestSpecs (..), readTestSpecsFile, testSpecsFileName, writeTestSpecsFile)
import Test.Tasty.Options (OptionDescription (..), OptionSet, lookupOption)
import Test.Tasty.Providers (IsTest (..), Result, testPassed)
import Test.Tasty.Runners (Progress, Result (..))
import Text.Printf (printf)

instance IsTest TestSpec where
  run :: OptionSet -> TestSpec -> (Progress -> IO ()) -> IO Result
  run options testSpec@(TestSpec {testSpecIgnore = Ignore {..}, ..}) _progress =
    if not $ isEnabled testSpec
      then return $ testSkip "disabled"
      else do
        let missingExternals = checkExternals (lookupOption options) testSpec
        if not $ null missingExternals
          then return $ testSkip $ "missing externals: " <> show missingExternals
          else do
            -- Create loose equality based on the ignore options
            let maybeLooseEq
                  | ignoreLines == mempty = Nothing
                  | otherwise = Just $ makeLooseEq (ignoreLines <> lookupOption options)
            -- Create test environment
            runTestIO testSpecDirectory testSpecName $ do
              -- Copy needs to test environment
              copyTestNeeds testSpecNeeds
              -- Run test command
              (stdout, stderr) <- runTestRun testSpecRun
              -- Check if --accept was passed, and act accordingly:
              if unAccept (lookupOption options)
                then do
                  -- Update .golden file for stderr
                  acceptStderr stderr
                  -- Update .golden file for stdout
                  acceptStdout stdout
                  -- Update produced .golden files
                  acceptTestProduced testSpecProduces (ignoreFiles <> lookupOption options)
                else do
                  maybeError <- execWriterT $ do
                    -- Diff stderr
                    diffStderr maybeLooseEq stderr
                    -- Diff stdout
                    diffStdout maybeLooseEq stdout
                    -- Diff produced files
                    diffTestProduced maybeLooseEq testSpecProduces (ignoreFiles <> lookupOption options) (lookupOption options)

                  maybe (return ()) throw maybeError

  testOptions :: Tagged TestSpec [OptionDescription]
  testOptions =
    return
      [ Option (Proxy :: Proxy Accept),
        Option (Proxy :: Proxy AllowlistExternals),
        Option (Proxy :: Proxy SizeOnlyExtensions),
        Option (Proxy :: Proxy IgnoreFiles),
        Option (Proxy :: Proxy IgnoreLines)
      ]

-- | 'Result' of a skipped test.
testSkip :: String -> Result
testSkip reason = (testPassed "") {resultShortDescription = "SKIP (" <> reason <> ")"}

-- | Whether or not the required externals are allowed.
checkExternals :: AllowlistExternals -> TestSpec -> [External]
checkExternals (AllowlistExternals allowlistExternals) (TestSpec {..}) =
  Set.toList $ Set.difference (Set.fromList testSpecExternals) (Set.fromList allowlistExternals)

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

data TestEnvironment = TestEnvironment
  { testName :: TestName,
    testNeeds :: [FilePath],
    testDirectory :: FilePath,
    tempDirectory :: FilePath
  }

-- | Monad for running tests in an isolated environment.
newtype TestT m a = TestT {unTest :: StateT TestEnvironment m a}
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

instance (MonadWriter w m) => MonadWriter w (TestT m) where
  writer :: (MonadWriter w m) => (a, w) -> TestT m a
  writer = lift . writer
  tell :: (MonadWriter w m) => w -> TestT m ()
  tell = lift . tell
  listen :: (MonadWriter w m) => TestT m a -> TestT m (a, w)
  listen (TestT m) = TestT (Strict.liftListen listen m)
  pass :: (MonadWriter w m) => TestT m (a, w -> w) -> TestT m a
  pass (TestT m) = TestT (Strict.liftPass pass m)

-- | Alias for @'TestT' 'IO' a@.
type TestIO a = TestT IO a

type DiffTestIO a = WriterT (Maybe ProducedFilesError) (TestT IO) a

getTestEnvironment :: (Monad m) => TestT m TestEnvironment
getTestEnvironment = TestT get

putTestEnvironment :: (Monad m) => TestEnvironment -> TestT m ()
putTestEnvironment testEnvironment = TestT (put testEnvironment)

-- | Create a temporary directory to execute the test.
runTestIO :: FilePath -> TestName -> TestIO () -> IO Result
runTestIO testDirectory testName (TestT testIO) = do
  handle handleNeededFilesError $
    handle handleGoldenFilesNotFoundError $
      handle handleProducedFilesError $
        handle handleExitFailure $
          withSystemTempDirectory testName $ \tempDirectory -> do
            createDirectoryRecursive tempDirectory
            evalStateT testIO TestEnvironment {testNeeds = [], ..}
            return $ testPassed mempty

-- | Copy the needed files over to the temporary directory.
copyTestNeeds :: [FilePath] -> TestIO ()
copyTestNeeds neededFiles = do
  TestEnvironment {..} <- getTestEnvironment

  neededFileTargets <- lift $ do
    for neededFiles $ \neededFile -> do
      let neededFileTarget = removeParent neededFile
      let neededFileTargetAbsolute = tempDirectory </> removeParent neededFile
      -- Try to find the needed file in the test directory
      let neededFileSource = testDirectory </> neededFile
      neededFileExists <- doesFileExist (testDirectory </> neededFile)
      -- Try to find the needed file in the test directory as a golden file
      let neededFileSourceAsGolden = neededFileSource <.> "golden"
      neededFileExistsAsGolden <- doesFileExist neededFileSourceAsGolden
      -- If either is successful, copy the file. Otherwise, throw an error.
      if
        | neededFileExists -> copyFile neededFileSource neededFileTargetAbsolute
        | neededFileExistsAsGolden -> copyFile neededFileSourceAsGolden neededFileTargetAbsolute
        | otherwise -> throw $ neededFileNotFound neededFile

      return neededFileTarget

  -- Register the 'neededFiles' with the environment.
  putTestEnvironment TestEnvironment {testNeeds = neededFileTargets <> testNeeds, ..}

-- | Run the test command in the temporary directory.
runTestRun :: String -> TestIO (Lazy.Text, Lazy.Text)
runTestRun cmd = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    -- Run the test command
    let cmdSpec = (shell cmd) {cwd = Just tempDirectory}
    (_exitCode, stdoutString, stderrString) <- readCreateProcessWithExitCode cmdSpec ""
    -- If the exit code is zero, return the stdout and stderr. Otherwise, throw an error.
    -- case exitCode of
    --   ExitCode.ExitSuccess -> return (Lazy.pack stdoutString, Lazy.pack stderrString)
    --   ExitCode.ExitFailure code -> throw $ ExitFailure code
    return (Lazy.pack stdoutString, Lazy.pack stderrString)

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStdout :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> DiffTestIO ()
diffStdout maybeLooseEq actual = do
  golden <- lift readGoldenStdout
  catch (lift $ lift $ diffText (shortCircuitWithEq maybeLooseEq) Nothing golden actual) $ \diff ->
    tell $ Just $ stdoutDiffer diff

-- | Update the standard output golden file.
acceptStdout :: Lazy.Text -> TestIO ()
acceptStdout contents
  | Lazy.null contents = return ()
  | otherwise = writeGoldenStdout contents

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

-- | Write the golden file for the standard output.
writeGoldenStdout :: Lazy.Text -> TestIO ()
writeGoldenStdout contents = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stdoutGoldenFile = testDirectory </> testName <.> "out.golden"
    LazyIO.writeFile stdoutGoldenFile contents

-- | Compare the standard output to the golden file.
--
-- NOTE: The loose equality must extend equality.
diffStderr :: Maybe (Text -> Text -> Bool) -> Lazy.Text -> DiffTestIO ()
diffStderr maybeLooseEq actual = do
  golden <- lift readGoldenStderr
  catch (lift $ lift $ diffText (shortCircuitWithEq maybeLooseEq) Nothing golden actual) $ \diff ->
    tell $ Just $ stderrDiffer diff

-- | Update the standard error golden file.
acceptStderr :: Lazy.Text -> TestIO ()
acceptStderr contents
  | Lazy.null contents = return ()
  | otherwise = writeGoldenStderr contents

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

-- | Write the golden file for the standard error.
writeGoldenStderr :: Lazy.Text -> TestIO ()
writeGoldenStderr contents = TestT $ do
  TestEnvironment {..} <- get
  lift $ do
    let stderrGoldenFile = testDirectory </> testName <.> "err.golden"
    LazyIO.writeFile stderrGoldenFile contents

-- | Compare the files produced by the test.
--
-- NOTE: The loose equality must extend equality.
diffTestProduced :: Maybe (Text -> Text -> Bool) -> [FilePattern] -> IgnoreFiles -> SizeOnlyExtensions -> DiffTestIO ()
diffTestProduced maybeLooseEq testProduces (IgnoreFiles testIgnores) sizeOnlyExtensions = do
  TestEnvironment {testDirectory, tempDirectory} <- lift getTestEnvironment
  let shortCircuitLooseEq = shortCircuitWithEq maybeLooseEq
  let sizeOnlyExtensionsSet = toSizeOnlyExtensionsSet sizeOnlyExtensions
  -- Find the golden and actual files:
  goldenFiles <- lift $ findTestProducesGolden testProduces
  actualFiles <- lift $ findTestProducesActual testIgnores

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
  let producedFilesNotExpected = Set.toAscList $ Set.difference actualFileSet goldenFileSet
  for_ producedFilesNotExpected $ tell . Just . producedFileNotExpected
  -- Diff the files which were produced and expected:
  for_ (Set.toAscList $ Set.intersection goldenFileSet actualFileSet) $ \file -> do
    let goldenFile = testDirectory </> file <.> "golden"
    let actualFile = tempDirectory </> file
    catch (lift $ lift $ diffFile shortCircuitLooseEq sizeOnlyExtensionsSet goldenFile actualFile) $ \diff ->
      tell $ Just $ producedAndExpectedDiffer actualFile diff

-- | Assert a golden file is not captured by another test's produces patterns.
assertFileNotCapturedByOtherTest ::
  (Monad m) => TestName -> FilePath -> TestSpec -> WriterT (Maybe AmbiguousGoldenFilesError) m ()
assertFileNotCapturedByOtherTest thisTestName thisTestGoldenFile otherTestSpec = do
  for_ (testSpecProduces otherTestSpec) $ \otherTestProducesPattern ->
    when (otherTestProducesPattern `match` thisTestGoldenFile) $ do
      tell . Just . goldenFileIsAmbiguous $
        AmbiguousGoldenFile
          { thisTestName = thisTestName,
            thisTestGoldenFile = thisTestGoldenFile,
            otherTestName = testSpecName otherTestSpec,
            otherTestProducesPattern = otherTestProducesPattern
          }

data AcceptState = AcceptState
  { acceptTestSpec :: TestSpec,
    acceptTestSpecChanged :: Bool,
    goldenFilesToRemove :: [FilePath],
    actualFilesToCopy :: [FilePath]
  }

initialAcceptState :: TestSpec -> AcceptState
initialAcceptState acceptTestSpec =
  AcceptState
    { acceptTestSpecChanged = False,
      goldenFilesToRemove = [],
      actualFilesToCopy = [],
      ..
    }

-- | Update the test produces patterns to capture a new golden file.
acceptTestProducesPattern :: (Monad m) => FilePath -> StateT AcceptState m ()
acceptTestProducesPattern actualFile = do
  acceptState@AcceptState {..} <- get
  unless (any (`match` actualFile) (testSpecProduces acceptTestSpec)) $ do
    put $
      acceptState
        { acceptTestSpec = acceptTestSpec {testSpecProduces = fromString actualFile : testSpecProduces acceptTestSpec},
          acceptTestSpecChanged = True
        }

-- | Update the files produced by the test.
acceptTestProduced :: [FilePattern] -> IgnoreFiles -> TestIO ()
acceptTestProduced testProduces (IgnoreFiles testIgnores) = do
  TestEnvironment {..} <- getTestEnvironment
  -- Read the test.json file:
  TestSpecs testSpecs <- lift $ readTestSpecsFile (testDirectory </> testSpecsFileName)
  let testSpecsList = toList testSpecs
  let thisTestIndices = List.findIndices ((== testName) . testSpecName) testSpecsList
  -- There should be EXACTLY ONE test named testSpecName:
  thisTestIndex <- maybe (error $ "No unique test named '" <> testName <> "'") (return . fst) (List.uncons thisTestIndices)
  let (otherTestSpecsBefore, thisTestSpecAndOtherTestSpecsAfter) = List.splitAt thisTestIndex testSpecsList
  let (thisTestSpec, otherTestSpecsAfter) =
        fromMaybe (error $ printf "Could not find test named '%s'" testName) $
          List.uncons thisTestSpecAndOtherTestSpecsAfter
  let otherTestSpecs = otherTestSpecsBefore <> otherTestSpecsAfter
  -- Find the golden and actual files:
  actualFiles <- findTestProducesActual testIgnores
  (goldenFiles, _maybeGoldenFilesNotFoundError) <- runWriterT $ findTestProducesGolden_ testProduces
  -- Run a state monad with an accept state, to make iterative updates to the test
  -- specification and schedule copy and delete operations:
  AcceptState {..} <- flip execStateT (initialAcceptState thisTestSpec) $ do
    -- Collect errors in a writer monad:
    maybeError <- execWriterT $
      do
        -- For each actualFile:
        for_ actualFiles $ \actualFile -> do
          -- If the actualFile is matched by any of the "produces" patterns
          -- of any of the other tests, we throw an error:
          for_ otherTestSpecs $ \otherTestSpec ->
            assertFileNotCapturedByOtherTest testName actualFile otherTestSpec
          -- If the actualFile is NOT matched by any of the "produces" patterns
          -- of this test, we add it to the "produces" patterns of this test:
          lift $ acceptTestProducesPattern actualFile
          -- Copy the actualFile:
          lift $ modify $ \acceptState ->
            acceptState {actualFilesToCopy = actualFile : actualFilesToCopy acceptState}
        -- For each goldenFile:
        for_ goldenFiles $ \goldenFile -> do
          -- Assert that the goldenFile ends with .golden:
          case stripExtension ".golden" goldenFile of
            Just goldenFileBase ->
              -- If the goldenFile is matched by any of the "produces" patterns
              -- of any of the other tests, we throw an error:
              for_ otherTestSpecs $ \otherTestSpec ->
                assertFileNotCapturedByOtherTest testName goldenFileBase otherTestSpec
            Nothing ->
              fail $
                printf "found golden file without .golden extension: %s" goldenFile
          -- If the goldenFile is NOT in the set of actualFiles, remove it:
          lift $ modify $ \acceptState ->
            acceptState {goldenFilesToRemove = goldenFile : goldenFilesToRemove acceptState}
        -- For each "produces" pattern:
        -- If the "produces" pattern does NOT match any of the new golden files, remove it:
        oldTestSpecProduces <- testSpecProduces . acceptTestSpec <$> get
        let newTestSpecProduces =
              [ testSpecProducesPattern
              | testSpecProducesPattern <- oldTestSpecProduces,
                any (testSpecProducesPattern `match`) actualFiles
              ]
        modify $ \acceptState@AcceptState {..} ->
          acceptState {acceptTestSpec = acceptTestSpec {testSpecProduces = newTestSpecProduces}}
    maybe (return ()) throw maybeError

  -- If no errors occurred:
  -- Write the new test specification:
  when acceptTestSpecChanged $ do
    let acceptTestSpecsList =
          NonEmpty.prependList
            otherTestSpecsBefore
            $ NonEmpty.appendList (NonEmpty.singleton acceptTestSpec) otherTestSpecsAfter
    let acceptTestSpecs = TestSpecs acceptTestSpecsList
    lift $ writeTestSpecsFile (testDirectory </> testSpecsFileName) acceptTestSpecs
  -- Remove the outdated .golden files:
  lift $
    for_ goldenFilesToRemove $ \goldenFile -> do
      goldenFileExists <- doesFileExist goldenFile
      when goldenFileExists $
        removeFile (testDirectory </> goldenFile)
  -- Copy the new .golden files:
  lift $ do
    for_ actualFilesToCopy $ \actualFile -> do
      let goldenFile = actualFile <.> ".golden"
      goldenFileExists <- doesFileExist (testDirectory </> goldenFile)
      if goldenFileExists
        then do
          actualFileContents <- TextIO.readFile (tempDirectory </> actualFile)
          writeFileChanged (testDirectory </> goldenFile) actualFileContents
        else do
          createDirectoryIfMissing True (takeDirectory (testDirectory </> actualFile))
          copyFile (tempDirectory </> actualFile) (testDirectory </> actualFile <.> ".golden")
  return ()

-- | Find the actual files produced by the test command.
findTestProducesActual :: [FilePattern] -> TestIO [FilePath]
findTestProducesActual testIgnoreFiles = do
  TestEnvironment {..} <- getTestEnvironment
  tempFiles <-
    lift $
      listFilesRecursive tempDirectory
        <&> map (makeRelative tempDirectory)
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
findTestProducesGolden testProduces = do
  (filesByPattern, maybeError) <- runWriterT $ findTestProducesGolden_ testProduces
  -- If any errors occurred, throw them.
  maybe (return filesByPattern) throw maybeError

-- | Variant of 'findTestProducesGolden' that gathers the errors in a 'WriterT' monad.
findTestProducesGolden_ :: [FilePattern] -> WriterT (Maybe GoldenFilesNotFoundError) (TestT IO) [FilePath]
findTestProducesGolden_ testProduces = do
  TestEnvironment {..} <- lift getTestEnvironment
  filesByPattern <-
    for testProduces $ \testProduce ->
      case asLiteral testProduce of
        -- If the pattern is a literal path, return that path.
        Just fileForPattern -> return [fileForPattern <.> ".golden"]
        -- Otherwise, file the golden files associated with the pattern.
        Nothing -> do
          filesForPattern <-
            lift $
              lift $
                glob [addExtension testProduce ".golden"] testDirectory
                  <&> map (makeRelative testDirectory)
          -- If the pattern does not result in any matches, don't throw any error.
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

fileSizeCutOffBytes :: Integer
fileSizeCutOffBytes = 100000

-- | Compare two files.
--
-- NOTE: The loose equality must extend equality.
diffFile :: (Text -> Text -> Bool) -> Set String -> FilePath -> FilePath -> IO ()
diffFile eq sizeOnlyExtensions golden actual = do
  withFile golden ReadMode $ \goldenHandle -> do
    goldenSize <- hFileSize goldenHandle
    goldenContents <- LazyIO.hGetContents goldenHandle
    withFile actual ReadMode $ \actualHandle -> do
      actualSize <- hFileSize actualHandle
      let sizeDiff = makeSizeOnlyDiff goldenSize actualSize
      let sizeOnly = Set.member (takeExtension actual) sizeOnlyExtensions
      let maybeSizeDiff = if sizeOnly then Just sizeDiff else Nothing
      actualContents <- LazyIO.hGetContents actualHandle
      if max goldenSize actualSize < fileSizeCutOffBytes
        then diffText eq maybeSizeDiff goldenContents actualContents
        else
          when (goldenContents /= actualContents) $
            throw (NoDiff ("file too big to diff but contents not equal. " <> sizeDiff))

-- | Compare two texts.
--
-- NOTE: The loose equality must extend equality.
diffText :: (Text -> Text -> Bool) -> Maybe String -> Lazy.Text -> Lazy.Text -> IO ()
diffText eq maybeSizeDiff golden actual = do
  -- Lazily split the golden and actual texts into lines
  let goldenLines = Lazy.toStrict <$> Lazy.lines golden
  let actualLines = Lazy.toStrict <$> Lazy.lines actual
  -- Compute the diff
  let groupedDiff = getGroupedDiffBy eq goldenLines actualLines
  -- If both files are the same, the diff should be just "Both":
  unless (all isBoth groupedDiff) $
    throw $
      Diff $ case maybeSizeDiff of
        Nothing -> ppDiff $ mapDiff (Text.unpack <$>) <$> groupedDiff
        Just sizeDiff -> sizeDiff
  return ()

-- | Make a loose equality which ignores text matching the provided text patterns.
makeLooseEq :: IgnoreLines -> Text -> Text -> Bool
makeLooseEq (IgnoreLines patterns) golden actual = strikeOutAll golden == strikeOutAll actual
  where
    strikeOutAll line = foldr strikeOut line patterns

-- | Make a loose equality which short-circuits using equality.
shortCircuitWithEq :: (Eq a) => Maybe (a -> a -> Bool) -> a -> a -> Bool
shortCircuitWithEq maybeEq x y = x == y || maybe False (\eq -> x `eq` y) maybeEq

makeSizeOnlyDiff :: Integer -> Integer -> String
makeSizeOnlyDiff goldenSize actualSize =
  "< expected "
    <> show goldenSize
    <> " bytes\n"
    <> "> produced "
    <> show actualSize
    <> " bytes"

removeParent :: FilePath -> FilePath
removeParent path = case path of
  '.' : '.' : '/' : remainder -> removeParent remainder
  _ -> path
