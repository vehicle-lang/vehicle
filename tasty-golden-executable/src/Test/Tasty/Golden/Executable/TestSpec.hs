module Test.Tasty.Golden.Executable.TestSpec
  ( TestSpec (..),
    derivedOptions,
    isEnabled,
  )
where

import Control.Exception (Exception, throw)
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    Parser,
    ToJSON (toJSON),
    Value,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Maybe (catMaybes, fromMaybe)
import General.Extra (boolToMaybe)
import General.Extra.Option (SomeOption (..))
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.Runner (copyTestNeeds, diffStderr, diffStdout, diffTestProduced, makeLooseEq, runTestIO, runTestRun)
import Test.Tasty.Golden.Executable.TestSpec.External (External)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (TextPattern)
import Test.Tasty.Golden.Executable.TestSpec.Timeout (Timeout, toSomeOption)
import Test.Tasty.Options (OptionSet)
import Test.Tasty.Providers (Result)

data TestSpec = TestSpec
  { -- | Test directory.
    testSpecDirectory :: FilePath,
    -- | Test name.
    --   In a file with multiple test specifications, each name must be unique.
    testSpecName :: TestName,
    -- | Test command to run.
    testSpecRun :: String,
    -- | Whether or not the test is enabled.
    --   By default, the test is assumed to be enabled.
    testSpecEnabled :: Maybe Bool,
    -- | Files needed by the test command.
    --   Paths should be relative to the test specification file.
    testSpecNeeds :: [FilePath],
    -- | Files produced by the test command.
    --   Paths should be relative to the test specification file,
    --   and should not contain the .golden file extension.
    testSpecProduces :: [FilePattern],
    -- | External tools needed by the test command.
    --   Paths should be the names of executables on the PATH.
    testSpecExternal :: [External],
    -- | Timeout for the test.
    testSpecTimeout :: Timeout,
    -- | Options that configure what differences to ignore.
    testSpecIgnore :: TestSpecIgnore
  }
  deriving (Eq, Show)

data TestSpecIgnore = TestSpecIgnore
  { -- | Files produced by the test command that should be ignored.
    testSpecIgnoreFiles :: [FilePattern],
    -- | Lines produced by the test command that should be ignored.
    testSpecIgnoreLines :: [TextPattern]
  }
  deriving (Eq, Show)

-- | Run a 'TestSpec'.
run :: TestSpec -> OptionSet -> IO Result
run TestSpec {testSpecIgnore = TestSpecIgnore {..}, ..} options = do
  -- Create loose equality based on the ignore options
  let maybeLooseEq
        | null testSpecIgnoreLines = Nothing
        | otherwise = Just $ makeLooseEq testSpecIgnoreLines
  -- Create test environment
  runTestIO testSpecDirectory testSpecName $ do
    -- Copy needs to test environment
    copyTestNeeds testSpecNeeds
    -- Run test command
    (stdout, stderr) <- runTestRun testSpecRun
    -- Diff stdout
    diffStdout maybeLooseEq stdout
    -- Diff stderr
    diffStderr maybeLooseEq stderr
    -- Diff produced files
    diffTestProduced maybeLooseEq testSpecProduces testSpecIgnoreFiles
    -- + Find golden files for produced files
    -- + Diff produced files and golden files
    undefined

-- | Local options derived from the test specification.
derivedOptions :: TestSpec -> [SomeOption]
derivedOptions testSpec = [toSomeOption (testSpecTimeout testSpec)]

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

-- | Error raised when 'testSpecDirectory' is not set.
data TestSpecDirectoryMissing = TestSpecDirectoryMissing
  deriving (Show)

instance Exception TestSpecDirectoryMissing

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o ->
    TestSpec (throw TestSpecDirectoryMissing)
      <$> o .: "name"
      <*> o .: "run"
      <*> o .:? "enabled"
      <*> o .:? "needs" .!= mempty
      <*> o .:? "produces" .!= mempty
      <*> o .:? "external" .!= mempty
      <*> o .:? "timeout" .!= mempty
      <*> o .:? "ignore" .!= mempty

instance ToJSON TestSpec where
  toJSON :: TestSpec -> Value
  toJSON TestSpec {..} =
    object $
      catMaybes
        [ Just $ "name" .= testSpecName,
          Just $ "run" .= testSpecRun,
          ("enabled" .=) <$> testSpecEnabled,
          -- Include "needs" only if it is non-empty:
          boolToMaybe (testSpecNeeds /= mempty) ("needs" .= testSpecNeeds),
          -- Include "produces" only if it is non-empty:
          boolToMaybe (testSpecProduces /= mempty) ("produces" .= testSpecProduces),
          -- Include "external" only if it is non-empty:
          boolToMaybe (testSpecExternal /= mempty) ("external" .= testSpecNeeds),
          -- Include "timeout" only if it is non-empty:
          boolToMaybe (testSpecTimeout /= mempty) ("timeout" .= testSpecTimeout),
          -- Include "ignore" only if it is non-empty:
          boolToMaybe (testSpecIgnore /= mempty) ("ignore" .= testSpecIgnore)
        ]

instance Semigroup TestSpecIgnore where
  (<>) :: TestSpecIgnore -> TestSpecIgnore -> TestSpecIgnore
  ignore1 <> ignore2 =
    TestSpecIgnore
      { testSpecIgnoreFiles = testSpecIgnoreFiles ignore1 <> testSpecIgnoreFiles ignore2,
        testSpecIgnoreLines = testSpecIgnoreLines ignore1 <> testSpecIgnoreLines ignore2
      }

instance Monoid TestSpecIgnore where
  mempty :: TestSpecIgnore
  mempty = TestSpecIgnore mempty mempty

instance FromJSON TestSpecIgnore where
  parseJSON :: Value -> Parser TestSpecIgnore
  parseJSON = withObject "TestSpecIgnore" $ \o ->
    TestSpecIgnore
      <$> o .:? "files" .!= mempty
      <*> o .:? "lines" .!= mempty

instance ToJSON TestSpecIgnore where
  toJSON :: TestSpecIgnore -> Value
  toJSON TestSpecIgnore {..} =
    object $
      catMaybes
        [ boolToMaybe (testSpecIgnoreFiles /= mempty) ("files" .= testSpecIgnoreFiles),
          boolToMaybe (testSpecIgnoreLines /= mempty) ("lines" .= testSpecIgnoreLines)
        ]
