module Test.Tasty.Golden.Executable.TestSpec
  ( TestSpec (..),
    derivedOptions,
    isEnabled,
  )
where

import Control.Monad (forM, unless)
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
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.TestSpec.External (External)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Golden.Executable.TestSpec.TextPattern (TextPattern)
import Test.Tasty.Golden.Executable.TestSpec.Timeout (Timeout, toSomeOption)
import Test.Tasty.Options (OptionSet)
import Test.Tasty.Providers (Result)
import Text.Printf (printf)

data TestSpec = TestSpec
  { -- | Test name.
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
run :: FilePath -> TestSpec -> OptionSet -> IO Result
run specTestDirectory TestSpec {..} options = do
  -- + Find needed files
  neededPaths <- forM testSpecNeeds $ \neededFile -> do
    let neededPath = specTestDirectory </> neededFile
    neededFileExists <- doesFileExist neededPath
    unless neededFileExists $
      fail $
        printf "Missing needed file: %s" neededPath
    return neededPath
  -- + Find golden files for stdout and stderr
  -- + Make temporary directory
  -- + Copy needed files
  -- + Exec command
  -- + Diff stdout and golden file for stdout
  -- + Diff stderr and golden file for stderr
  -- + Find produced files
  -- + Find golden files for produced files
  -- + Diff produced files and golden files
  _

-- | Local options derived from the test specification.
derivedOptions :: TestSpec -> [SomeOption]
derivedOptions testSpec = [toSomeOption (testSpecTimeout testSpec)]

-- | Whether or not the test is enabled.
isEnabled :: TestSpec -> Bool
isEnabled = fromMaybe True . testSpecEnabled

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o ->
    TestSpec
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
