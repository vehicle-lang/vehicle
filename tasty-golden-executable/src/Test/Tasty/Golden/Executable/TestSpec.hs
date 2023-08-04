module Test.Tasty.Golden.Executable.TestSpec
  ( TestSpec (..),
    derivedOptions,
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
import Data.Maybe (catMaybes)
import General.Extra (boolToMaybe)
import General.Extra.Option (SomeOption (..))
import Test.Tasty (TestName)
import Test.Tasty.Golden.Executable.TestSpec.External (External)
import Test.Tasty.Golden.Executable.TestSpec.FilePattern (FilePattern)
import Test.Tasty.Golden.Executable.TestSpec.Ignore (Ignore (..))
import Test.Tasty.Golden.Executable.TestSpec.Timeout (Timeout, toSomeOption)

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
    -- | A list of external tools needed by the test command.
    --   Paths should be the names of executables on the PATH.
    testSpecExternals :: [External],
    -- | Timeout for the test.
    testSpecTimeout :: Timeout,
    -- | Options that configure what differences to ignore.
    testSpecIgnore :: Ignore
  }
  deriving (Eq, Show)

-- | Local options derived from the test specification.
derivedOptions :: TestSpec -> [SomeOption]
derivedOptions testSpec = [toSomeOption (testSpecTimeout testSpec)]

-- | Error raised when 'testSpecDirectory' is not set.
data TestSpecDirectoryMissing = TestSpecDirectoryMissing
  deriving (Show)

instance Exception TestSpecDirectoryMissing

instance FromJSON TestSpec where
  parseJSON :: Value -> Parser TestSpec
  parseJSON = withObject "TestSpec" $ \o -> do
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
          boolToMaybe (testSpecExternals /= mempty) ("external" .= testSpecNeeds),
          -- Include "timeout" only if it is non-empty:
          boolToMaybe (testSpecTimeout /= mempty) ("timeout" .= testSpecTimeout),
          -- Include "ignore" only if it is non-empty:
          boolToMaybe (testSpecIgnore /= mempty) ("ignore" .= testSpecIgnore)
        ]
