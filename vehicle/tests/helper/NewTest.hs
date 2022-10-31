
import Data.Map
import Data.Maybe (maybeToList)
import Options.Applicative
import System.FilePath ((</>))

import Vehicle (ModeOptions (Check, Compile, Export, Verify), Options (..))
import Vehicle.Backend.Prelude
import Vehicle.Check (CheckOptions (..))
import Vehicle.CommandLine (commandLineOptionsParserInfo)
import Vehicle.Compile
import Vehicle.Export (ExportOptions (..))
import Vehicle.Verify (VerifyOptions (..))
import Vehicle.Verify.Core (VerifierIdentifier (..))

main :: IO ()
main = do
  options <- execParser newTestOptionsParserInfo
  testIOFiles <- getTestIOFiles options
  _

--------------------------------------------------------------------------------
-- New test options

data NewTestOptions = NewTestOptions
  { vehicleCommand :: String
  , testDirectory  :: FilePath
  }

newTestOptionsParserInfo :: ParserInfo NewTestOptions
newTestOptionsParserInfo = info (optionsParser <**> helper)
   ( fullDesc
  <> header "Generates new tests for Vehicle"
   )

optionsParser :: Parser NewTestOptions
optionsParser = NewTestOptions
  <$> vehicleCommandParser
  <*> testDirectoryParser

vehicleCommandParser :: Parser String
vehicleCommandParser = strOption $
  long    "vehicleCommand" <>
  short   'c' <>
  metavar "STRING" <>
  help    "The Vehicle command to run the test."

testDirectoryParser :: Parser FilePath
testDirectoryParser = strOption $
  long    "testDirectory" <>
  short   'd' <>
  metavar "FILE" <>
  help    "Location in which to create the test."

--------------------------------------------------------------------------------
-- Creating the new test

data IOFiles = IOFiles
  { needs    :: [FilePath]
  , produces :: [String]
  }

getTestIOFiles :: NewTestOptions -> IO IOFiles
getTestIOFiles NewTestOptions{..} = do
  let vehicleArgs = words vehicleCommand
  let result = execParserPure defaultPrefs commandLineOptionsParserInfo vehicleArgs

  vehicleOptions <- case result of
    Failure failure      -> error (show failure)
    CompletionInvoked cr -> error "should not return CompletionInvoked in test case"
    Success actual       -> return actual

  return $ getIOFiles vehicleOptions

class HasIOFiles a where
  getIOFiles :: a -> IOFiles

instance HasIOFiles Options where
  getIOFiles Options{..} =
    case modeOptions of
      Nothing          -> IOFiles [] []
      Just modeOptions -> case modeOptions of
        Compile opts -> getIOFiles opts
        Verify  opts -> getIOFiles opts
        Export  opts -> getIOFiles opts
        Check   opts -> getIOFiles opts

instance HasIOFiles CompileOptions where
  getIOFiles CompileOptions{..} = IOFiles
    { needs    = specification : elems networkLocations <> elems datasetLocations
    , produces = case target of
        TypeCheck    -> []
        ITP{}        -> maybeToList outputFile
        LossFunction -> maybeToList outputFile
        VerifierBackend verifier -> case verifier of
          Marabou -> maybeToList (fmap (</> "*.txt") outputFile)
    }

instance HasIOFiles ExportOptions where
  getIOFiles ExportOptions{..} = IOFiles
    { needs    = [proofCacheLocation]
    , produces = maybeToList outputFile
    }

instance HasIOFiles VerifyOptions where
  getIOFiles VerifyOptions{..} = IOFiles
    { needs    = specification : elems networkLocations <> elems datasetLocations
    , produces = maybeToList proofCache
    }

instance HasIOFiles CheckOptions where
  getIOFiles CheckOptions{..} = IOFiles
    { needs    = [proofCache]
    , produces = []
    }
