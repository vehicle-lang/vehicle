{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle
  ( run
  , runWithOptions
  , Options(..)
  , Command(..)
  , CompileOptions(..)
  , CheckOptions(..)
  ) where

import Paths_vehicle qualified as VehiclePath (version)

import Control.Monad (when,)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
import Data.Version (Version, makeVersion)
import System.Exit (exitSuccess, exitFailure)

import Options.Applicative

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Vehicle.Language.Parse qualified as V
import Vehicle.Language.Elaborate.Frontend as Frontend
import Vehicle.Language.Scope qualified as V
import Vehicle.Language.Type qualified as V
import Vehicle.Language.Normalise qualified as V (normalise)

import Vehicle.Backend.Verifier.SMTLib (compileToSMTLib, SMTDoc(..))
import Vehicle.Backend.Verifier.VNNLib (compileToVNNLib, VNNLibDoc(..))
import Vehicle.Backend.ITP.Agda (compileToAgda, AgdaOptions(..))

--------------------------------------------------------------------------------
-- Main command

run :: IO ()
run = runWithOptions =<< execParser opts
  where
  opts = info (optionsParser <**> helper)
      ( fullDesc
     <> header "vehicle - a program for neural network verification" )

runWithOptions :: Options -> IO ()
runWithOptions Options{..} = do
  when version $ do
    print version
    exitSuccess

  case commandOption of
    Compile options -> compile logFile options
    Check   options -> check   logFile options

--------------------------------------------------------------------------------
-- Options and parsing

type LogFilePath = Maybe (Maybe FilePath)

data Options = Options
  { version       :: Bool
  , logFile       :: LogFilePath
  , commandOption :: Command
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
      ( long "version"
     <> short 'V'
     <> help "Show version information." )
  <*> option auto
      ( long "log-file"
     <> help "Enables logging to the provided file. If no argument is provided will output to stdout."
     <> showDefault
     <> value Nothing
     <> metavar "FILENAME" )
  <*> commandParser

data Command
  = Compile CompileOptions
  | Check   CheckOptions
   deriving (Show)

commandParser :: Parser Command
commandParser = hsubparser
    ( command "compile" (info (Compile <$> compileParser) compileDescription)
   <> command "check"   (info (Check   <$> checkParser)   checkDescription)
    )

data CompileOptions = CompileOptions
  { inputFile      :: FilePath
  , outputFile     :: Maybe FilePath
  , outputTarget   :: OutputTarget
  } deriving (Show)

compileDescription :: InfoMod Command
compileDescription = progDesc "Compile a .vcl file to an output target"

compileParser :: Parser CompileOptions
compileParser = CompileOptions
  <$> option auto
      ( long "inputFile"
     <> short 'i'
     <> help "Input .vcl file."
     <> metavar "FILENAME" )
  <*> optional (option auto
      ( long "outputFile"
     <> short 'o'
     <> help "Output location for compiled file. Defaults to stdout if not provided."
     <> metavar "FILENAME" ))
  <*> option auto
      ( long "target"
     <> short 't'
     <> help "Compilation target."
     <> metavar "TARGET" )

data CheckOptions = CheckOptions
  { databaseFile :: FilePath
  , propertyUUID :: String
  } deriving (Show)

checkDescription :: InfoMod Command
checkDescription = progDesc "Check the verification status of a Vehicle property."

checkParser :: Parser CheckOptions
checkParser = CheckOptions
 <$> option auto
      ( long "databaseFile"
     <> short 'd'
     <> help "The database file for the Vehicle project."
     <> metavar "FILENAME" )
 <*> option auto
      ( long "property"
     <> short 'p'
     <> help "The UUID of the Vehicle property."
     <> metavar "UUID" )

--------------------------------------------------------------------------------
-- Compilation

compile :: Maybe (Maybe FilePath) -> CompileOptions -> IO ()
compile logFile opts@CompileOptions{..} = do

  -- Read file, parse and elaborate to core if necessary
  contents <- TIO.readFile inputFile
  rawProg  <- parseAndElab logFile contents

  -- Scope check, type check etc.
  scopedCoreProg <- fromLoggedEitherIO logFile $ V.scopeCheck rawProg
  typedCoreProg  <- fromLoggedEitherIO logFile $ V.typeCheck scopedCoreProg

  -- Compile to requested backend
  case outputTarget of
    target@(ITP itp) -> do
      case itp of
        Agda -> do
          let agdaOptions = AgdaOptions "TODO/vehicle/path" ["MyTestModule"] mempty
          agdaDoc <- fromLoggedEitherIO logFile $ compileToAgda agdaOptions typedCoreProg
          writeResultToFile opts target agdaDoc

    (Verifier verifier) -> do
      normProg <- fromLoggedEitherIO logFile $ V.normalise typedCoreProg
      case verifier of
        SMTLib -> toSMTLib logFile opts normProg
        VNNLib -> toVNNLib logFile opts normProg

parseAndElab :: Maybe (Maybe FilePath) -> Text -> IO V.InputProg
parseAndElab logFile contents = do
  progVF <- fromEitherIO (V.parseFrontendText contents)
  fromLoggedEitherIO logFile $ Frontend.runElab progVF

fromEitherIO :: MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print $ details err; exitFailure
fromEitherIO (Right x)  = return x

fromLoggedEitherIO :: MeaningfulError e => Maybe (Maybe FilePath) -> ExceptT e Logger a -> IO a
fromLoggedEitherIO logFile x = fromEitherIO =<< fromLoggedIO logFile (runExceptT x)

fromLoggedIO :: Maybe (Maybe FilePath) -> Logger a -> IO a
fromLoggedIO Nothing        logger = return $ discardLogger logger
fromLoggedIO (Just logFile) logger = flushLogs logFile logger

writeResultToFile :: CompileOptions -> OutputTarget -> Doc a -> IO ()
writeResultToFile CompileOptions{..} target doc = do
  let fileHeader = makefileHeader target
  let outputText = layoutAsText (fileHeader <> line <> line <> doc)
  case outputFile of
    Nothing             -> TIO.putStrLn outputText
    Just outputFilePath -> TIO.writeFile outputFilePath outputText

toSMTLib :: LogFilePath -> CompileOptions -> V.CheckedProg -> IO ()
toSMTLib logFile options prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToSMTLib prog)
  mapM_ (\doc -> writeResultToFile options (Verifier SMTLib) (text doc)) propertyDocs

toVNNLib :: LogFilePath -> CompileOptions -> V.CheckedProg -> IO ()
toVNNLib logFile options prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToVNNLib prog)
  mapM_ (\doc -> writeResultToFile options (Verifier VNNLib) (text (smtDoc doc))) propertyDocs

-- |Generate the file header given the token used to start comments in the
-- target language
makefileHeader :: OutputTarget -> Doc a
makefileHeader target = vsep $
  map (commentTokenOf target <+>)
    [ "WARNING: This file was generated automatically by Vehicle"
    , "and should not be modified manually!"
    , "Metadata"
    , " -" <+> pretty (show target) <> " version:" <+> targetVersion
    , " - AISEC version:" <+> pretty VehiclePath.version
    , " - Time generated: ???"
    ]
  where targetVersion = maybe "N/A" pretty (versionOf target)

versionOf :: OutputTarget -> Maybe Version
versionOf target = case target of
  Verifier VNNLib        -> Nothing
  Verifier SMTLib        -> Nothing
  ITP Agda               -> Just $ makeVersion [2,6,2]

commentTokenOf :: OutputTarget -> Doc a
commentTokenOf = \case
  Verifier VNNLib        -> ";"
  Verifier SMTLib        -> ";"
  ITP Agda               -> "--"

--------------------------------------------------------------------------------
-- Checking

check :: LogFilePath -> CheckOptions -> IO ()
check _logFile _checkOptions = do
  print ("Valid" :: String)
  return ()