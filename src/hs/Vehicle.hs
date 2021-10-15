{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle
  ( VehicleLang(..)
  , ITP(..)
  , Verifier(..)
  , OutputTarget(..)
  , Options(..)
  , defaultOptions
  , parseAndRun
  , run
  ) where

import Paths_vehicle (version)

import Control.Monad (when,)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Data.Char (toLower)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

import Vehicle.Prelude
import Vehicle.Core.AST qualified as VC
import Vehicle.Core.Parse qualified as VC
import Vehicle.Core.Print qualified as VC
import Vehicle.Core.Compile.Scope qualified as VC
import Vehicle.Core.Compile.Type qualified as VC
import Vehicle.Core.Compile.Descope qualified as VC
import Vehicle.Core.Normalise qualified as VC (normalise)

import Vehicle.Frontend.AST qualified as VF
import Vehicle.Frontend.Print qualified as VF
import Vehicle.Frontend.Elaborate qualified as VF
import Vehicle.Frontend.Delaborate qualified as VF
import Vehicle.Frontend.Parse qualified as VF

--import Vehicle.Backend.ITP.Core (ITPOptions(..))
--import Vehicle.Backend.ITP.Agda (AgdaOptions(..), compileToAgda)

import Vehicle.Backend.Verifier.SMTLib (compileToSMTLib, SMTDoc(..))
import Vehicle.Backend.Verifier.VNNLib (compileToVNNLib, VNNLibDoc(..))

--------------------------------------------------------------------------------
-- Command-line options

data VehicleLang = Frontend | Core
  deriving (Show)

data ITP
  = Agda
  | Vehicle VehicleLang
  deriving (Show)

data Verifier
  = VNNLib
  | SMTLib
  deriving (Show)

data OutputTarget
  = ITP ITP
  | Verifier Verifier

instance Show OutputTarget where
  show = \case
    ITP      arg -> show arg
    Verifier arg -> show arg

data Options = Options
  { showHelp     :: Bool
  , showVersion  :: Bool
  , logFile      :: Maybe (Maybe FilePath)
  , inputFile    :: Maybe FilePath
  , inputLang    :: VehicleLang
  , outputFile   :: Maybe FilePath
  , outputTarget :: Maybe OutputTarget
  }

defaultOptions :: Options
defaultOptions = Options
  { showHelp     = False
  , showVersion  = False
  , logFile      = Nothing
  , inputFile    = Nothing
  , inputLang    = Frontend
  , outputTarget = Nothing
  , outputFile   = Nothing
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> opts { showHelp = True }))
    "Show help information."

  , Option ['v'] ["version"]
    (NoArg (\opts -> opts { showVersion = True }))
    "Show version information."

  , Option ['l'] ["log-file"]
    (OptArg (\arg opts -> opts { logFile = Just arg }) "FILE")
    "Enables logging to the provided file. If no argument is provided will output to stdout."

  ,  Option ['i'] ["input-file"]
    (ReqArg (\arg opts -> opts { inputFile = Just arg }) "FILE")
    "Input file."

  , Option [] ["core"]
    (NoArg (\opts -> opts { inputLang = Core }))
    "Set input language to Vehicle Core."

  , Option ['t'] ["target"]
    (ReqArg (\arg opts -> opts { outputTarget = parseOutputTarget arg}) "OutputTarget")
    "Compilation target."

  , Option ['o'] ["output-file"]
    (ReqArg (\arg opts -> opts { outputFile = Just arg}) "FILE")
    "Output file."
  ]

usageHeader :: String
usageHeader = "Usage: vehicle [OPTION...]\n"

usageInstructions :: String
usageInstructions = usageInfo usageHeader options

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInstructions))

parseOutputTarget :: String -> Maybe OutputTarget
parseOutputTarget s = outputTargets !? map toLower s
  where
    outputTargets =
      [ "vnnlib"       |-> Verifier VNNLib
      , "agda"         |-> ITP Agda
      , "vehicle-core" |-> ITP (Vehicle Core)
      , "vehicle"      |-> ITP (Vehicle Frontend)
      ]

--------------------------------------------------------------------------------
-- Main function

parseAndRun :: IO ()
parseAndRun = do
  (opts, _args) <- parseOpts =<< getArgs
  run opts

run :: Options -> IO ()
run opts@Options{..} = do
  -- Print usage information and exit
  when (showVersion || showHelp) $ do
    when showVersion $ do
      print version
    when showHelp $ do
      putStrLn usageInstructions
    exitSuccess

  -- Read file, parse and elaborate to core if necessary
  contents <- readFileOrStdin inputFile
  coreProg <- parseAndElab inputLang contents

  -- Scope check, type check etc.
  scopedCoreProg <- fromLoggedEitherIO logFile $ VC.scopeCheck coreProg
  typedCoreProg  <- fromLoggedEitherIO logFile $ VC.typeCheck scopedCoreProg

  -- Compile to requested backend
  case outputTarget of
    Nothing -> do
      putStrLn "Please specify an output target with -t or --target"
      exitFailure

    Just (ITP itp) -> do
      let descopedCoreProg :: VC.OutputProg = VC.runDescope typedCoreProg
      case itp of
        (Vehicle Core) ->
          writeResultToFile opts $ layoutAsText $ VC.prettyVerbose descopedCoreProg

        (Vehicle Frontend) -> do
          compFrontProg :: VF.OutputProg <- fromLoggedIO logFile $ VF.runDelab descopedCoreProg
          writeResultToFile opts $ layoutAsText $ VF.prettyFrontend compFrontProg

        Agda -> do
          developerError "Agda not current supported"

    Just (Verifier verifier) -> do
      normProg <- fromLoggedEitherIO logFile $ VC.normalise typedCoreProg
      case verifier of
        SMTLib -> toSMTLib opts normProg
        VNNLib -> toVNNLib opts normProg




parseAndElab :: VehicleLang -> Text -> IO VC.InputProg
parseAndElab Frontend contents = do
  progVF <- fromEitherIO (VF.parseText contents)
  return $ VF.runElab progVF
parseAndElab Core contents =
  fromEitherIO (VC.parseText contents)

fromEitherIO :: MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print $ details err; exitFailure
fromEitherIO (Right x)  = return x

fromLoggedEitherIO :: MeaningfulError e => Maybe (Maybe FilePath) -> ExceptT e Logger a -> IO a
fromLoggedEitherIO logFile x = fromEitherIO =<< fromLoggedIO logFile (runExceptT x)

fromLoggedIO :: Maybe (Maybe FilePath) -> Logger a -> IO a
fromLoggedIO Nothing        logger = return $ discardLogger logger
fromLoggedIO (Just logFile) logger = flushLogs logFile logger

readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin (Just file) = T.readFile file
readFileOrStdin Nothing     = T.getContents

writeResultToFile :: Options -> Text -> IO ()
writeResultToFile Options{..} outputText =
  case outputFile of
    Nothing             -> T.putStrLn outputText
    Just outputFilePath -> T.writeFile outputFilePath outputText


toVNNLib :: Options -> VC.CheckedProg -> IO ()
toVNNLib opts@Options{..} prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToSMTLib prog)
  mapM_ (\doc -> writeResultToFile opts (layoutAsText (text doc))) propertyDocs

toSMTLib :: Options -> VC.CheckedProg -> IO ()
toSMTLib opts@Options{..} prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToVNNLib prog)
  mapM_ (\doc -> writeResultToFile opts (layoutAsText (text (smtDoc doc)))) propertyDocs
