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
  , readFileOrStdin
  ) where

import Paths_vehicle (version)

import Control.Monad (when,)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Char (toLower)
import Data.Text as T (Text, replace)
import Data.Text.IO qualified as TIO
import Data.ByteString qualified as B
import Data.Version (Version, makeVersion)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

import Vehicle.Prelude
import Vehicle.Language.AST qualified as V
import Vehicle.Language.Parse qualified as V
import Vehicle.Language.Print qualified as V
import Vehicle.Language.Elaborate.Core as Core
import Vehicle.Language.Elaborate.Frontend as Frontend
import Vehicle.Language.Scope qualified as V
import Vehicle.Language.Type qualified as V
import Vehicle.Language.Normalise qualified as V (normalise)

import Vehicle.Backend.Verifier.SMTLib (compileToSMTLib, SMTDoc(..))
import Vehicle.Backend.Verifier.VNNLib (compileToVNNLib, VNNLibDoc(..))
import Vehicle.Backend.ITP.Agda (compileToAgda, AgdaOptions(..))
import System.Info (os)

--------------------------------------------------------------------------------
-- Command-line options

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

versionOf :: OutputTarget -> Version -> Maybe Version
versionOf target vehicleVersion = case target of
  Verifier VNNLib        -> Nothing
  Verifier SMTLib        -> Nothing
  ITP Agda               -> Just $ makeVersion [2,6,2]
  ITP (Vehicle Frontend) -> Just vehicleVersion
  ITP (Vehicle Core)     -> Just vehicleVersion

commentTokenOf :: OutputTarget -> Doc a
commentTokenOf = \case
  Verifier VNNLib        -> ";"
  Verifier SMTLib        -> ";"
  ITP Agda               -> "--"
  ITP (Vehicle Frontend) -> "--"
  ITP (Vehicle Core)     -> "--"

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
  rawProg  <- parseAndElab logFile inputLang contents

  -- Scope check, type check etc.
  scopedCoreProg <- fromLoggedEitherIO logFile $ V.scopeCheck rawProg
  typedCoreProg  <- fromLoggedEitherIO logFile $ V.typeCheck scopedCoreProg

  -- Compile to requested backend
  case outputTarget of
    Nothing -> do
      putStrLn "Please specify an output target with -t or --target"
      exitFailure

    Just target@(ITP itp) -> do
      case itp of
        (Vehicle Core) ->
          writeResultToFile opts target $ V.prettyVerbose typedCoreProg

        (Vehicle Frontend) ->
          writeResultToFile opts target $ V.prettyFriendlyDBClosed typedCoreProg

        Agda -> do
          let agdaOptions = AgdaOptions "TODO/vehicle/path" ["MyTestModule"] mempty
          agdaDoc <- fromLoggedEitherIO logFile $ compileToAgda agdaOptions typedCoreProg
          writeResultToFile opts target agdaDoc

    Just (Verifier verifier) -> do
      normProg <- fromLoggedEitherIO logFile $ V.normalise typedCoreProg
      case verifier of
        SMTLib -> toSMTLib opts normProg
        VNNLib -> toVNNLib opts normProg

parseAndElab :: Maybe (Maybe FilePath) -> VehicleLang -> Text -> IO V.InputProg
parseAndElab logFile Frontend contents = do
  progVF <- fromEitherIO (V.parseFrontendText contents)
  fromLoggedEitherIO logFile $ Frontend.runElab progVF
parseAndElab logFile Core contents = do
  progVC <- fromEitherIO (V.parseCoreText contents)
  fromLoggedEitherIO logFile $ Core.runElab progVC

fromEitherIO :: MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print $ details err; exitFailure
fromEitherIO (Right x)  = return x

fromLoggedEitherIO :: MeaningfulError e => Maybe (Maybe FilePath) -> ExceptT e Logger a -> IO a
fromLoggedEitherIO logFile x = fromEitherIO =<< fromLoggedIO logFile (runExceptT x)

fromLoggedIO :: Maybe (Maybe FilePath) -> Logger a -> IO a
fromLoggedIO Nothing        logger = return $ discardLogger logger
fromLoggedIO (Just logFile) logger = flushLogs logFile logger

readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin (Just file) = decodeUtf8 <$> B.readFile file
readFileOrStdin Nothing     = TIO.getContents

writeResultToFile :: Options -> OutputTarget -> Doc a -> IO ()
writeResultToFile Options{..} target doc = do
  let fileHeader = makefileHeader version target
  let outputText = layoutAsText (fileHeader <> line <> line <> doc)
  let outputText2 = if os == "mingw32" then replace "\n" "\r\n" outputText else outputText
  case outputFile of
    Nothing             -> TIO.putStrLn outputText2
    Just outputFilePath -> B.writeFile outputFilePath (encodeUtf8 outputText2)

toSMTLib :: Options -> V.CheckedProg -> IO ()
toSMTLib opts@Options{..} prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToSMTLib prog)
  mapM_ (\doc -> writeResultToFile opts (Verifier SMTLib) (text doc)) propertyDocs

toVNNLib :: Options -> V.CheckedProg -> IO ()
toVNNLib opts@Options{..} prog = do
  propertyDocs <- fromLoggedEitherIO logFile (compileToVNNLib prog)
  mapM_ (\doc -> writeResultToFile opts (Verifier VNNLib) (text (smtDoc doc))) propertyDocs

-- |Generate the file header given the token used to start comments in the
-- target language
makefileHeader :: Version -> OutputTarget -> Doc a
makefileHeader aisecVersion target = vsep $
  map (commentTokenOf target <+>)
    [ "WARNING: This file was generated automatically by Vehicle"
    , "and should not be modified manually!"
    , "Metadata"
    , " -" <+> pretty (show target) <> " version:" <+> targetVersion
    , " - AISEC version:" <+> pretty aisecVersion
    , " - Time generated: ???"
    ]
  where targetVersion = maybe "N/A" pretty (versionOf target aisecVersion)