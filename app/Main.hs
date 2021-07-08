{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paths_vehicle (version)

import Control.Monad (when)
import Data.Text (Text, unpack)
import Data.Char (toLower)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

import Vehicle.Prelude ((|->) , (!?))
import Vehicle.Core.AST qualified as VC
import Vehicle.Core.Parse qualified as VC
import Vehicle.Core.Print qualified as VC
import Vehicle.Core.Compile qualified as VC
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Frontend.Print qualified as VF
import Vehicle.Frontend.Elaborate qualified as VF
import Vehicle.Frontend.Delaborate qualified as VF
import Vehicle.Frontend.Parse qualified as VF
import Vehicle.Backend.ITP.Core (ITPOptions(..))
import Vehicle.Backend.ITP.Agda (AgdaOptions(..), compileToAgda)
import Vehicle.Error qualified as V

--------------------------------------------------------------------------------
-- Command flow

data VehicleLang = Frontend | Core

data ITP = Agda

data Solver

data OutputTarget
  = ITP ITP
  | Solver Solver
  | Vehicle VehicleLang

--------------------------------------------------------------------------------
-- Command-line options

data Options = Options
  { showHelp     :: Bool
  , showVersion  :: Bool
  , inputLang    :: VehicleLang
  , inputFile    :: Maybe FilePath
  , outputTarget :: Maybe OutputTarget
  , outputFile   :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { showHelp     = False
  , showVersion  = False
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
    (ReqArg (\arg opts -> opts { outputTarget = parseOutputTarget arg}) "FILE")
    "Output file."
  ]

usageHeader :: String
usageHeader = "Usage: vehicle [OPTION...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

parseOutputTarget :: String -> Maybe OutputTarget
parseOutputTarget s = outputTargets !? map toLower s
  where
    outputTargets =
      [ "agda"         |-> ITP Agda
      , "vehicle-core" |-> Vehicle Core
      , "vehicle"      |-> Vehicle Frontend
      ]

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do

  -- Parse command-line arguments
  (_opts@Options{..}, _args) <- parseOpts =<< getArgs

  -- Print usage information and exit
  when (showVersion || showHelp) $ do
    when showVersion $ do
      print version
    when showHelp $ do
      putStrLn (usageInfo usageHeader options)
    exitSuccess

  -- Read file, parse and elaborate to core if necessary
  contents <- readFileOrStdin inputFile
  coreProg <- parseAndElab inputLang contents

  T.putStrLn (VC.printTree coreProg)

  -- Scope check, type check etc.
  compCoreProg <- compile coreProg

  T.putStrLn (VC.printTree compCoreProg)

  -- Compile to requested backend
  case outputTarget of
    Nothing -> do
      putStrLn "Please specify an output target with -t or --target"
      exitFailure

    Just (ITP itp) -> do
      compFrontProg <- fromEitherIO $ VF.runDelab compCoreProg

      outputText <- case itp of
        Agda -> do
          let itpOptions = ITPOptions
                { vehicleUIDs  = mempty
                , aisecVersion = version
                , backendOpts  = AgdaOptions
                  { useProp    = False
                  , modulePath = ["Testing"]
                  }
                }
          fromEitherIO $ compileToAgda itpOptions compFrontProg

      case outputFile of
        Nothing             -> T.putStrLn outputText
        Just outputFilePath -> T.writeFile outputFilePath outputText

    Just (Solver v) -> do
      putStrLn "No solvers implemented yet"
      exitFailure

    Just (Vehicle Core) -> T.putStrLn (VC.printTree compCoreProg)
    Just (Vehicle Frontend) -> do
      compFrontProg <- fromEitherIO $ VF.runDelab compCoreProg
      T.putStrLn (VF.printTree compFrontProg)

parseAndElab :: VehicleLang -> Text -> IO VC.InputProg
parseAndElab Frontend contents = do
  progVF <- fromEitherIO (VF.parseText contents)
  fromEitherIO (VF.runElab (VF.elab progVF))
parseAndElab Core contents =
  fromEitherIO (VC.parseText contents)

compile :: VC.InputProg -> IO VC.OutputProg
compile prog = do
  fromEitherIO (VC.compile prog)

fromEitherIO :: V.MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print (V.details err); exitFailure
fromEitherIO (Right x)  = return x

readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin (Just file) = T.readFile file
readFileOrStdin Nothing = T.getContents
