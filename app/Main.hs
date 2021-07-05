{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}

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
import Vehicle.Frontend.Elaborate qualified as VF
import Vehicle.Frontend.Parse qualified as VF
import Vehicle.Error qualified as V

--------------------------------------------------------------------------------
-- Command flow

data InputLang = Frontend | Core

data ITP = Agda

data Solver

data OutputTarget
  = ITP ITP
  | Solver Solver

--------------------------------------------------------------------------------
-- Command-line options

data Options = Options
  { showHelp     :: Bool
  , showVersion  :: Bool
  , inputFile    :: Maybe FilePath
  , inputLang    :: InputLang
  , outputTarget :: Maybe OutputTarget
  }

defaultOptions :: Options
defaultOptions = Options
  { showHelp     = False
  , showVersion  = False
  , inputFile    = Nothing
  , inputLang    = Frontend
  , outputTarget = Nothing
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
      [ "agda" |-> ITP Agda
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

  -- Scope check, type check etc.
  compCoreProg <- compile coreProg

  -- Compile to requested backend
  case outputTarget of
    Nothing -> do
      putStrLn "Please specify an output target with -t or --target"
      exitFailure

    Just (ITP itp) -> putStrLn (VC.printTree compCoreProg)
      -- Delaborate back to front end

    Just (Solver v) -> putStrLn (VC.printTree compCoreProg)

parseAndElab :: InputLang -> Text -> IO VC.InputProg
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
fromEitherIO (Right x) = return x

readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin (Just file) = T.readFile file
readFileOrStdin Nothing = T.getContents
