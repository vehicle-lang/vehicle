{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad (when)
import Data.Text (Text, unpack)
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.Console.GetOpt

import Vehicle.Core.AST qualified as VC
import Vehicle.Core.Parse qualified as VC
import Vehicle.Core.Print qualified as VC
import Vehicle.Frontend.AST qualified as VF
import Vehicle.Frontend.Elaborate qualified as VF
import Vehicle.Frontend.Parse qualified as VF
import Vehicle.Error qualified as V

data Lang = Frontend | Core

data Options = Options
  { inputFile :: Maybe FilePath
  , inputLang :: Lang
  , showHelp  :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { inputFile = Nothing
  , inputLang = Frontend
  , showHelp  = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['i'] ["input-file"]
    (ReqArg (\arg opts -> opts { inputFile = Just arg }) "FILE")
    "Input file."
  , Option [] ["core"]
    (NoArg (\opts -> opts { inputLang = Core }))
    "Set input language to Vehicle Core."
  , Option ['h'] ["help"]
    (NoArg (\opts -> opts { showHelp = True }))
    "Show help information."
  ]


main :: IO ()
main = do

  -- Parse command-line arguments
  (_opts@Options{..}, _args) <- parseOpts =<< getArgs

  -- Print usage information and exit
  when showHelp $ do
    putStrLn (usageInfo usageHeader options)
    exitSuccess

  -- Parse frontend, elaborate to core, and print
  contents <- readFileOrStdin inputFile
  progVC <- parseAndElab inputLang contents
  putStrLn (VC.printTree progVC)

parseAndElab :: Lang -> Text -> IO VC.InputProg
parseAndElab Frontend contents = do
  progVF <- fromEitherIO (VF.parseText contents)
  fromEitherIO (VF.runElab (VF.elab progVF))
parseAndElab Core contents =
  fromEitherIO (VC.parseText contents)

fromEitherIO :: V.MeaningfulError e => Either e a -> IO a
fromEitherIO (Left err) = do print (V.details err); exitFailure
fromEitherIO (Right x) = return x

readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin (Just file) = T.readFile file
readFileOrStdin Nothing = T.getContents

usageHeader :: String
usageHeader = "Usage: vehicle [OPTION...]\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))
