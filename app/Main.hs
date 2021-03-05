{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (when)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, exitFailure)
import           System.Console.GetOpt
import qualified Vehicle.Core.Parse as VC
import qualified Vehicle.Core.Print as VC
import qualified Vehicle.Frontend.Elaborate as VF
import qualified Vehicle.Frontend.Parse as VF

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


parseAndElab :: Lang -> Text -> IO VC.Prog
parseAndElab Frontend contents = do
  progVF <- fromEitherIO (VF.parseText contents)
  fromEitherIO (VF.runElab (VF.elab progVF))
parseAndElab Core contents =
  fromEitherIO (VC.parseText contents)

fromEitherIO :: Show e => Either e a -> IO a
fromEitherIO (Left err) = do print err; exitFailure
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
