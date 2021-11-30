module Vehicle
  ( run
  , Options(..)
  , Command(..)
  ) where

import Control.Monad (when,)
import System.Exit (exitSuccess)

import Vehicle.Prelude
import Vehicle.Compile (CompileOptions(..), compile)
import Vehicle.Check (CheckOptions(..), check)

--------------------------------------------------------------------------------
-- Main command

run :: Options -> IO ()
run Options{..} = do
  when version $ do
    print version
    exitSuccess

  case commandOption of
    Compile options -> compile logFile options
    Check   options -> check   logFile options

data Options = Options
  { version       :: Bool
  , logFile       :: LogFilePath
  , commandOption :: Command
  } deriving (Show)

data Command
  = Compile CompileOptions
  | Check   CheckOptions
   deriving (Show)