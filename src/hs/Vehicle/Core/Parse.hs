module Vehicle.Core.Parse
  ( parseText
  , parseFile
  , module Abs
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Exit (exitFailure)
import Vehicle.Core.Abs as Abs
import Vehicle.Core.Par (pProg, myLexer)

parseText :: Text -> Either String Prog
parseText = pProg . myLexer

parseFile :: FilePath -> IO Prog
parseFile file = do
  contents <- T.readFile file
  case parseText contents of
    Left err -> do putStrLn err; exitFailure
    Right ast -> return ast
