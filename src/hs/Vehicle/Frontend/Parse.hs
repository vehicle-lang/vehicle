module Vehicle.Frontend.Parse
  ( parseText
  , parseFile
  , module Abs
  ) where


import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Exit (exitFailure)
import Vehicle.Frontend.Abs as Abs
import Vehicle.Frontend.Layout (resolveLayout)
import Vehicle.Frontend.Lex (Token)
import Vehicle.Frontend.Par (pProg, myLexer)


parseText :: Text -> Either String Prog
parseText = runParser True pProg


parseFile :: FilePath -> IO Prog
parseFile file = do
  contents <- T.readFile file
  case parseText contents of
    Left err -> do putStrLn err; exitFailure
    Right ast -> return ast


type Parser a = [Token] -> Either String a


runParser :: Bool -> Parser a -> Text -> Either String a
runParser topLevel p t = p (runLexer topLevel t)


runLexer :: Bool -> Text -> [Token]
runLexer topLevel = resolveLayout topLevel . myLexer
