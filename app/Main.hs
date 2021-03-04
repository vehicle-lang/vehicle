module Main where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment
import Vehicle.Frontend.Lex (Token)
import Vehicle.Frontend.Par (pProg, myLexer)
import Vehicle.Frontend.Layout (resolveLayout)

type Parser a = [Token] -> Either String a

runParser :: Bool -> Parser a -> Text -> Either String a
runParser topLevel p t = p (runLexer topLevel t)

runLexer :: Bool -> Text -> [Token]
runLexer topLevel = resolveLayout topLevel . myLexer

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "usage: vehicle [INPUT_FILE]"
  let [file] = args
  contents <- T.readFile file
  let errOrAst = runParser True pProg contents
  case errOrAst of
    Left  err -> putStrLn err
    Right ast -> print ast
