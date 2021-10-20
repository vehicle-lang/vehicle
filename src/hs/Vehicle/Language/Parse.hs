module Vehicle.Language.Parse
  ( ParseError(..)
  , parseCoreFile
  , parseCoreText
  , parseFrontendFile
  , parseFrontendText
  ) where

import Data.Text (Text, pack)
import Data.Text.IO qualified as T
import System.Exit (exitFailure)

import Vehicle.Core.Abs as Core
import Vehicle.Core.Par as Core (pProg, myLexer)

import Vehicle.Frontend.Abs as Frontend
import Vehicle.Frontend.Lex as Frontend (Token)
import Vehicle.Frontend.Par as Frontend (pProg, myLexer)
import Vehicle.Frontend.Layout as Frontend (resolveLayout)

import Vehicle.Prelude



--------------------------------------------------------------------------------
-- Errors

newtype ParseError
  = BNFCParseError String

instance MeaningfulError ParseError where
  -- TODO need to revamp this error, BNFC must provide some more
  -- information than a simple string surely?
  details (BNFCParseError text) = EError $ ExternalError (pack text)

--------------------------------------------------------------------------------
-- Core

-- | Parses the provided text into the core BNFC AST.
parseCoreText :: Text -> Either ParseError Core.Prog
parseCoreText txt = case Core.pProg (Core.myLexer txt) of
  Left err1      -> Left $ BNFCParseError err1
  Right bnfcProg -> Right bnfcProg

-- Used in both application and testing which is why it lives here.
parseCoreFile :: FilePath -> IO Core.Prog
parseCoreFile file = do
  contents <- T.readFile file
  case parseCoreText contents of
    Left  err  -> do print (details err); exitFailure
    Right ast -> return ast

--------------------------------------------------------------------------------
-- Frontend

type FrontendParser a = [Frontend.Token] -> Either String a

runFrontendParser :: Bool -> FrontendParser a -> Text -> Either String a
runFrontendParser topLevel p t = p (runFrontendLexer topLevel t)

runFrontendLexer :: Bool -> Text -> [Frontend.Token]
runFrontendLexer topLevel = Frontend.resolveLayout topLevel . Frontend.myLexer

-- | Parses the provided text into the frontend BNFC AST.
parseFrontendText :: Text -> Either ParseError Frontend.Prog
parseFrontendText txt = case runFrontendParser True Frontend.pProg txt of
  Left err1      -> Left $ BNFCParseError err1
  Right bnfcProg -> Right bnfcProg

-- Used in both application and testing which is why it lives here.
parseFrontendFile :: FilePath -> IO Frontend.Prog
parseFrontendFile file = do
  contents <- T.readFile file
  case parseFrontendText contents of
    Left  err  -> do print (details err); exitFailure
    Right ast -> return ast