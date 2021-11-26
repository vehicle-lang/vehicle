module Vehicle.Compile.Parse
  ( ParseError(..)
  , ParseVehicle(..)
  , parseVehicleFile
  ) where

import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Bifunctor (first)

import Vehicle.Core.Abs as Core
import Vehicle.Core.Par as Core (pProg, pExpr, myLexer)

import Vehicle.Frontend.Abs as Frontend
import Vehicle.Frontend.Lex as Frontend (Token)
import Vehicle.Frontend.Par as Frontend (pProg, pExpr, myLexer)
import Vehicle.Frontend.Layout as Frontend (resolveLayout)

import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- Parsing

class ParseVehicle a where
  parseVehicle :: Text -> Either ParseError a

instance ParseVehicle Core.Prog where
  parseVehicle txt = first BNFCParseError $ Core.pProg (Core.myLexer txt)

instance ParseVehicle Core.Expr where
  parseVehicle txt = first BNFCParseError $ Core.pExpr (Core.myLexer txt)

instance ParseVehicle Frontend.Prog where
  parseVehicle txt = first BNFCParseError $ runFrontendParser True Frontend.pProg txt

instance ParseVehicle Frontend.Expr where
  parseVehicle txt = first BNFCParseError $ runFrontendParser True Frontend.pExpr txt

type FrontendParser a = [Frontend.Token] -> Either String a

runFrontendParser :: Bool -> FrontendParser a -> Text -> Either String a
runFrontendParser topLevel p t = p (runFrontendLexer topLevel t)

runFrontendLexer :: Bool -> Text -> [Frontend.Token]
runFrontendLexer topLevel = Frontend.resolveLayout topLevel . Frontend.myLexer

-- Used in both application and testing which is why it lives here.
parseVehicleFile :: ParseVehicle a => FilePath -> IO (Either ParseError a)
parseVehicleFile file = do
  contents <- T.readFile file
  return $ parseVehicle contents
