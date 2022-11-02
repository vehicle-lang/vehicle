module Vehicle.Compile.Parse
  ( ParseVehicle(..)
  , parseVehicleFile
  , parseExternalExpr
  ) where

import Control.Monad.Except (MonadError (..), liftEither)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.IO qualified as T

import Vehicle.Syntax.Internal.Abs as Internal
import Vehicle.Syntax.Internal.Par as Internal (myLexer, pExpr, pProg)

import Vehicle.Syntax.External.Abs as External
import Vehicle.Syntax.External.Layout as External (resolveLayout)
import Vehicle.Syntax.External.Lex as External (Token)
import Vehicle.Syntax.External.Par as External (myLexer, pExpr, pProg)

import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- Parsing

class ParseVehicle a where
  parseVehicle :: MonadError CompileError m => Text -> m a

instance ParseVehicle Internal.Prog where
  parseVehicle = castError . parseInternalProg

instance ParseVehicle Internal.Expr where
  parseVehicle = castError . parseInternalExpr

instance ParseVehicle External.Prog where
  parseVehicle = castError . parseExternalProg

instance ParseVehicle External.Expr where
  parseVehicle = castError . parseExternalExpr

castError :: MonadError CompileError m => Either String a -> m a
castError = liftEither . first BNFCParseError

type ExternalParser a = [External.Token] -> Either String a

parseExternalExpr :: Text -> Either String External.Expr
parseExternalExpr = runExternalParser False External.pExpr

parseExternalProg :: Text -> Either String External.Prog
parseExternalProg = runExternalParser True External.pProg

parseInternalExpr :: Text -> Either String Internal.Expr
parseInternalExpr = Internal.pExpr . Internal.myLexer

parseInternalProg :: Text -> Either String Internal.Prog
parseInternalProg = Internal.pProg . Internal.myLexer

runExternalParser :: Bool -> ExternalParser a -> Text -> Either String a
runExternalParser topLevel p t = p (runExternalLexer topLevel t)

runExternalLexer :: Bool -> Text -> [External.Token]
runExternalLexer topLevel = External.resolveLayout topLevel . External.myLexer

-- Used in both application and testing which is why it lives here.
parseVehicleFile :: ParseVehicle a => FilePath -> IO (Either CompileError a)
parseVehicleFile file = do
  contents <- T.readFile file
  return $ parseVehicle contents
