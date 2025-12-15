module Vehicle.Syntax.Parse
  ( ParseError (..),
    ParseLocation,
    readAndParseModule,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import Vehicle.Syntax.AST
import Vehicle.Syntax.BNFC.Elaborate.External (elabModule)
import Vehicle.Syntax.BNFC.Utils (ParseLocation)
import Vehicle.Syntax.External.Abs qualified as External (Module)
import Vehicle.Syntax.External.Layout as External (resolveLayout)
import Vehicle.Syntax.External.Lex as External (Token)
import Vehicle.Syntax.External.Par as External (myLexer, pModule)
import Vehicle.Syntax.Parse.Error (ParseError (..))

--------------------------------------------------------------------------------
-- Interface

readAndParseModule :: (MonadError ParseError m) => ParseLocation -> Text -> m Module
readAndParseModule modul txt = castBNFCError (elabModule modul) (parseExternalModule txt)

{-
parseDecl :: (MonadError ParseError m) => ParseLocation -> PartiallyParsedDecl -> m Decl
parseDecl = elaborateDecl

parseExpr :: (MonadError ParseError m) => ParseLocation -> UnparsedExpr -> m Expr
parseExpr = elaborateExpr

readExpr :: (MonadError ParseError m) => Text -> m UnparsedExpr
readExpr txt = castBNFCError (return . UnparsedExpr) (parseExternalExpr txt)
-}
--------------------------------------------------------------------------------
-- Parsing

type ExternalParser a = [External.Token] -> Either String a

{-
parseExternalExpr :: Text -> Either String External.Expr
parseExternalExpr = runExternalParser False External.pExpr
-}
parseExternalModule :: Text -> Either String External.Module
parseExternalModule = runExternalParser True External.pModule

runExternalParser :: Bool -> ExternalParser a -> Text -> Either String a
runExternalParser topLevel p t = p (runExternalLexer topLevel t)

runExternalLexer :: Bool -> Text -> [External.Token]
runExternalLexer topLevel = External.resolveLayout topLevel . External.myLexer

castBNFCError :: (MonadError ParseError m) => (a -> m b) -> Either String a -> m b
castBNFCError f = \case
  Left err -> throwError $ RawParseError err
  Right value -> f value
