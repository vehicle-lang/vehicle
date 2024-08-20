module Vehicle.Syntax.Parse
  ( ParseError (..),
    UnparsedExpr,
    PartiallyParsedProg,
    PartiallyParsedDecl,
    ParseLocation,
    readAndParseProg,
    parseDecl,
    parseExpr,
    readExpr,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Text (Text)
import Vehicle.Syntax.AST
import Vehicle.Syntax.BNFC.Elaborate.External
import Vehicle.Syntax.BNFC.Utils (ParseLocation)
import Vehicle.Syntax.Builtin
import Vehicle.Syntax.External.Abs qualified as External (Expr, Prog)
import Vehicle.Syntax.External.Layout as External (resolveLayout)
import Vehicle.Syntax.External.Lex as External (Token)
import Vehicle.Syntax.External.Par as External (myLexer, pExpr, pProg)
import Vehicle.Syntax.Parse.Error (ParseError (..))

--------------------------------------------------------------------------------
-- Interface

readAndParseProg :: (MonadError ParseError m) => ParseLocation -> Text -> m PartiallyParsedProg
readAndParseProg modul txt =
  castBNFCError (partiallyElabProg modul) (parseExternalProg txt)

parseDecl :: (MonadError ParseError m) => ParseLocation -> PartiallyParsedDecl -> m (Decl Name Builtin)
parseDecl = elaborateDecl

parseExpr :: (MonadError ParseError m) => ParseLocation -> UnparsedExpr -> m (Expr Name Builtin)
parseExpr = elaborateExpr

readExpr :: (MonadError ParseError m) => Text -> m UnparsedExpr
readExpr txt = castBNFCError (return . UnparsedExpr) (parseExternalExpr txt)

--------------------------------------------------------------------------------
-- Parsing

type ExternalParser a = [External.Token] -> Either String a

parseExternalExpr :: Text -> Either String External.Expr
parseExternalExpr = runExternalParser False External.pExpr

parseExternalProg :: Text -> Either String External.Prog
parseExternalProg = runExternalParser True External.pProg

runExternalParser :: Bool -> ExternalParser a -> Text -> Either String a
runExternalParser topLevel p t = p (runExternalLexer topLevel t)

runExternalLexer :: Bool -> Text -> [External.Token]
runExternalLexer topLevel = External.resolveLayout topLevel . External.myLexer

castBNFCError :: (MonadError ParseError m) => (a -> m b) -> Either String a -> m b
castBNFCError f = \case
  Left err -> throwError $ RawParseError err
  Right value -> f value
