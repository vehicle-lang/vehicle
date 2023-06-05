module Vehicle.Syntax.Parse
  ( ParseError (..),
    UnparsedExpr,
    PartiallyParsedProg,
    PartiallyParsedDecl,
    readAndParseProg,
    parseDecl,
    parseExpr,
    readExpr,
  )
where

import Control.Monad.Except (MonadError (..), liftEither)
import Data.Bifunctor (first)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Vehicle.Syntax.AST
import Vehicle.Syntax.BNFC.Elaborate.External
import Vehicle.Syntax.External.Abs qualified as External (Expr, Prog)
import Vehicle.Syntax.External.Layout as External (resolveLayout)
import Vehicle.Syntax.External.Lex as External (Token)
import Vehicle.Syntax.External.Par as External (myLexer, pExpr, pProg)
import Vehicle.Syntax.Internal.Abs qualified as Internal (Expr, Prog)
import Vehicle.Syntax.Internal.Par as Internal (myLexer, pExpr, pProg)
import Vehicle.Syntax.Parse.Error (ParseError (..))

--------------------------------------------------------------------------------
-- Interface

readAndParseProg :: (MonadError ParseError m) => Module -> Text -> m PartiallyParsedProg
readAndParseProg modul txt =
  castBNFCError (partiallyElabProg modul) (parseExternalProg txt)

parseDecl :: (MonadError ParseError m) => Module -> PartiallyParsedDecl -> m (Decl Name Builtin)
parseDecl = elaborateDecl

parseExpr :: (MonadError ParseError m) => Module -> UnparsedExpr -> m (Expr Name Builtin)
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

parseInternalExpr :: Text -> Either String Internal.Expr
parseInternalExpr = Internal.pExpr . Internal.myLexer

parseInternalProg :: Text -> Either String Internal.Prog
parseInternalProg = Internal.pProg . Internal.myLexer

runExternalParser :: Bool -> ExternalParser a -> Text -> Either String a
runExternalParser topLevel p t = p (runExternalLexer topLevel t)

runExternalLexer :: Bool -> Text -> [External.Token]
runExternalLexer topLevel = External.resolveLayout topLevel . External.myLexer

castBNFCError :: (MonadError ParseError m) => (a -> m b) -> Either String a -> m b
castBNFCError f = \case
  Left err -> throwError $ RawParseError err
  Right value -> f value
