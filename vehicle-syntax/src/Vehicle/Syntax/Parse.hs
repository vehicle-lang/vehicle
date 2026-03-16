module Vehicle.Syntax.Parse
  ( parseExternalModule,
  )
where

import Data.Text (Text)
import Vehicle.Syntax.External.Abs qualified as External (Module)
import Vehicle.Syntax.External.Layout as External (resolveLayout)
import Vehicle.Syntax.External.Lex as External (Token)
import Vehicle.Syntax.External.Par as External (myLexer, pModule)

--------------------------------------------------------------------------------
-- Parsing

type ExternalParser a = [External.Token] -> Either String a

parseExternalModule :: Text -> Either String External.Module
parseExternalModule = runExternalParser True External.pModule

runExternalParser :: Bool -> ExternalParser a -> Text -> Either String a
runExternalParser topLevel p t = p (runExternalLexer topLevel t)

runExternalLexer :: Bool -> Text -> [External.Token]
runExternalLexer topLevel = External.resolveLayout topLevel . External.myLexer

{-
parseExternalExpr :: Text -> Either String External.Expr
parseExternalExpr = runExternalParser False External.pExpr
-}
