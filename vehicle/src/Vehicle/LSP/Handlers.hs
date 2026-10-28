{-# LANGUAGE ExplicitNamespaces #-}

module Vehicle.LSP.Handlers
  ( handlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Aeson.Text (encodeToLazyText)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (Method (..), SMethod (..), TResponseError (..))
import Language.LSP.Protocol.Types as Lsp
  ( ClientCapabilities,
    LSPErrorCodes (..),
    SemanticTokenAbsolute (..),
    SemanticTokenModifiers,
    SemanticTokenTypes (..),
    SemanticTokens,
    defaultSemanticTokensLegend,
    makeSemanticTokens,
    toNormalizedUri,
    uriToFilePath,
    pattern InL,
  )
import Language.LSP.Server (MonadLsp)
import Language.LSP.Server qualified as Lsp
import Vehicle.Compile.Print.Error (formatCompileError, logCompileError)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.AST.Prog (GenericProg (..), Prog)
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)
import Vehicle.Prelude
  ( BinderDisplayForm (..),
    BinderNamingForm (..),
    GenericArg (..),
    GenericBinder (..),
    GenericDecl (..),
    GenericRecordField,
    HasProvenance (..),
    Position (..),
    Provenance (Provenance),
    Range (..),
  )
import Vehicle.Prelude.Logging (runSilentLoggerT)
import Vehicle.Prelude.Prettyprinter (Pretty (..), layoutAsText)
import Vehicle.TypeCheck (loadUserSpecification)
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification)

type MonadVehicleLsp m =
  ( MonadLsp Config m,
    MonadReadSpecification m
  )

handlers ::
  forall m.
  (MonadVehicleLsp m) =>
  LogAction m (WithSeverity Text) ->
  Server ->
  Lsp.ClientCapabilities ->
  Lsp.Handlers m
handlers logger server clientCapabilities =
  mconcat
    [ Lsp.notificationHandler SMethod_Initialized initializedHandler,
      -- TextDocument notifications
      Lsp.notificationHandler SMethod_WorkspaceDidChangeConfiguration workspaceDidChangeConfigurationHandler,
      -- TextDocument notifications
      Lsp.notificationHandler SMethod_TextDocumentDidOpen textDocumentDidOpenHandler,
      Lsp.notificationHandler SMethod_TextDocumentDidChange textDocumentDidChangeHandler,
      Lsp.notificationHandler SMethod_TextDocumentDidSave textDocumentDidSaveHandler,
      Lsp.notificationHandler SMethod_TextDocumentDidClose textDocumentDidCloseHandler,
      -- TextDocument requests
      Lsp.requestHandler SMethod_TextDocumentSemanticTokensFull textDocumentSemanticTokensFull
    ]
  where
    initializedHandler :: Lsp.Handler m 'Method_Initialized
    initializedHandler _notification = do
      logger <& (T.pack "ClientCapabilities: " <> TL.toStrict (encodeToLazyText clientCapabilities)) `WithSeverity` Info
      config <- Lsp.getConfig
      logger <& (T.pack "Config: " <> TL.toStrict (encodeToLazyText config)) `WithSeverity` Info
      return ()

    workspaceDidChangeConfigurationHandler :: Lsp.Handler m 'Method_WorkspaceDidChangeConfiguration
    workspaceDidChangeConfigurationHandler _notification = do
      return ()

    textDocumentDidOpenHandler :: Lsp.Handler m 'Method_TextDocumentDidOpen
    textDocumentDidOpenHandler msg = do
      let doc = msg ^. params . textDocument
      let url = Lsp.toNormalizedUri (doc ^. uri)
      let ver = doc ^. version
      fileUpdated server ver url

    textDocumentDidChangeHandler :: Lsp.Handler m 'Method_TextDocumentDidChange
    textDocumentDidChangeHandler msg = do
      let doc = msg ^. params . textDocument
      let url = Lsp.toNormalizedUri (doc ^. uri)
      let ver = doc ^. version
      fileUpdated server ver url

    textDocumentDidCloseHandler :: Lsp.Handler m 'Method_TextDocumentDidClose
    textDocumentDidCloseHandler _notification = do
      return ()

    textDocumentDidSaveHandler :: Lsp.Handler m 'Method_TextDocumentDidSave
    textDocumentDidSaveHandler _notification = do
      return ()

    textDocumentSemanticTokensFull :: Lsp.Handler m Method_TextDocumentSemanticTokensFull
    textDocumentSemanticTokensFull request responder = do
      let doc = request ^. params . textDocument . uri
      let mFilePath = uriToFilePath doc
      case mFilePath of
        Nothing ->
          responder $
            Left $
              TResponseError
                { _code = InL LSPErrorCodes_RequestFailed,
                  _message = "File not found",
                  _xdata = Nothing
                }
        Just filePath -> do
          -- TODO: This means a specification has to successfully type check before we can
          -- produce semantic tokens for it, which sucks. We should just do scope checking
          -- here instead.
          (errorOrResult, _warnings) <- runSilentLoggerT $ logCompileError $ loadUserSpecification filePath

          let respondWithTokens toks =
                case makeLspSemanticTokens toks of
                  Left errorMessage ->
                    responder $
                      Left $
                        TResponseError
                          { _code = InL LSPErrorCodes_RequestFailed,
                            _message = errorMessage,
                            _xdata = Nothing
                          }
                  Right tokens ->
                    responder $ Right $ InL tokens

          case errorOrResult of
            Left err -> do
              logger <& (layoutAsText $ pretty $ formatCompileError err) `WithSeverity` Warning
              respondWithTokens []
            Right (prog, _, _) ->
              respondWithTokens $ progToSemanticTokens filePath prog

makeLspSemanticTokens :: [Maybe Lsp.SemanticTokenAbsolute] -> Either Text Lsp.SemanticTokens
makeLspSemanticTokens tokens =
  makeSemanticTokens defaultSemanticTokensLegend sortedTokens
  where
    sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) unsortedTokens
    unsortedTokens = catMaybes tokens

progToSemanticTokens :: FilePath -> Prog Builtin -> [Maybe Lsp.SemanticTokenAbsolute]
progToSemanticTokens filePath (Main decls) =
  concatMap (declToSemanticTokens filePath) decls

declToSemanticTokens :: FilePath -> GenericDecl (Expr Builtin) -> [Maybe Lsp.SemanticTokenAbsolute]
declToSemanticTokens filePath = \case
  DefAbstract p _ _ t -> [makeToken filePath p SemanticTokenTypes_Variable []] ++ exprsToSemanticTokens filePath [t]
  DefFunction p _ _ t e -> [makeToken filePath p SemanticTokenTypes_Function []] ++ exprsToSemanticTokens filePath [t, e]
  DefRecord p _ _ t e _ -> [makeToken filePath p SemanticTokenTypes_Function []] ++ concatMap (binderToSemanticTokens filePath) t ++ concatMap (genericRecordFieldToSemanticTokens filePath) e

genericRecordFieldToSemanticTokens :: FilePath -> GenericRecordField (Expr Builtin) -> [Maybe Lsp.SemanticTokenAbsolute]
genericRecordFieldToSemanticTokens filePath (fname, fexpr) =
  [makeToken filePath (provenanceOf fname) SemanticTokenTypes_Variable []]
    ++ exprsToSemanticTokens filePath [fexpr]

binderToSemanticTokens :: FilePath -> GenericBinder (Expr Builtin) -> [Maybe Lsp.SemanticTokenAbsolute]
binderToSemanticTokens filePath (Binder (BinderDisplayForm namingForm _) _ _ v) =
  case namingForm of
    NameAndType _ p -> [makeToken filePath p SemanticTokenTypes_Variable []] -- TODO: probably want to highlight name and type separately
    OnlyName _ p -> [makeToken filePath p SemanticTokenTypes_Variable []]
    OnlyType -> []
    ++ exprsToSemanticTokens filePath [v]

exprsToSemanticTokens :: FilePath -> [Expr Builtin] -> [Maybe Lsp.SemanticTokenAbsolute]
exprsToSemanticTokens filePath exprs =
  foldMap (collectTokens filePath) exprs

-- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
makeToken :: FilePath -> Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> Maybe Lsp.SemanticTokenAbsolute
makeToken filePath p@(Provenance (Range (Position sl sc) (Position _el ec)) _) tType modifiers =
  checkFile
    filePath
    p
    ( Lsp.SemanticTokenAbsolute
        { _line = fromIntegral sl - 1,
          _startChar = fromIntegral sc - 1,
          _length = fromIntegral (ec - sc),
          _tokenType = tType,
          _tokenModifiers = modifiers
        }
    )

checkFile :: FilePath -> Provenance -> x -> Maybe x
checkFile filePath (Provenance _ provFilePath) x
  | filePath == provFilePath = Just x
  | otherwise = Nothing

-- | Traverse the Expr AST and collect all leaf tokens.
collectTokens ::
  FilePath ->
  Expr Builtin ->
  [Maybe Lsp.SemanticTokenAbsolute]
collectTokens filePath expr = case expr of
  Universe p _level -> [makeToken filePath p SemanticTokenTypes_Type []]
  Pi _p binder res -> collectBinderTokens filePath binder ++ collectTokens filePath res
  Builtin p _builtinName -> [makeToken filePath p SemanticTokenTypes_Function []]
  BoundVar p _ix -> [makeToken filePath p SemanticTokenTypes_Variable []]
  FreeVar p _ident -> [makeToken filePath p SemanticTokenTypes_Variable []]
  Hole p _name -> [makeToken filePath p SemanticTokenTypes_Decorator []]
  Meta p _metaId -> [makeToken filePath p SemanticTokenTypes_TypeParameter []]
  App f args -> collectTokens filePath f ++ concatMap (collectArgTokens filePath) (NonEmpty.toList args)
  Let p bound bodyBinder bodyExpr -> [makeToken filePath p SemanticTokenTypes_Keyword []] ++ (collectTokens filePath bound) ++ collectBinderTokens filePath bodyBinder ++ collectTokens filePath bodyExpr
  Lam p binder body -> [makeToken filePath p SemanticTokenTypes_Macro []] ++ (collectBinderTokens filePath binder) ++ collectTokens filePath body
  Record p typ fields -> [makeToken filePath p SemanticTokenTypes_Class []] ++ (collectTokens filePath typ) ++ collectFieldsTokens filePath fields
  RecordProj p typ body _fieldName -> [makeToken filePath p SemanticTokenTypes_Class []] ++ (collectTokens filePath typ) ++ collectTokens filePath body

-- Stubbed functions for the datatypes not included in your snippet:
collectArgTokens ::
  FilePath ->
  Arg Builtin ->
  [Maybe Lsp.SemanticTokenAbsolute]
collectArgTokens filePath = collectTokens filePath . argExpr

collectBinderTokens ::
  FilePath ->
  Binder Builtin ->
  [Maybe Lsp.SemanticTokenAbsolute]
collectBinderTokens filePath = collectTokens filePath . binderValue

collectFieldsTokens ::
  FilePath ->
  RecordFields Builtin ->
  [Maybe Lsp.SemanticTokenAbsolute]
collectFieldsTokens filePath = concatMap $ \(field, fieldValue) ->
  makeToken filePath (provenanceOf field) SemanticTokenTypes_Method [] : collectTokens filePath fieldValue
