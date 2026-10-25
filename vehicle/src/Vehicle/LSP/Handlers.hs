{-# LANGUAGE ExplicitNamespaces #-}

module Vehicle.LSP.Handlers
  ( handlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (toList)
import Data.List
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
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)
import Vehicle.Prelude
  ( GenericArg (..),
    GenericBinder (..),
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
          case errorOrResult of
            Left err ->
              logger <& (layoutAsText $ pretty $ formatCompileError err) `WithSeverity` Warning
            Right (prog, _, _) ->
              case exprsToSemanticTokens filePath $ toList prog of
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

exprsToSemanticTokens :: FilePath -> [Expr Builtin] -> Either Text Lsp.SemanticTokens
exprsToSemanticTokens filePath exprs = do
  let unsortedTokens = foldMap (collectTokens $ onlyFile filePath) exprs
  let sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) unsortedTokens
  makeSemanticTokens defaultSemanticTokensLegend sortedTokens

-- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
makeToken :: Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> Lsp.SemanticTokenAbsolute
makeToken (Provenance (Range (Position sl sc) (Position _el ec)) _) tType modifiers =
  Lsp.SemanticTokenAbsolute
    { _line = fromIntegral sl - 1,
      _startChar = fromIntegral sc - 1,
      _length = fromIntegral (ec - sc),
      _tokenType = tType,
      _tokenModifiers = modifiers
    }

onlyFile :: FilePath -> Provenance -> [a] -> [a]
onlyFile filePath (Provenance _ provFilePath) lst
  | filePath == provFilePath = lst
  | otherwise = []

-- | Traverse the Expr AST and collect all leaf tokens.
collectTokens ::
  (Provenance -> [Lsp.SemanticTokenAbsolute] -> [Lsp.SemanticTokenAbsolute]) ->
  Expr Builtin ->
  [Lsp.SemanticTokenAbsolute]
collectTokens checkProv expr = case expr of
  Universe p _level -> checkProv p [makeToken p SemanticTokenTypes_Type []]
  Pi p binder res -> checkProv p $ collectBinderTokens checkProv binder ++ collectTokens checkProv res
  Builtin p _builtinName -> checkProv p [makeToken p SemanticTokenTypes_Function []]
  BoundVar p _ix -> checkProv p [makeToken p SemanticTokenTypes_Variable []]
  FreeVar p _ident -> checkProv p [makeToken p SemanticTokenTypes_Variable []]
  Hole p _name -> checkProv p [makeToken p SemanticTokenTypes_Decorator []]
  Meta p _metaId -> checkProv p [makeToken p SemanticTokenTypes_TypeParameter []]
  App f args -> collectTokens checkProv f ++ concatMap (collectArgTokens checkProv) (toList args)
  Let p bound bodyBinder bodyExpr -> checkProv p $ collectTokens checkProv bound ++ collectBinderTokens checkProv bodyBinder ++ collectTokens checkProv bodyExpr
  Lam p binder body -> checkProv p $ collectBinderTokens checkProv binder ++ collectTokens checkProv body
  Record p typ fields -> checkProv p $ collectTokens checkProv typ ++ collectFieldsTokens checkProv fields
  RecordProj p typ body _fieldName -> checkProv p $ collectTokens checkProv typ ++ collectTokens checkProv body

-- Stubbed functions for the datatypes not included in your snippet:
collectArgTokens ::
  (Provenance -> [Lsp.SemanticTokenAbsolute] -> [Lsp.SemanticTokenAbsolute]) ->
  Arg Builtin ->
  [Lsp.SemanticTokenAbsolute]
collectArgTokens checkProv = collectTokens checkProv . argExpr

collectBinderTokens ::
  (Provenance -> [Lsp.SemanticTokenAbsolute] -> [Lsp.SemanticTokenAbsolute]) ->
  Binder Builtin ->
  [Lsp.SemanticTokenAbsolute]
collectBinderTokens checkProv = collectTokens checkProv . binderValue

collectFieldsTokens ::
  (Provenance -> [Lsp.SemanticTokenAbsolute] -> [Lsp.SemanticTokenAbsolute]) ->
  RecordFields Builtin ->
  [Lsp.SemanticTokenAbsolute]
collectFieldsTokens checkProv = concatMap $ \(field, fieldValue) ->
  makeToken (provenanceOf field) SemanticTokenTypes_Method [] : collectTokens checkProv fieldValue
