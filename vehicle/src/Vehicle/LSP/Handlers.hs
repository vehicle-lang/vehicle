{-# LANGUAGE ExplicitNamespaces #-}

module Vehicle.LSP.Handlers
  ( handlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message (Method (..), SMethod (..))
import Language.LSP.Protocol.Types as Lsp (ClientCapabilities, toNormalizedUri)
import Language.LSP.Server (MonadLsp)
import Language.LSP.Server qualified as Lsp
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)

type MonadVehicleLsp m =
  ( MonadLsp Config m
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
      -- Lsp.notificationHandler SMethod_TextDocumentDidChange textDocumentDidChangeHandler,
      Lsp.notificationHandler SMethod_TextDocumentDidSave textDocumentDidSaveHandler,
      Lsp.notificationHandler SMethod_TextDocumentDidClose textDocumentDidCloseHandler
      -- TextDocument requests
      -- Lsp.requestHandler SMethod_TextDocumentSemanticTokensFull textDocumentSemanticTokensFull
    ]
  where
    initializedHandler :: Lsp.Handler m Method_Initialized
    initializedHandler _notification = do
      logger <& (T.pack "ClientCapabilities: " <> TL.toStrict (encodeToLazyText clientCapabilities)) `WithSeverity` Info
      config <- Lsp.getConfig
      logger <& (T.pack "Config: " <> TL.toStrict (encodeToLazyText config)) `WithSeverity` Info
      return ()

    workspaceDidChangeConfigurationHandler :: Lsp.Handler m Method_WorkspaceDidChangeConfiguration
    workspaceDidChangeConfigurationHandler _notification = do
      return ()

    textDocumentDidOpenHandler :: Lsp.Handler m Method_TextDocumentDidOpen
    textDocumentDidOpenHandler msg = do
      let doc = msg ^. params . textDocument
      let url = Lsp.toNormalizedUri (doc ^. uri)
      let txt = doc ^. text
      let ver = doc ^. version
      fileUpdated server ver url txt
    {-
        textDocumentDidChangeHandler :: Lsp.Handler m Method_TextDocumentDidChange
        textDocumentDidChangeHandler msg = do
          let doc = msg ^. params . textDocument
          let url = Lsp.toNormalizedUri (doc ^. uri)
          let txt = doc ^. text
          let ver = doc ^. version
          fileUpdated server ver url txt
    -}
    textDocumentDidCloseHandler :: Lsp.Handler m Method_TextDocumentDidClose
    textDocumentDidCloseHandler _notification = do
      return ()

    textDocumentDidSaveHandler :: Lsp.Handler m Method_TextDocumentDidSave
    textDocumentDidSaveHandler _notification = do
      return ()

{-
    textDocumentSemanticTokensFull :: Lsp.Handler m Method_TextDocumentSemanticTokensFull
    textDocumentSemanticTokensFull request responder = do
      let doc = request ^. Lsp.params . Lsp.textDocument . Lsp.uri
      mdoc <- Lsp.getVirtualFile (Lsp.toNormalizedUri doc)
      case mdoc of
        Nothing ->
          responder $
            Left $
              TResponseError
                { _code = InL LSPErrorCodes_RequestFailed,
                  _message = "File not found",
                  _xdata = Nothing
                }
        Just vfile -> do
          let src = virtualFileText vfile
          expr <- _

          case exprToSemanticTokens _ of
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

-- | Main entry point to convert an Expr into LSP SemanticTokens
exprToSemanticTokens :: Expr Builtin -> Either Text Lsp.SemanticTokens
exprToSemanticTokens expr = do
  let unsortedTokens = collectTokens expr
  let sortedTokens = sortOn (\t -> (t ^. Lsp.line, t ^. Lsp.startChar)) unsortedTokens
  makeSemanticTokens defaultSemanticTokensLegend sortedTokens

-- \| Helper to convert your Provenance into an LSP Absolute Token.
-- NOTE: LSP expects 0-indexed lines and columns. If your custom language
-- is 1-indexed, you will need to subtract 1 from 'sl' and 'sc' here.
makeToken :: Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> Lsp.SemanticTokenAbsolute
makeToken (Provenance (Range (Position sl sc) (Position _el ec)) _) tType modifiers =
  Lsp.SemanticTokenAbsolute
    { _line = fromIntegral sl,
      _startChar = fromIntegral sc,
      _length = fromIntegral (ec - sc),
      _tokenType = tType,
      _tokenModifiers = modifiers
    }

-- | Traverse the Expr AST and collect all leaf tokens.
collectTokens :: Expr Builtin -> [Lsp.SemanticTokenAbsolute]
collectTokens expr = case expr of
  Universe p _level -> [makeToken p SemanticTokenTypes_Type []]
  Pi _prov binder res -> collectBinderTokens binder ++ collectTokens res
  Builtin p _builtinName -> [makeToken p SemanticTokenTypes_Function []]
  BoundVar p _ix -> [makeToken p SemanticTokenTypes_Variable []]
  FreeVar p _ident -> [makeToken p SemanticTokenTypes_Variable []]
  Hole p _name -> [makeToken p SemanticTokenTypes_Decorator []]
  Meta p _metaId -> [makeToken p SemanticTokenTypes_TypeParameter []]
  App f args -> collectTokens f ++ concatMap collectArgTokens (NonEmpty.toList args)
  Let _prov bound bodyBinder bodyExpr -> collectTokens bound ++ collectBinderTokens bodyBinder ++ collectTokens bodyExpr
  Lam _prov binder body -> collectBinderTokens binder ++ collectTokens body
  Record _prov typ fields -> collectTokens typ ++ collectFieldsTokens fields
  RecordProj _prov typ body _fieldName -> collectTokens typ ++ collectTokens body

-- Stubbed functions for the datatypes not included in your snippet:
collectArgTokens :: Arg Builtin -> [Lsp.SemanticTokenAbsolute]
collectArgTokens = collectTokens . argExpr

collectBinderTokens :: Binder Builtin -> [Lsp.SemanticTokenAbsolute]
collectBinderTokens = collectTokens . binderValue

collectFieldsTokens :: RecordFields Builtin -> [Lsp.SemanticTokenAbsolute]
collectFieldsTokens = concatMap $ \(field, fieldValue) ->
  makeToken (provenanceOf field) SemanticTokenTypes_Method [] : collectTokens fieldValue
      -}
