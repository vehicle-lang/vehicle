{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Vehicle.LSP.Internal.Handlers (
    handlers,
) where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Language.LSP.Protocol.Message (SMethod (..))
import Language.LSP.Protocol.Types (ClientCapabilities)
import Language.LSP.Server qualified as Lsp
import Vehicle.LSP.Internal.Config (LspTc)

handlers ::
    LogAction LspTc (WithSeverity Text) ->
    ClientCapabilities ->
    Lsp.Handlers LspTc
handlers logger clientCapabilities =
    mconcat
        [ initializedHandler
        , textDocumentDidChangeHandler
        , workspaceDidChangeConfiguration
        ]
  where
    initializedHandler :: Lsp.Handlers LspTc
    initializedHandler =
        Lsp.notificationHandler SMethod_Initialized $ \_notification -> do
            logger <& (T.pack "ClientCapabilities: " <> TL.toStrict (encodeToLazyText clientCapabilities)) `WithSeverity` Debug
            config <- Lsp.getConfig
            logger <& (T.pack "Config: " <> TL.toStrict (encodeToLazyText config)) `WithSeverity` Debug
            pure ()

    textDocumentDidChangeHandler :: Lsp.Handlers LspTc
    textDocumentDidChangeHandler =
        Lsp.notificationHandler SMethod_TextDocumentDidChange $ \_notification -> do
            pure ()

    workspaceDidChangeConfiguration :: Lsp.Handlers LspTc
    workspaceDidChangeConfiguration =
        Lsp.notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_notification ->
            pure ()
