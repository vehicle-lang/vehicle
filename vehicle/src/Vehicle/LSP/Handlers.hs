{-# LANGUAGE ExplicitNamespaces #-}

module Vehicle.LSP.Handlers
  ( handlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Aeson.Text (encodeToLazyText)
-- import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Data.List
import Data.List.NonEmpty qualified as NonEmpty (toList)
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
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Print.Error (formatCompileError, logCompileError)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.AST.Prog (GenericProg (..), Prog)
import Vehicle.Data.Builtin.Standard (Builtin (..), BuiltinConstructor (..))
import Vehicle.Data.Tensor (Tensor (..))
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
            Right (prog, _, _) -> do
              let badProvOrToks = progToSemanticTokens filePath prog
              let (badProvs, toks) = partitionEithers badProvOrToks
              let (Main decls) = prog
              logger <& (layoutAsText $ prettyVerbose decls) `WithSeverity` Debug
              logger <& (layoutAsText $ pretty $ badProvs) `WithSeverity` Debug
              logger <& (layoutAsText $ pretty $ map (\t -> (t ^. line, t ^. startChar, t ^. tokenType)) toks) `WithSeverity` Debug
              respondWithTokens $ toks

makeLspSemanticTokens :: [Lsp.SemanticTokenAbsolute] -> Either Text Lsp.SemanticTokens
makeLspSemanticTokens tokens =
  makeSemanticTokens defaultSemanticTokensLegend sortedTokens
  where
    sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) tokens

type IntermediateSemTok = Either (Provenance, Lsp.SemanticTokenTypes) Lsp.SemanticTokenAbsolute

progToSemanticTokens :: FilePath -> Prog Builtin -> [IntermediateSemTok]
progToSemanticTokens filePath (Main decls) =
  concatMap (declToSemanticTokens filePath) decls

declToSemanticTokens :: FilePath -> GenericDecl (Expr Builtin) -> [IntermediateSemTok]
declToSemanticTokens filePath = \case
  DefAbstract p _ _ t -> [makeToken filePath p SemanticTokenTypes_Function []] ++ exprsToSemanticTokens filePath [t]
  DefFunction p _ _ t e -> [makeToken filePath p SemanticTokenTypes_Function []] ++ exprsToSemanticTokens filePath [t, e]
  DefRecord p _ _ t e _ -> [makeToken filePath p SemanticTokenTypes_Class []] ++ concatMap (binderToSemanticTokens filePath) t ++ concatMap (genericRecordFieldToSemanticTokens filePath) e

genericRecordFieldToSemanticTokens :: FilePath -> GenericRecordField (Expr Builtin) -> [IntermediateSemTok]
genericRecordFieldToSemanticTokens filePath (fname, fexpr) =
  [makeToken filePath (provenanceOf fname) SemanticTokenTypes_Variable []]
    ++ exprsToSemanticTokens filePath [fexpr]

binderToSemanticTokens :: FilePath -> GenericBinder (Expr Builtin) -> [IntermediateSemTok]
binderToSemanticTokens filePath (Binder (BinderDisplayForm namingForm _) _ _ v) =
  case namingForm of
    NameAndType _ p -> [makeToken filePath p SemanticTokenTypes_Variable []] -- TODO: probably want to highlight name and type separately
    OnlyName _ p -> [makeToken filePath p SemanticTokenTypes_Variable []]
    OnlyType -> []
    ++ exprsToSemanticTokens filePath [v]

exprsToSemanticTokens :: FilePath -> [Expr Builtin] -> [IntermediateSemTok]
exprsToSemanticTokens filePath exprs =
  foldMap (collectTokens filePath) exprs

-- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
makeToken :: FilePath -> Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> IntermediateSemTok
makeToken filePath p@(Provenance (Range (Position sl sc) (Position _el ec)) provFilePath) tType modifiers
  | filePath == provFilePath =
      Right
        ( Lsp.SemanticTokenAbsolute
            { _line = fromIntegral sl - 1,
              _startChar = fromIntegral sc - 1,
              _length = fromIntegral (ec - sc),
              _tokenType = tType,
              _tokenModifiers = modifiers
            }
        )
  | otherwise = Left (p, tType)

-- | Traverse the Expr AST and collect all leaf tokens.
collectTokens ::
  FilePath ->
  Expr Builtin ->
  [IntermediateSemTok]
collectTokens filePath expr = case expr of
  Universe p _level -> [makeToken filePath p SemanticTokenTypes_Type []]
  Pi _p binder res -> collectBinderTokens filePath binder ++ collectTokens filePath res
  Builtin p builtin -> collectBuiltinTokens filePath p builtin
  BoundVar p _ix -> [makeToken filePath p SemanticTokenTypes_Variable []]
  FreeVar p _ident -> [makeToken filePath p SemanticTokenTypes_Variable []]
  Hole p _name -> [makeToken filePath p SemanticTokenTypes_Decorator []]
  Meta p _metaId -> [makeToken filePath p SemanticTokenTypes_TypeParameter []]
  App f args -> collectTokens filePath f ++ concatMap (collectArgTokens filePath) (NonEmpty.toList args)
  Let p bound bodyBinder bodyExpr -> [makeToken filePath p SemanticTokenTypes_Keyword []] ++ (collectTokens filePath bound) ++ collectBinderTokens filePath bodyBinder ++ collectTokens filePath bodyExpr
  Lam p binder body -> [makeToken filePath p SemanticTokenTypes_Macro []] ++ (collectBinderTokens filePath binder) ++ collectTokens filePath body
  Record p typ fields -> [makeToken filePath p SemanticTokenTypes_Class []] ++ (collectTokens filePath typ) ++ collectFieldsTokens filePath fields
  RecordProj p typ body _fieldName -> [makeToken filePath p SemanticTokenTypes_Class []] ++ (collectTokens filePath typ) ++ collectTokens filePath body

collectBuiltinTokens ::
  FilePath ->
  Provenance ->
  Builtin ->
  [IntermediateSemTok]
collectBuiltinTokens filePath p = \case
  BuiltinConstructor constructor -> case constructor of
    Nil -> tok SemanticTokenTypes_Keyword
    Cons -> tok SemanticTokenTypes_Operator
    UnitLiteral -> tok SemanticTokenTypes_Keyword
    IndexLiteral _ -> tok SemanticTokenTypes_Number
    NatLiteral _ -> tok SemanticTokenTypes_Number
    VectorLiteral -> tok SemanticTokenTypes_Keyword -- TODO: what is this?
    BoolTensorLiteral tensor -> collectTensorLiteralTokens filePath p SemanticTokenTypes_Keyword tensor
    NatTensorLiteral tensor -> collectTensorLiteralTokens filePath p SemanticTokenTypes_Number tensor
    RatTensorLiteral tensor -> collectTensorLiteralTokens filePath p SemanticTokenTypes_Number tensor
  BuiltinFunction _fn -> tok SemanticTokenTypes_Operator -- TODO: may want to highlight different builtin funcs differently, e.g. as Function
  BuiltinType _typ -> tok SemanticTokenTypes_Type
  BuiltinCast _cast -> tok SemanticTokenTypes_Function
  DerivedFunction _fn -> tok SemanticTokenTypes_Function -- TODO: better parity with BuiltinFunction
  TypeClass _tc -> tok SemanticTokenTypes_Class
  TypeClassOp _tcOp -> tok SemanticTokenTypes_Operator
  NatInDomainConstraint -> [] -- TODO: ??
  where
    tok typ = [makeToken filePath p typ []]

collectTensorLiteralTokens ::
  FilePath ->
  Provenance ->
  SemanticTokenTypes ->
  Tensor a ->
  [IntermediateSemTok]
collectTensorLiteralTokens filePath p _valueTokType = \case
  -- TODO: We need the provenences of the literal values, not just the whole literal
  DenseTensor _shape _vec -> [makeToken filePath p SemanticTokenTypes_Operator []]
  ConstantTensor _shape _a -> [makeToken filePath p SemanticTokenTypes_Operator []]

collectArgTokens ::
  FilePath ->
  Arg Builtin ->
  [IntermediateSemTok]
collectArgTokens filePath = collectTokens filePath . argExpr

collectBinderTokens ::
  FilePath ->
  Binder Builtin ->
  [IntermediateSemTok]
collectBinderTokens filePath = collectTokens filePath . binderValue

collectFieldsTokens ::
  FilePath ->
  RecordFields Builtin ->
  [IntermediateSemTok]
collectFieldsTokens filePath = concatMap $ \(field, fieldValue) ->
  makeToken filePath (provenanceOf field) SemanticTokenTypes_Method [] : collectTokens filePath fieldValue
