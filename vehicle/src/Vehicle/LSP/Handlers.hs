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
import Data.Map (elems)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
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
import Vehicle.Data.Builtin.Standard (Builtin (..), BuiltinConstructor (..))
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Data.Variable.Bound.Context.Core (lookupIx)
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (..), runFreshBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry, runFreshFreeContextT)
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)
import Vehicle.Prelude
  ( BinderDisplayForm (..),
    BinderNamingForm (..),
    GenericArg (..),
    GenericBinder (..),
    GenericDecl (..),
    HasProvenance (..),
    Position (..),
    Provenance (Provenance),
    Range (..),
    Visibility (..),
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
          (errorOrToks, _warnings) <-
            runSilentLoggerT $ logCompileError $ do
              -- TODO: This means a specification has to successfully type check before we can
              -- produce semantic tokens for it, which sucks. We should just do scope checking
              -- here instead.
              (prog@(Main userDecls), builtinModules, _) <- loadUserSpecification filePath
              let allDecls = userDecls ++ concat (elems builtinModules)

              maybeToks <-
                runFreshFreeContextT (Proxy @Builtin) $
                  populateFreeContext allDecls $
                    runFreshBoundContextT (Proxy @(Expr Builtin)) $
                      collectProgTokens filePath prog

              return $ catMaybes maybeToks

          case errorOrToks of
            Left err -> do
              logger <& (layoutAsText $ pretty $ formatCompileError err) `WithSeverity` Warning
              respondWithTokens []
            Right toks -> do
              respondWithTokens $ toks
          where
            respondWithTokens :: [SemanticTokenAbsolute] -> m ()
            respondWithTokens toks =
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

type MonadCollectTokens m =
  ( MonadFreeContext Builtin m,
    MonadBoundContext (Expr Builtin) m
  )

makeLspSemanticTokens :: [Lsp.SemanticTokenAbsolute] -> Either Text Lsp.SemanticTokens
makeLspSemanticTokens tokens =
  makeSemanticTokens defaultSemanticTokensLegend sortedTokens
  where
    sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) tokens

type IntermediateSemTok = Maybe Lsp.SemanticTokenAbsolute

populateFreeContext :: (MonadFreeContext Builtin m) => [Decl Builtin] -> m a -> m a
populateFreeContext [] f = f
populateFreeContext (d : ds) f = addDeclToContext d $ populateFreeContext ds f

collectProgTokens :: (MonadCollectTokens m) => FilePath -> Prog Builtin -> m [IntermediateSemTok]
collectProgTokens filePath (Main decls) =
  concat <$> traverse (collectDeclTokens filePath) decls

collectDeclTokens :: (MonadCollectTokens m) => FilePath -> GenericDecl (Expr Builtin) -> m [IntermediateSemTok]
collectDeclTokens filePath decl = do
  bodyToks <- getBodyToks
  return $ headTok : bodyToks
  where
    headTok = uncurry (makeToken filePath (provenanceOf decl)) (getDeclHeadTokenType decl)

    getBodyToks = case decl of
      DefAbstract _ _ _ t -> collectExprTokens filePath t
      DefFunction _ _ _ t e -> concat <$> traverse (collectExprTokens filePath) ([t, e] :: [Expr Builtin])
      DefRecord _ _ _ t e _ -> do
        binderToks <- concat <$> traverse (collectBinderTokens filePath) t
        fieldsToks <- collectFieldsTokens filePath e
        return $ binderToks ++ fieldsToks

getDeclHeadTokenType :: GenericDecl (Expr Builtin) -> (Lsp.SemanticTokenTypes, [Lsp.SemanticTokenModifiers])
getDeclHeadTokenType = \case
  DefAbstract {} -> (SemanticTokenTypes_Function, [])
  DefFunction {} -> (SemanticTokenTypes_Function, [])
  DefRecord {} -> (SemanticTokenTypes_Class, [])

collectBinderTokens :: (MonadCollectTokens m) => FilePath -> GenericBinder (Expr Builtin) -> m [IntermediateSemTok]
collectBinderTokens filePath binder@(Binder _ _ _ val) = do
  exprToks <- collectExprTokens filePath val
  return $ getBinderNameToken filePath binder : exprToks

getBinderNameToken :: FilePath -> GenericBinder (Expr Builtin) -> IntermediateSemTok
getBinderNameToken filePath (Binder (BinderDisplayForm namingForm _foldingForm) vis _r _val)
  | insertedByCompiler vis = Nothing
  | otherwise = case namingForm of
      NameAndType _n p -> mkNameTok p -- TODO: probably want to highlight name and type separately
      OnlyName _n p -> mkNameTok p
      OnlyType -> Nothing
  where
    insertedByCompiler Explicit = False
    insertedByCompiler (Implicit b) = b
    insertedByCompiler (Instance b) = b

    -- TODO: Should use the same rules as decls to decide what token type to use
    mkNameTok p = makeToken filePath p SemanticTokenTypes_Variable []

makeToken :: FilePath -> Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> IntermediateSemTok
makeToken filePath p@(Provenance _ provFilePath) t m
  | filePath == provFilePath = makeTokenAnyFile p t m
  | otherwise = Nothing

-- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
makeTokenAnyFile :: Provenance -> Lsp.SemanticTokenTypes -> [Lsp.SemanticTokenModifiers] -> IntermediateSemTok
makeTokenAnyFile (Provenance (Range (Position sl sc) (Position _el ec)) _) tType modifiers =
  Just $
    Lsp.SemanticTokenAbsolute
      { _line = fromIntegral sl - 1,
        _startChar = fromIntegral sc - 1,
        _length = fromIntegral (ec - sc),
        _tokenType = tType,
        _tokenModifiers = modifiers
      }

-- TODO: We should special-case 0-argument functions to render as variables
collectExprTokens ::
  (MonadCollectTokens m) =>
  FilePath ->
  Expr Builtin ->
  m [IntermediateSemTok]
collectExprTokens filePath = \case
  Universe p _level -> pure [makeToken filePath p SemanticTokenTypes_Type []]
  Pi p binder res ->
    addBinderToContext binder $ do
      let piToks = [makeToken filePath p SemanticTokenTypes_Keyword []]
      binderToks <- collectBinderTokens filePath binder
      exprToks <- collectExprTokens filePath res
      return $ piToks ++ binderToks ++ exprToks
  Builtin p builtin -> collectBuiltinTokens filePath p builtin
  -- TODO: Bound context
  BoundVar p ix -> do
    ctx <- getBoundCtx (Proxy @(Expr Builtin))
    case lookupIx ctx ix of
      Nothing -> return []
      Just binder -> do
        case getBinderNameToken filePath binder of
          Nothing -> return []
          Just tok -> return [makeTokenAnyFile p (tok ^. tokenType) (tok ^. tokenModifiers)]
  FreeVar p ident -> do
    decl <- getDeclEntry (Proxy @Builtin) ident
    return [uncurry (makeTokenAnyFile p) (getDeclHeadTokenType decl)]
  Hole p _name -> pure [makeToken filePath p SemanticTokenTypes_Decorator []]
  Meta p _metaId -> pure [makeToken filePath p SemanticTokenTypes_TypeParameter []]
  App f args -> do
    fnToks <- collectExprTokens filePath f
    argsToks <- concat <$> traverse (collectArgTokens filePath) (NonEmpty.toList args)
    return $ fnToks ++ argsToks
  Let p bound bodyBinder bodyExpr ->
    addBinderToContext bodyBinder $ do
      let letToks = [makeToken filePath p SemanticTokenTypes_Keyword []]
      boundToks <- collectExprTokens filePath bound
      binderToks <- collectBinderTokens filePath bodyBinder
      bodyToks <- collectExprTokens filePath bodyExpr
      return $ letToks ++ boundToks ++ binderToks ++ bodyToks
  Lam p binder body -> do
    addBinderToContext binder $ do
      let lamToks = [makeToken filePath p SemanticTokenTypes_Macro []]
      binderToks <- collectBinderTokens filePath binder
      bodyToks <- collectExprTokens filePath body
      return $ lamToks ++ binderToks ++ bodyToks
  Record p typ fields -> do
    let recordToks = [makeToken filePath p SemanticTokenTypes_Class []]
    typToks <- collectExprTokens filePath typ
    fieldsToks <- collectFieldsTokens filePath fields
    return $ recordToks ++ typToks ++ fieldsToks
  RecordProj p typ body _fieldName -> do
    let recordProjToks = [makeToken filePath p SemanticTokenTypes_Class []]
    typToks <- collectExprTokens filePath typ
    bodyToks <- collectExprTokens filePath body
    return $ recordProjToks ++ typToks ++ bodyToks

collectBuiltinTokens ::
  (MonadCollectTokens m) =>
  FilePath ->
  Provenance ->
  Builtin ->
  m [IntermediateSemTok]
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
  NatInDomainConstraint -> pure [] -- TODO: ??
  where
    tok typ = pure [makeToken filePath p typ []]

collectTensorLiteralTokens ::
  (MonadCollectTokens m) =>
  FilePath ->
  Provenance ->
  SemanticTokenTypes ->
  Tensor a ->
  m [IntermediateSemTok]
collectTensorLiteralTokens filePath p _valueTokType = \case
  -- TODO: We need the provenences of the literal values, not just the whole literal
  DenseTensor _shape _vec -> pure [makeToken filePath p SemanticTokenTypes_Operator []]
  ConstantTensor _shape _a -> pure [makeToken filePath p SemanticTokenTypes_Operator []]

collectArgTokens ::
  (MonadCollectTokens m) =>
  FilePath ->
  Arg Builtin ->
  m [IntermediateSemTok]
collectArgTokens filePath = collectExprTokens filePath . argExpr

collectFieldsTokens ::
  (MonadCollectTokens m) =>
  FilePath ->
  RecordFields Builtin ->
  m [IntermediateSemTok]
collectFieldsTokens filePath fields = concat <$> traverse collectFieldTokens fields
  where
    collectFieldTokens ::
      (MonadCollectTokens m) =>
      RecordField Builtin ->
      m [IntermediateSemTok]
    collectFieldTokens (field, fieldValue) = do
      valueToks <- collectExprTokens filePath fieldValue
      return $ makeToken filePath (provenanceOf field) SemanticTokenTypes_Method [] : valueToks
