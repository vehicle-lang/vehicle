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
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry, runFreshFreeContextT)
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)
import Vehicle.Prelude
  ( BinderNamingForm (..),
    GenericArg (..),
    GenericBinder (..),
    GenericDecl (..),
    HasProvenance (..),
    Identifier,
    Position (..),
    Provenance (Provenance),
    Range (..),
    binderNamingForm,
    typeOf,
    wasInsertedByCompiler,
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
                      collectProgTokens (makeTokenThisFile filePath) prog

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

collectProgTokens :: (MonadCollectTokens m) => TokenMaker -> Prog Builtin -> m [IntermediateSemTok]
collectProgTokens makeToken (Main decls) =
  concat <$> traverse (collectDeclTokens makeToken) decls

collectDeclTokens :: (MonadCollectTokens m) => TokenMaker -> GenericDecl (Expr Builtin) -> m [IntermediateSemTok]
collectDeclTokens makeToken decl = do
  bodyToks <- getBodyToks
  headTok <- getHeadTok
  return $ headTok : bodyToks
  where
    getHeadTok = getDeclHeadToken makeToken (provenanceOf decl) decl

    getBodyToks = case decl of
      DefAbstract _ _ _ t -> collectExprTokens makeToken t
      DefFunction _ _ _ t e -> concat <$> traverse (collectExprTokens makeToken) ([t, e] :: [Expr Builtin])
      DefRecord _ _ _ t e _ -> do
        binderToks <- concat <$> traverse (collectBinderTokens makeToken) t
        fieldsToks <- collectFieldsTokens makeToken e
        return $ binderToks ++ fieldsToks

getDeclHeadToken :: (MonadCollectTokens m) => TokenMaker -> Provenance -> GenericDecl (Expr Builtin) -> m IntermediateSemTok
getDeclHeadToken makeToken p = \case
  DefAbstract _ _ _ t -> getTokenOfType makeToken p t
  DefFunction _ _ _ t _ -> getTokenOfType makeToken p t
  DefRecord {} -> pure $ makeToken p (SemanticTokenTypes_Class, [])

type SemanticTokenTypeComplete = (Lsp.SemanticTokenTypes, [Lsp.SemanticTokenModifiers])

getTokenOfType :: (MonadCollectTokens m) => TokenMaker -> Provenance -> Expr Builtin -> m IntermediateSemTok
getTokenOfType makeToken p = \case
  Universe {} -> pure $ makeToken p (SemanticTokenTypes_Type, [])
  Pi _ binder res ->
    if wasInsertedByCompiler binder
      then getTokenOfType makeToken p res
      else pure $ makeToken p (SemanticTokenTypes_Function, [])
  Builtin _ _builtin -> pure $ makeToken p (SemanticTokenTypes_Variable, [])
  BoundVar _ ix -> getBoundVarToken makeToken p ix
  FreeVar _ ident -> do
    decl <- getDeclEntry (Proxy @Builtin) ident
    case decl of
      DefAbstract _ _ _ t -> getTokenOfType makeToken p t
      DefFunction _ _ _ _ e -> getTokenOfType makeToken p e
      DefRecord {} -> pure $ makeToken p (SemanticTokenTypes_Class, [])
  Hole _ _name -> pure $ makeToken p (SemanticTokenTypes_Macro, []) -- ??
  Meta _ _metaId -> pure $ makeToken p (SemanticTokenTypes_Macro, []) -- ??
  App _f _args -> pure $ makeToken p (SemanticTokenTypes_Variable, [])
  Let _ _bound _bodyBinder _bodyExpr -> pure $ makeToken p (SemanticTokenTypes_Macro, []) -- ??
  Lam _ _binder _body -> pure $ makeToken p (SemanticTokenTypes_Function, [])
  Record _ _typ _fields -> pure $ makeToken p (SemanticTokenTypes_Macro, []) -- ??
  RecordProj _ _typ _body _fieldName -> pure $ makeToken p (SemanticTokenTypes_Macro, []) -- ??

collectBinderTokens :: (MonadCollectTokens m) => TokenMaker -> GenericBinder (Expr Builtin) -> m [IntermediateSemTok]
collectBinderTokens makeToken binder@(Binder _ _ _ val) = do
  exprToks <- collectExprTokens makeToken val
  nameTok <- getBinderNameToken makeToken binder
  return $ nameTok : exprToks

type TokenMaker = Provenance -> SemanticTokenTypeComplete -> IntermediateSemTok

getBinderNameToken :: (MonadCollectTokens m) => TokenMaker -> GenericBinder (Expr Builtin) -> m IntermediateSemTok
getBinderNameToken makeToken b = getTokenOfType makeToken nameProvenance (typeOf b)
  where
    -- `provenanceOf b` exists, but it mixes the `typeOf b` and `binderNamingForm b`'s provenances;
    -- we just want the provenance of the name.
    nameProvenance = case binderNamingForm b of
      NameAndType _n p -> p
      OnlyName _n p -> p
      OnlyType -> mempty

makeTokenThisFile :: FilePath -> Provenance -> SemanticTokenTypeComplete -> IntermediateSemTok
makeTokenThisFile filePath p@(Provenance _ provFilePath) t
  | filePath == provFilePath = makeTokenAnyFile p t
  | otherwise = Nothing

-- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
makeTokenAnyFile :: Provenance -> SemanticTokenTypeComplete -> IntermediateSemTok
makeTokenAnyFile (Provenance (Range (Position sl sc) (Position _el ec)) _) (tType, modifiers) =
  Just $
    Lsp.SemanticTokenAbsolute
      { _line = fromIntegral sl - 1,
        _startChar = fromIntegral sc - 1,
        _length = fromIntegral (ec - sc),
        _tokenType = tType,
        _tokenModifiers = modifiers
      }

-- drop `ix + 1` binders because we want to drop all binders up to and including binder `ix`
-- e.g. if we want to run in the context of before we added i2, we need to drop i0, i1, and i2 (3 total).
inBinderContext :: (MonadBoundContext (Expr Builtin) m) => Ix -> m a -> m a
inBinderContext ix = dropBindersFromContext (Proxy @(Expr Builtin)) (unIx ix + 1)

getBoundVarToken :: (MonadCollectTokens m) => TokenMaker -> Provenance -> Ix -> m IntermediateSemTok
getBoundVarToken makeToken p ix = do
  ctx <- getBoundCtx (Proxy @(Expr Builtin))
  case lookupIx ctx ix of
    Nothing -> return Nothing
    Just binder -> do
      binderNameToken <- inBinderContext ix $ getBinderNameToken makeTokenAnyFile binder
      case binderNameToken of
        Nothing -> return Nothing
        Just tok -> return $ makeToken p ((tok ^. tokenType), (tok ^. tokenModifiers))

getFreeVarToken :: (MonadCollectTokens m) => TokenMaker -> Provenance -> Identifier -> m IntermediateSemTok
getFreeVarToken makeToken p ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  getDeclHeadToken makeToken p decl

collectExprTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  Expr Builtin ->
  m [IntermediateSemTok]
collectExprTokens makeToken = \case
  Universe p _level -> pure [makeToken p (SemanticTokenTypes_Type, [])]
  Pi _p binder res -> do
    binderToks <- collectBinderTokens makeToken binder
    exprToks <- addBinderToContext binder $ collectExprTokens makeToken res
    return $ binderToks ++ exprToks
  Builtin p builtin -> collectBuiltinTokens makeToken p builtin
  BoundVar p ix -> singleton <$> getBoundVarToken makeToken p ix
  FreeVar p ident -> singleton <$> getFreeVarToken makeToken p ident
  Hole p _name -> pure [makeToken p (SemanticTokenTypes_Decorator, [])]
  Meta p _metaId -> pure [makeToken p (SemanticTokenTypes_TypeParameter, [])]
  App f args -> do
    fnToks <- collectExprTokens makeToken f
    argsToks <- concat <$> traverse (collectArgTokens makeToken) (NonEmpty.toList args)
    return $ fnToks ++ argsToks
  Let p bound bodyBinder bodyExpr -> do
    let letToks = [makeToken p (SemanticTokenTypes_Keyword, [])]
    boundToks <- collectExprTokens makeToken bound
    binderToks <- collectBinderTokens makeToken bodyBinder
    bodyToks <- addBinderToContext bodyBinder $ collectExprTokens makeToken bodyExpr
    return $ letToks ++ boundToks ++ binderToks ++ bodyToks
  Lam p binder body -> do
    let lamToks = [makeToken p (SemanticTokenTypes_Keyword, [])]
    binderToks <- collectBinderTokens makeToken binder
    bodyToks <- addBinderToContext binder $ collectExprTokens makeToken body
    return $ lamToks ++ binderToks ++ bodyToks
  Record p typ fields -> do
    let recordToks = [makeToken p (SemanticTokenTypes_Class, [])]
    typToks <- collectExprTokens makeToken typ
    fieldsToks <- collectFieldsTokens makeToken fields
    return $ recordToks ++ typToks ++ fieldsToks
  RecordProj p typ body _fieldName -> do
    let recordProjToks = [makeToken p (SemanticTokenTypes_Class, [])]
    typToks <- collectExprTokens makeToken typ
    bodyToks <- collectExprTokens makeToken body
    return $ recordProjToks ++ typToks ++ bodyToks

collectBuiltinTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  Provenance ->
  Builtin ->
  m [IntermediateSemTok]
collectBuiltinTokens makeToken p = \case
  BuiltinConstructor constructor -> case constructor of
    Nil -> tok SemanticTokenTypes_Keyword
    Cons -> tok SemanticTokenTypes_Operator
    UnitLiteral -> tok SemanticTokenTypes_Keyword
    IndexLiteral _ -> tok SemanticTokenTypes_Number
    NatLiteral _ -> tok SemanticTokenTypes_Number
    VectorLiteral -> tok SemanticTokenTypes_Keyword -- TODO: what is this?
    BoolTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Keyword tensor
    NatTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Number tensor
    RatTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Number tensor
  BuiltinFunction _fn -> tok SemanticTokenTypes_Operator -- TODO: may want to highlight different builtin funcs differently, e.g. as Function
  BuiltinType _typ -> tok SemanticTokenTypes_Type
  BuiltinCast _cast -> tok SemanticTokenTypes_Function
  DerivedFunction _fn -> tok SemanticTokenTypes_Function -- TODO: better parity with BuiltinFunction
  TypeClass _tc -> tok SemanticTokenTypes_Class
  TypeClassOp _tcOp -> tok SemanticTokenTypes_Operator -- TODO: may want to highlight different typeclass ops differently, e.g. as function (e.g. FoldTC) or type (e.g. TensorTypeTC)
  NatInDomainConstraint -> pure [] -- TODO: ??
  where
    tok typ = pure [makeToken p (typ, [])]

collectTensorLiteralTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  Provenance ->
  SemanticTokenTypes ->
  Tensor a ->
  m [IntermediateSemTok]
collectTensorLiteralTokens makeToken p _valueTokType = \case
  -- TODO: We need the provenences of the literal values, not just the whole literal
  DenseTensor _shape _vec -> pure [makeToken p (SemanticTokenTypes_Operator, [])]
  ConstantTensor _shape _a -> pure [makeToken p (SemanticTokenTypes_Operator, [])]

collectArgTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  Arg Builtin ->
  m [IntermediateSemTok]
collectArgTokens makeToken = collectExprTokens makeToken . argExpr

collectFieldsTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  RecordFields Builtin ->
  m [IntermediateSemTok]
collectFieldsTokens makeToken fields = concat <$> traverse collectFieldTokens fields
  where
    collectFieldTokens ::
      (MonadCollectTokens m) =>
      RecordField Builtin ->
      m [IntermediateSemTok]
    collectFieldTokens (field, fieldValue) = do
      valueToks <- collectExprTokens makeToken fieldValue
      return $ makeToken (provenanceOf field) (SemanticTokenTypes_Method, []) : valueToks
