{-# LANGUAGE ExplicitNamespaces #-}

module Vehicle.LSP.Handlers
  ( handlers,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Aeson.Text (encodeToLazyText)
import Data.Functor ((<&>))
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
import Vehicle.Data.Builtin.Standard
  ( Builtin (..),
    BuiltinConstructor (..),
    BuiltinFunction (..),
    DerivedFunction (..),
    TypeClassOp (..),
  )
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Data.Variable.Bound.Context.Core (lookupIx)
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (..), runFreshBoundContextT)
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry, runFreshFreeContextT)
import Vehicle.LSP.Config (Config)
import Vehicle.LSP.State (Server, fileUpdated)
import Vehicle.Prelude
  ( BinderNamingForm (..),
    DefFunctionSort (..),
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
  headTok <- getHeadTok
  bodyToks <- getBodyToks
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
  DefAbstract _ _ _ t -> getTokenOfType makeToken p 0 t
  DefFunction _ _ s t _ -> do
    -- Record accesses (e.g. `pair.left`) should render the `.left` part as a Method, not a Function
    let fixProjectionTokenType tok = case s of
          ProjectionDecl {} -> tok {_tokenType = Lsp.SemanticTokenTypes_Method}
          _ -> tok
    getTokenOfType makeToken p 0 t <&> fmap fixProjectionTokenType
  DefRecord {} -> pure $ makeToken p (SemanticTokenTypes_Class, [])

type SemanticTokenTypeComplete = (Lsp.SemanticTokenTypes, [Lsp.SemanticTokenModifiers])

type BinderDepth = Int

getTokenOfType :: (MonadCollectTokens m) => TokenMaker -> Provenance -> BinderDepth -> Expr Builtin -> m IntermediateSemTok
getTokenOfType makeToken p depth = \case
  Universe {}
    | depth > 0 -> pure $ makeToken p (SemanticTokenTypes_Variable, [])
    | otherwise -> pure $ makeToken p (SemanticTokenTypes_Type, [])
  Pi _ binder res ->
    if wasInsertedByCompiler binder
      then addBinderToContext binder $ getTokenOfType makeToken p depth res
      else pure $ makeToken p (SemanticTokenTypes_Function, [])
  Builtin _ _builtin -> pure $ makeToken p (SemanticTokenTypes_Variable, [])
  -- We need to keep track of how many "binders deep" we are, so we can determine if a BoundVar is an
  -- element of Type, or merely an element of something which is an element of Type.
  --
  -- For example, consider the following:
  --
  --   identity (T : Type) (x : T) = x
  --
  -- T is a Type, x is a T; x is not a Type.
  --
  -- Without tracking BinderDepth, x would resolve to T which would resolve to Type (Universe), so x
  -- would be highlighted the same as T. Instead, when x resolves to T, it does so with an incremented
  -- BinderDepth, so when T resolves to Type, it knows it is not the first one in the binder chain.
  -- I'm sure there is a more elegant solution to this, and there are probably edge cases I haven't
  -- considered, but this will do for now.
  BoundVar _ ix -> getBoundVarToken makeToken p ix True (depth + 1)
  FreeVar _ ident -> do
    decl <- getDeclEntry (Proxy @Builtin) ident
    case decl of
      DefAbstract _ _ _ t -> getTokenOfType makeToken p depth t
      DefFunction _ _ _ _ e -> getTokenOfType makeToken p depth e
      DefRecord {} -> pure $ makeToken p (SemanticTokenTypes_Class, [])
  Hole _ _name -> pure $ makeToken p (SemanticTokenTypes_Macro, [])
  Meta _ _metaId -> pure $ makeToken p (SemanticTokenTypes_Macro, [])
  App _f _args -> pure $ makeToken p (SemanticTokenTypes_Variable, [])
  Let _ _bound bodyBinder bodyExpr -> addBinderToContext bodyBinder $ getTokenOfType makeToken p depth bodyExpr
  Lam _ binder body ->
    if wasInsertedByCompiler binder
      then addBinderToContext binder $ getTokenOfType makeToken p depth body
      else getTokenOfType makeToken p depth (typeOf binder)
  Record _ typ _fields -> getTokenOfType makeToken p depth typ
  RecordProj _ _typ _body _fieldName -> pure $ makeToken p (SemanticTokenTypes_Method, [])

collectBinderTokens :: (MonadCollectTokens m) => TokenMaker -> GenericBinder (Expr Builtin) -> m [IntermediateSemTok]
collectBinderTokens makeToken binder@(Binder _ _ _ val)
  | wasInsertedByCompiler binder = pure []
  | otherwise = do
      exprToks <- collectExprTokens makeToken val
      nameTok <- getBinderNameToken makeToken 0 binder
      return $ nameTok : exprToks

type TokenMaker = Provenance -> SemanticTokenTypeComplete -> IntermediateSemTok

getBinderNameToken :: (MonadCollectTokens m) => TokenMaker -> BinderDepth -> GenericBinder (Expr Builtin) -> m IntermediateSemTok
getBinderNameToken makeToken depth b = getTokenOfType makeToken nameProvenance depth (typeOf b)
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

getBoundVarToken :: (MonadCollectTokens m) => TokenMaker -> Provenance -> Ix -> Bool -> BinderDepth -> m IntermediateSemTok
getBoundVarToken makeToken p ix allowInsertedByCompiler depth = do
  ctx <- getBoundCtx (Proxy @(Expr Builtin))
  case lookupIx ctx ix of
    Nothing -> return Nothing
    Just binder -> do
      if not allowInsertedByCompiler && wasInsertedByCompiler binder
        then return Nothing
        else do
          binderNameToken <-
            Debug.Trace.trace (show $ "resolved " <+> pretty ix <+> " to " <+> prettyVerbose binder <+> " at " <+> pretty (provenanceOf binder) <+> " aka " <+> pretty (show binder)) $
              inBinderContext ix $
                getBinderNameToken makeTokenAnyFile depth binder
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
  BoundVar p ix -> singleton <$> getBoundVarToken makeToken p ix False 1
  FreeVar p ident -> singleton <$> getFreeVarToken makeToken p ident
  Hole p _name -> pure [makeToken p (SemanticTokenTypes_Decorator, [])]
  Meta p _metaId -> pure [makeToken p (SemanticTokenTypes_TypeParameter, [])]
  App f args -> do
    fnToks <- collectExprTokens makeToken f
    argsToks <- concat <$> traverse (collectArgTokens makeToken) (filter (not . wasInsertedByCompiler) (NonEmpty.toList args))
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
  Record _p _typ fields -> do
    fieldsToks <- collectFieldsTokens makeToken fields
    return fieldsToks
  RecordProj p _typ _body _fieldName -> pure [makeToken p (SemanticTokenTypes_Method, [])]

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
    VectorLiteral -> tok SemanticTokenTypes_Keyword
    BoolTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Keyword tensor
    NatTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Number tensor
    RatTensorLiteral tensor -> collectTensorLiteralTokens makeToken p SemanticTokenTypes_Number tensor
  -- TODO: It would be nice to be able to access the external syntax here. We want to say
  -- "if the external token is alphanumeric, use Keyword, otherwise use Operator".
  BuiltinFunction fn -> case fn of
    Not -> tok SemanticTokenTypes_Operator
    And -> tok SemanticTokenTypes_Operator
    Or -> tok SemanticTokenTypes_Operator
    Implies -> tok SemanticTokenTypes_Operator
    QuantifyRatTensor {} -> tok SemanticTokenTypes_Keyword
    QuantifyRecord {} -> tok SemanticTokenTypes_Keyword
    If -> tok SemanticTokenTypes_Keyword
    CompareIndex {} -> tok SemanticTokenTypes_Operator
    CompareNat {} -> tok SemanticTokenTypes_Operator
    CompareRatTensor {} -> tok SemanticTokenTypes_Operator
    ReduceAndTensor -> tok SemanticTokenTypes_Keyword
    ReduceOrTensor -> tok SemanticTokenTypes_Keyword
    -- Rat operations
    Add {} -> tok SemanticTokenTypes_Operator
    Mul {} -> tok SemanticTokenTypes_Operator
    Neg {} -> tok SemanticTokenTypes_Operator
    Sub {} -> tok SemanticTokenTypes_Operator
    Div {} -> tok SemanticTokenTypes_Operator
    Min {} -> tok SemanticTokenTypes_Operator
    Max {} -> tok SemanticTokenTypes_Operator
    Pow {} -> tok SemanticTokenTypes_Operator
    Log {} -> tok SemanticTokenTypes_Operator
    Exp {} -> tok SemanticTokenTypes_Operator
    ReduceAddRatTensor -> tok SemanticTokenTypes_Keyword
    ReduceMulRatTensor -> tok SemanticTokenTypes_Keyword
    ReduceMinRatTensor -> tok SemanticTokenTypes_Keyword
    ReduceMaxRatTensor -> tok SemanticTokenTypes_Keyword
    -- Tensor operations
    AtTensor -> tok SemanticTokenTypes_Operator
    StackTensor -> tok SemanticTokenTypes_Keyword
    ConstTensor -> tok SemanticTokenTypes_Keyword
    Iterate -> tok SemanticTokenTypes_Keyword
    ForeachTensor -> tok SemanticTokenTypes_Keyword
    Transpose -> tok SemanticTokenTypes_Keyword
    WhereTensor -> tok SemanticTokenTypes_Keyword
    SearchRatTensor -> tok SemanticTokenTypes_Keyword
    -- Vector operations
    AtVector -> tok SemanticTokenTypes_Operator
    ForeachVector -> tok SemanticTokenTypes_Keyword
    -- List operations
    FoldList -> tok SemanticTokenTypes_Keyword
    MapList -> tok SemanticTokenTypes_Keyword
    ReverseList -> tok SemanticTokenTypes_Keyword
    AppendList -> tok SemanticTokenTypes_Keyword
  BuiltinType _typ -> tok SemanticTokenTypes_Type
  BuiltinCast _cast -> tok SemanticTokenTypes_Function
  DerivedFunction fn -> case fn of
    TypeAnn -> tok SemanticTokenTypes_Operator
    QuantifyIndex {} -> tok SemanticTokenTypes_Keyword
    QuantifyInList {} -> tok SemanticTokenTypes_Keyword
  TypeClass _tc -> tok SemanticTokenTypes_Class
  TypeClassOp tcOp -> case tcOp of
    FromNatTC -> pure []
    FromRatTC -> pure []
    VecLiteralTC -> tok SemanticTokenTypes_Keyword
    NegTC -> tok SemanticTokenTypes_Operator
    AtTC -> tok SemanticTokenTypes_Operator
    MapTC -> tok SemanticTokenTypes_Keyword
    FoldTC -> tok SemanticTokenTypes_Keyword
    ForeachTC -> tok SemanticTokenTypes_Keyword
    TensorTypeTC -> tok SemanticTokenTypes_Type
  NatInDomainConstraint -> pure []
  where
    tok typ = pure [makeToken p (typ, [])]

collectTensorLiteralTokens ::
  (MonadCollectTokens m) =>
  TokenMaker ->
  Provenance ->
  SemanticTokenTypes ->
  Tensor a ->
  m [IntermediateSemTok]
collectTensorLiteralTokens makeToken p valueTokType = \case
  DenseTensor _shape _vec -> pure [makeToken p (valueTokType, [])]
  ConstantTensor _shape _a -> pure [makeToken p (valueTokType, [])]

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
