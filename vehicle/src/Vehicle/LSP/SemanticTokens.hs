module Vehicle.LSP.SemanticTokens
  ( collectSemanticTokens,
    LspErrorMessage,
  )
where

import Colog.Core (LogAction, Severity (..), WithSeverity (..), (<&))
import Control.Lens ((^.))
import Data.Functor ((<&>))
import Data.List
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Map (elems)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenAbsolute (..),
    SemanticTokenModifiers,
    SemanticTokenTypes (..),
    SemanticTokens,
    defaultSemanticTokensLegend,
    makeSemanticTokens,
  )
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
import Vehicle.Data.Variable.Bound.Context.Core (lookupIx)
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (..), runFreshBoundContextT)
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry, runFreshFreeContextT)
import Vehicle.LSP.State (MonadVehicleLsp)
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

--------------------------------------------------------------------------------
-- API

-- Inherited from `Language.LSP.Protocol.Types.SemanticTokens.makeSemanticTokens`,
-- type-aliased for readability.
type LspErrorMessage = Text

collectSemanticTokens ::
  (MonadVehicleLsp m) =>
  LogAction m (WithSeverity Text) ->
  FilePath ->
  m (Either LspErrorMessage Lsp.SemanticTokens)
collectSemanticTokens logger filePath = do
  (errorOrToks, _warnings) <-
    runSilentLoggerT $ logCompileError $ do
      -- TODO: This means a specification has to successfully type check before we can
      -- produce semantic tokens for it, which sucks. We should just do scope checking
      -- here instead.
      (prog@(Main userDecls), builtinModules, _) <- loadUserSpecification filePath
      let allDecls = userDecls ++ concat (elems builtinModules)

      imToks <-
        runFreshFreeContextT (Proxy @Builtin) $
          populateFreeContext allDecls $
            runFreshBoundContextT (Proxy @(Expr Builtin)) $
              collectProgTokens prog

      return $ catMaybes $ map (makeTokenThisFile filePath) imToks

  case errorOrToks of
    Left err -> do
      logger <& (layoutAsText $ pretty $ formatCompileError err) `WithSeverity` Warning
      return $ makeLspSemanticTokens []
    Right toks ->
      return $ makeLspSemanticTokens toks
  where
    makeLspSemanticTokens :: [Lsp.SemanticTokenAbsolute] -> Either LspErrorMessage Lsp.SemanticTokens
    makeLspSemanticTokens tokens = do
      let sortedTokens = sortOn (\t -> (t ^. line, t ^. startChar)) tokens
      makeSemanticTokens defaultSemanticTokensLegend sortedTokens

    populateFreeContext :: (MonadFreeContext Builtin n) => [Decl Builtin] -> n a -> n a
    populateFreeContext [] f = f
    populateFreeContext (d : ds) f = addDeclToContext d $ populateFreeContext ds f

--------------------------------------------------------------------------------
-- Types and Utilities

type MonadCollectTokens m =
  ( MonadFreeContext Builtin m,
    MonadBoundContext (Expr Builtin) m
  )

type IntermediateSemTok = Maybe (Provenance, TokenClass)

type TokenClass = (Lsp.SemanticTokenTypes, [Lsp.SemanticTokenModifiers])

type BinderDepth = Int

makeTokenThisFile :: FilePath -> IntermediateSemTok -> Maybe Lsp.SemanticTokenAbsolute
makeTokenThisFile filePath = (makeToken =<<)
  where
    makeToken :: (Provenance, TokenClass) -> Maybe Lsp.SemanticTokenAbsolute
    makeToken ((Provenance (Range (Position sl sc) (Position _el ec)) provFilePath), (tType, modifiers))
      | filePath == provFilePath =
          Just $
            -- LSP expects 0-indexed lines and columns but we use 1-indexed, so subtract 1 from 'sl' and 'sc'
            Lsp.SemanticTokenAbsolute
              { _line = fromIntegral sl - 1,
                _startChar = fromIntegral sc - 1,
                _length = fromIntegral (ec - sc),
                _tokenType = tType,
                _tokenModifiers = modifiers
              }
      | otherwise = Nothing

-- drop `ix + 1` binders because we want to drop all binders up to and including binder `ix`
-- e.g. if we want to run in the context of before we added i2, we need to drop i0, i1, and i2 (3 total).
inBinderContext :: (MonadBoundContext (Expr Builtin) m) => Ix -> m a -> m a
inBinderContext ix = dropBindersFromContext (Proxy @(Expr Builtin)) (unIx ix + 1)

emitAt :: Provenance -> Maybe TokenClass -> IntermediateSemTok
emitAt p = (mkTok p =<<)

mkTok :: Provenance -> TokenClass -> IntermediateSemTok
mkTok p tok = Just (p, tok)

--------------------------------------------------------------------------------
-- Token collection:
--
-- We have an arbitrary expression; we need to identify and colour every token
-- inside of it.

collectProgTokens :: (MonadCollectTokens m) => Prog Builtin -> m [IntermediateSemTok]
collectProgTokens (Main decls) =
  concat <$> traverse collectDeclTokens decls

collectDeclTokens :: (MonadCollectTokens m) => GenericDecl (Expr Builtin) -> m [IntermediateSemTok]
collectDeclTokens decl = do
  headTok <- getHeadToks
  bodyToks <- getBodyToks
  return $ headTok : bodyToks
  where
    getHeadToks = emitAt (provenanceOf decl) <$> classifyDeclHead decl

    getBodyToks = case decl of
      DefAbstract _ _ _ t -> collectExprTokens t
      DefFunction _ _ _ t e -> concat <$> traverse collectExprTokens ([t, e] :: [Expr Builtin])
      DefRecord _ _ _ t e _ -> do
        binderToks <- concat <$> traverse collectBinderTokens t
        fieldsToks <- collectFieldsTokens e
        return $ binderToks ++ fieldsToks

collectBinderTokens :: (MonadCollectTokens m) => GenericBinder (Expr Builtin) -> m [IntermediateSemTok]
collectBinderTokens binder@(Binder _ _ _ val)
  | wasInsertedByCompiler binder = return []
  | otherwise = do
      exprToks <- collectExprTokens val
      nameTok <- getNameTok
      return $ nameTok : exprToks
  where
    getNameTok = emitAt nameProvenance <$> classifyBinderName 0 binder

    -- `provenanceOf b` exists, but it mixes the `typeOf b` and `binderNamingForm b`'s provenances;
    -- we just want the provenance of the name.
    nameProvenance = case binderNamingForm binder of
      NameAndType _n p -> p
      OnlyName _n p -> p
      OnlyType -> mempty

collectExprTokens ::
  (MonadCollectTokens m) =>
  Expr Builtin ->
  m [IntermediateSemTok]
collectExprTokens = \case
  Universe p _level -> return [mkTok p (SemanticTokenTypes_Type, [])]
  Pi _p binder res -> do
    binderToks <- collectBinderTokens binder
    exprToks <- addBinderToContext binder $ collectExprTokens res
    return $ binderToks ++ exprToks
  Builtin p builtin -> return $ [emitAt p (getBuiltinToken builtin)]
  BoundVar p ix -> singleton <$> emitAt p <$> classifyBoundVar 0 ix
  FreeVar p ident -> singleton <$> emitAt p <$> classifyFreeVar ident
  Hole p _name -> return [mkTok p (SemanticTokenTypes_Decorator, [])]
  Meta p _metaId -> return [mkTok p (SemanticTokenTypes_TypeParameter, [])]
  App f args -> do
    fnToks <- collectExprTokens f
    argsToks <- concat <$> traverse collectArgTokens (filter (not . wasInsertedByCompiler) (NonEmpty.toList args))
    return $ fnToks ++ argsToks
  Let p bound bodyBinder bodyExpr -> do
    let letToks = [mkTok p (SemanticTokenTypes_Keyword, [])]
    boundToks <- collectExprTokens bound
    binderToks <- collectBinderTokens bodyBinder
    bodyToks <- addBinderToContext bodyBinder $ collectExprTokens bodyExpr
    return $ letToks ++ boundToks ++ binderToks ++ bodyToks
  Lam p binder body -> do
    let lamToks = [mkTok p (SemanticTokenTypes_Keyword, [])]
    binderToks <- collectBinderTokens binder
    bodyToks <- addBinderToContext binder $ collectExprTokens body
    return $ lamToks ++ binderToks ++ bodyToks
  Record _p _typ fields -> do
    fieldsToks <- collectFieldsTokens fields
    return fieldsToks
  RecordProj p _typ _body _fieldName -> return [mkTok p (SemanticTokenTypes_Method, [])]

collectArgTokens ::
  (MonadCollectTokens m) =>
  Arg Builtin ->
  m [IntermediateSemTok]
collectArgTokens = collectExprTokens . argExpr

collectFieldsTokens ::
  (MonadCollectTokens m) =>
  RecordFields Builtin ->
  m [IntermediateSemTok]
collectFieldsTokens fields = concat <$> traverse collectFieldTokens fields
  where
    collectFieldTokens ::
      (MonadCollectTokens m) =>
      RecordField Builtin ->
      m [IntermediateSemTok]
    collectFieldTokens (field, fieldValue) = do
      valueToks <- collectExprTokens fieldValue
      return $ mkTok (provenanceOf field) (SemanticTokenTypes_Method, []) : valueToks

-- Builtins are always a single token and cannot be free/bound variables, so this function
-- can be pure.
getBuiltinToken :: Builtin -> Maybe TokenClass
getBuiltinToken = \case
  BuiltinConstructor constructor -> case constructor of
    Nil -> tok SemanticTokenTypes_Keyword
    Cons -> tok SemanticTokenTypes_Operator
    UnitLiteral -> tok SemanticTokenTypes_Keyword
    IndexLiteral _ -> tok SemanticTokenTypes_Number
    NatLiteral _ -> tok SemanticTokenTypes_Number
    VectorLiteral -> tok SemanticTokenTypes_Keyword
    BoolTensorLiteral _ -> tok SemanticTokenTypes_Keyword
    NatTensorLiteral _ -> tok SemanticTokenTypes_Number
    RatTensorLiteral _ -> tok SemanticTokenTypes_Number
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
    FromNatTC -> Nothing
    FromRatTC -> Nothing
    VecLiteralTC -> tok SemanticTokenTypes_Keyword
    NegTC -> tok SemanticTokenTypes_Operator
    AtTC -> tok SemanticTokenTypes_Operator
    MapTC -> tok SemanticTokenTypes_Keyword
    FoldTC -> tok SemanticTokenTypes_Keyword
    ForeachTC -> tok SemanticTokenTypes_Keyword
    TensorTypeTC -> tok SemanticTokenTypes_Type
  NatInDomainConstraint -> Nothing
  where
    tok typ = Just (typ, [])

--------------------------------------------------------------------------------
-- Token classification:
--
-- We have a single token `foo` of type `bar`, where `bar` is an arbitrary
-- expression; we need to work out what colour to make `foo`.

classifyTokenOfType :: (MonadCollectTokens m) => BinderDepth -> Expr Builtin -> m (Maybe TokenClass)
classifyTokenOfType depth = \case
  Universe {}
    | depth > 0 -> return $ Just (SemanticTokenTypes_Variable, [])
    | otherwise -> return $ Just (SemanticTokenTypes_Type, [])
  Pi _ binder res ->
    if wasInsertedByCompiler binder
      then addBinderToContext binder $ classifyTokenOfType depth res
      else return $ Just (SemanticTokenTypes_Function, [])
  Builtin _ _builtin -> return $ Just (SemanticTokenTypes_Variable, [])
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
  BoundVar _ ix -> classifyBoundVar (depth + 1) ix
  FreeVar _ ident -> do
    decl <- getDeclEntry (Proxy @Builtin) ident
    case decl of
      DefAbstract {} -> return $ Just (SemanticTokenTypes_Variable, [])
      DefFunction _ _ _ _ e -> classifyTokenOfType depth e
      DefRecord {} -> return $ Just (SemanticTokenTypes_Class, [])
  Hole _ _name -> return $ Just (SemanticTokenTypes_Macro, [])
  Meta _ _metaId -> return $ Just (SemanticTokenTypes_Macro, [])
  App f _args -> classifyTokenOfType depth f
  Let _ _bound bodyBinder bodyExpr -> addBinderToContext bodyBinder $ classifyTokenOfType depth bodyExpr
  Lam _ binder body -> addBinderToContext binder $ classifyTokenOfType depth body
  Record _ typ _fields -> classifyTokenOfType depth typ
  RecordProj _ _typ _body _fieldName -> return $ Just (SemanticTokenTypes_Method, [])

classifyDeclHead :: (MonadCollectTokens m) => GenericDecl (Expr Builtin) -> m (Maybe TokenClass)
classifyDeclHead = \case
  DefAbstract _ _ _ t -> classifyTokenOfType 0 t
  DefFunction _ _ s t _ -> do
    -- Record accesses (e.g. `pair.left`) should render the `.left` part as a Method, not a Function
    let fixProjectionTokenType tok@(_typ, mods) = case s of
          ProjectionDecl {} -> (Lsp.SemanticTokenTypes_Method, mods)
          _ -> tok
    classifyTokenOfType 0 t <&> fmap fixProjectionTokenType
  DefRecord {} -> return $ Just (SemanticTokenTypes_Class, [])

classifyBinderName :: (MonadCollectTokens m) => BinderDepth -> GenericBinder (Expr Builtin) -> m (Maybe TokenClass)
classifyBinderName depth b = classifyTokenOfType depth (typeOf b)

classifyBoundVar :: (MonadCollectTokens m) => BinderDepth -> Ix -> m (Maybe TokenClass)
classifyBoundVar depth ix = do
  ctx <- getBoundCtx (Proxy @(Expr Builtin))
  case lookupIx ctx ix of
    Nothing -> return Nothing
    Just binder -> inBinderContext ix $ classifyBinderName depth binder

classifyFreeVar :: (MonadCollectTokens m) => Identifier -> m (Maybe TokenClass)
classifyFreeVar ident = do
  decl <- getDeclEntry (Proxy @Builtin) ident
  classifyDeclHead decl
