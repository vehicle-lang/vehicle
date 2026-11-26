module Vehicle.LSP.SemanticTokens.Semantic
  ( collectDeclsTokens,
    mkTokPure,
  )
where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ask)
import Data.Functor ((<&>))
import Data.List
import Data.List.NonEmpty qualified as NonEmpty (toList)
import Data.Proxy (Proxy (..))
import Language.LSP.Protocol.Types as Lsp
  ( SemanticTokenTypes (..),
  )
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.Builtin.Standard
  ( Builtin (..),
  )
import Vehicle.Data.Variable.Bound.Context.Core (lookupIx)
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (..))
import Vehicle.Data.Variable.Bound.Index (Ix (..))
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, addDeclToContext, getDeclEntry)
import Vehicle.LSP.SemanticTokens.Common (IntermediateSemTok, TokenClass, mkTokPure)
import Vehicle.Prelude
  ( DefFunctionSort (..),
    GenericArg (..),
    GenericBinder (..),
    GenericDecl (..),
    HasProvenance (..),
    Identifier,
    Provenance,
    binderNamingForm,
    typeOf,
    wasInsertedByCompiler,
  )

--------------------------------------------------------------------------------
-- Internal types and utilities

type MonadEmitTokens m = MonadReader FilePath m

type MonadCollectTokens m =
  ( MonadFreeContext Builtin m,
    MonadBoundContext (Expr Builtin) m,
    MonadEmitTokens m
  )

type BinderDepth = Int

-- drop `ix + 1` binders because we want to drop all binders up to and including binder `ix`
-- e.g. if we want to run in the context of before we added i2, we need to drop i0, i1, and i2 (3 total).
inBinderContext :: (MonadBoundContext (Expr Builtin) m) => Ix -> m a -> m a
inBinderContext ix = dropBindersFromContext (Proxy @(Expr Builtin)) (unIx ix + 1)

emitAt :: (MonadEmitTokens m) => Provenance -> Maybe TokenClass -> m IntermediateSemTok
emitAt p = \case
  Nothing -> return Nothing
  Just tok -> mkTok p tok

mkTok :: (MonadEmitTokens m) => Provenance -> TokenClass -> m IntermediateSemTok
mkTok p tok = do
  filePath <- ask
  return $ mkTokPure filePath p tok

--------------------------------------------------------------------------------
-- Token collection:
--
-- We have an arbitrary expression; we need to identify and colour every token
-- inside of it.

collectDeclsTokens :: (MonadCollectTokens m) => [GenericDecl (Expr Builtin)] -> m [IntermediateSemTok]
collectDeclsTokens [] = return []
collectDeclsTokens (d : ds) = do
  declToks <- collectDeclTokens d
  restToks <- addDeclToContext d $ collectDeclsTokens ds
  return $ declToks ++ restToks

collectDeclTokens :: (MonadCollectTokens m) => GenericDecl (Expr Builtin) -> m [IntermediateSemTok]
collectDeclTokens decl = do
  headTok <- emitAt (provenanceOf decl) =<< classifyDeclHead decl
  bodyToks <- getBodyToks
  return $ headTok : bodyToks
  where
    getBodyToks = case decl of
      DefAbstract _ _ _ t -> collectExprTokens t
      DefFunction _ _ _ t e -> concat <$> traverse collectExprTokens ([t, e] :: [Expr Builtin])
      DefRecord _ _ _ t e _ -> go t
        where
          go [] = collectFieldsTokens e
          go (binder : binders) = do
            binderToks <- collectBinderTokens binder
            restToks <- addBinderToContext binder $ go binders
            return $ binderToks ++ restToks

collectBinderTokens :: (MonadCollectTokens m) => GenericBinder (Expr Builtin) -> m [IntermediateSemTok]
collectBinderTokens binder
  | wasInsertedByCompiler binder = return []
  | otherwise = do
      exprToks <- collectExprTokens (typeOf binder)
      nameTok <- emitAt (provenanceOf $ binderNamingForm binder) =<< classifyBinderName 0 binder
      return $ nameTok : exprToks

collectExprTokens ::
  (MonadCollectTokens m) =>
  Expr Builtin ->
  m [IntermediateSemTok]
collectExprTokens = \case
  Universe _p _level -> return [] -- Handled by syntax pass
  Pi _p binder res -> do
    binderToks <- collectBinderTokens binder
    exprToks <- addBinderToContext binder $ collectExprTokens res
    return $ binderToks ++ exprToks
  Builtin _p _builtin -> return [] -- Handled by syntax pass
  BoundVar p ix -> singleton <$> (emitAt p =<< classifyBoundVar 0 ix)
  FreeVar p ident -> singleton <$> (emitAt p =<< classifyFreeVar ident)
  Hole _p _name -> return [] -- Handled by syntax pass
  Meta _p _metaId -> return [] -- Handled by syntax pass
  App f args -> do
    fnToks <- collectExprTokens f
    argsToks <- concat <$> traverse collectArgTokens (filter (not . wasInsertedByCompiler) (NonEmpty.toList args))
    return $ fnToks ++ argsToks
  Let _p bound bodyBinder bodyExpr -> do
    boundToks <- collectExprTokens bound
    binderToks <- collectBinderTokens bodyBinder
    bodyToks <- addBinderToContext bodyBinder $ collectExprTokens bodyExpr
    return $ boundToks ++ binderToks ++ bodyToks
  Lam _p binder body -> do
    binderToks <- collectBinderTokens binder
    bodyToks <- addBinderToContext binder $ collectExprTokens body
    return $ binderToks ++ bodyToks
  Record _p _typ fields -> do
    fieldsToks <- collectFieldsTokens fields
    return fieldsToks
  RecordProj p _typ _body _fieldName -> singleton <$> mkTok p (SemanticTokenTypes_Method, [])

collectArgTokens ::
  (MonadCollectTokens m) =>
  Arg Builtin ->
  m [IntermediateSemTok]
collectArgTokens = collectExprTokens . argExpr

collectFieldsTokens ::
  (MonadCollectTokens m) =>
  RecordFields Builtin ->
  m [IntermediateSemTok]
collectFieldsTokens fields =
  concat
    <$> forM
      fields
      ( \(field, fieldValue) -> do
          valueToks <- collectExprTokens fieldValue
          fieldTok <- mkTok (provenanceOf field) (SemanticTokenTypes_Method, [])
          return $ fieldTok : valueToks
      )

--------------------------------------------------------------------------------
-- Token classification:
--
-- We have a single token `foo` of type `bar`, where `bar` is an arbitrary
-- expression; we need to work out what colour to make `foo`.

-- | A note on 'BinderDepth':
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
classifyTokenOfType :: (MonadCollectTokens m) => BinderDepth -> Expr Builtin -> m (Maybe TokenClass)
classifyTokenOfType depth = \case
  Universe {}
    | depth > 0 -> return $ Just (SemanticTokenTypes_Variable, [])
    | otherwise -> return $ Just (SemanticTokenTypes_Type, [])
  Pi _ binder res
    | wasInsertedByCompiler binder -> addBinderToContext binder $ classifyTokenOfType depth res
    | otherwise -> return $ Just (SemanticTokenTypes_Function, [])
  Builtin _ _builtin -> return $ Just (SemanticTokenTypes_Variable, [])
  BoundVar _ ix -> classifyBoundVar (depth + 1) ix
  FreeVar _ ident -> do
    decl <- getDeclEntry (Proxy @Builtin) ident
    case decl of
      DefAbstract {} -> return $ Just (SemanticTokenTypes_Variable, [])
      DefFunction _ _ _ _ e -> classifyTokenOfType depth e
      DefRecord {} -> return $ Just (SemanticTokenTypes_Variable, [])
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
  DefRecord {} -> return $ Just (SemanticTokenTypes_Type, [])

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
