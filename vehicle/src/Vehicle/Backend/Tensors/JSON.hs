{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Tensors.JSON
  ( compileProgToJSON,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Aeson (KeyValue (..), ToJSON (..), genericToJSON)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types (object)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NonEmpty (filter, toList)
import Data.Maybe (mapMaybe)
import Vehicle.Backend.Tensors.Core
import Vehicle.Compile.Arity
import Vehicle.Compile.Descope (DescopeNamed (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (GenericBoundCtx, GenericFreeCtx, HasType (..), getExplicitArg)
import Vehicle.Compile.Print
import Vehicle.Data.DeBruijn
import Vehicle.Data.RelevantExpr
import Vehicle.Prelude
import Vehicle.Prelude.Logging.Class
import Vehicle.Syntax.AST (Position (..), Provenance (..), UniverseLevel)
import Vehicle.Syntax.AST qualified as V

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  forall m a.
  (MonadCompile m) =>
  V.Prog Ix TensorBuiltin ->
  m (Doc a)
compileProgToJSON prog = do
  logCompilerPass MinDetail currentPass $ do
    jProg <- runReaderT (toJProg prog) mempty
    let namedProg = descopeNamed jProg
    let json = toJSON namedProg
    return $ pretty $ unpack $ encodePretty' prettyJSONConfig json

--------------------------------------------------------------------------------
-- Conversion of JExpr to JSON

type JProg var = RelProg var TensorBuiltin

type JDecl var = RelDecl var TensorBuiltin

type JExpr var = RelExpr var TensorBuiltin

type JBinder var = RelBinder var TensorBuiltin

instance (ToJSON var) => ToJSON (JProg var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JDecl var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JExpr var) where
  toJSON = genericToJSON jsonOptions

instance (ToJSON var) => ToJSON (JBinder var) where
  toJSON = genericToJSON jsonOptions

instance ToJSON TensorBuiltin where
  toJSON = genericToJSON jsonOptions

instance ToJSON Position where
  toJSON = genericToJSON jsonOptions

instance ToJSON UniverseLevel where
  toJSON = genericToJSON jsonOptions

instance ToJSON Provenance where
  toJSON (V.Provenance (V.Range start end) _) =
    object
      [ "tag" .= toJSON @String "Provenance",
        "contents" .= toJSON @[Int] [posLine start, posColumn start, posLine end, posColumn end]
      ]

--------------------------------------------------------------------------------
-- Conversion Expr to JExpr

currentPass :: Doc a
currentPass = "conversion to JSON"

type MonadJSON m =
  ( MonadCompile m,
    MonadReader (GenericFreeCtx Arity, GenericBoundCtx Arity) m
  )

toJProg :: (MonadJSON m) => V.Prog Ix TensorBuiltin -> m (JProg Ix)
toJProg (V.Main ds) = Main <$> toJDecls ds

toJDecls :: (MonadJSON m) => [V.Decl Ix TensorBuiltin] -> m [JDecl Ix]
toJDecls [] = return []
toJDecls (decl : decls) = do
  decl' <- logCompilerPass MinDetail (currentPass <+> "of" <+> quotePretty (V.identifierOf decl)) $ do
    case decl of
      V.DefAbstract p i s t -> case s of
        V.NetworkDef -> resourceError s
        V.DatasetDef -> resourceError s
        V.ParameterDef {} -> resourceError s
        V.PostulateDef {} -> DefPostulate p (V.nameOf i) <$> toJExpr t
      V.DefFunction p i _anns t e -> do
        t' <- toJExpr t
        e' <- toJExpr e
        return $ DefFunction p (V.nameOf i) t' e'

  decls' <- addDeclToContext decl (toJDecls decls)
  return $ decl' : decls'

toJExpr :: forall m. (MonadJSON m) => V.Expr Ix TensorBuiltin -> m (JExpr Ix)
toJExpr expr = case expr of
  V.Hole {} -> resolutionError currentPass "Hole"
  V.Meta {} -> resolutionError currentPass "Meta"
  V.Universe p l -> return $ Universe p (coerce l)
  V.Builtin p b -> return $ Builtin p b
  V.BoundVar p v -> return $ BoundVar p v
  V.FreeVar p v -> return $ FreeVar p $ V.nameOf v
  -- Otherwise, calculate whether function is partial or not.
  V.App p fun args -> do
    fun' <- toJExpr fun
    let explicitArgs = mapMaybe getExplicitArg (NonEmpty.toList args)
    args' <- traverse toJExpr explicitArgs
    arity <- functionArity fun
    case args' of
      [] -> return fun'
      _ : _
        | arity == length args' -> return $ App p fun' args'
        | arity > length args' -> return $ PartialApp p arity fun' args'
        | otherwise -> arityError fun arity explicitArgs args'
  V.Pi p binder body -> Pi p <$> toJBinder binder <*> addBinderToContext binder (toJExpr body)
  V.Lam p _ _ -> do
    (foldedBinders, body) <- foldLamBinders expr
    Lam p <$> toJBinders foldedBinders <*> toJExpr body
  V.Let p bound binder body ->
    Let p <$> toJExpr bound <*> toJBinder binder <*> addBinderToContext binder (toJExpr body)

foldLamBinders ::
  (MonadJSON m) =>
  V.Expr Ix TensorBuiltin ->
  m ([V.Binder Ix TensorBuiltin], V.Expr Ix TensorBuiltin)
foldLamBinders = \case
  V.Lam _ binder body -> addBinderToContext binder (first (binder :) <$> foldLamBinders body)
  expr -> return ([], expr)

toJBinder :: (MonadJSON m) => V.Binder Ix TensorBuiltin -> m (JBinder Ix)
toJBinder binder = do
  type' <- toJExpr $ typeOf binder
  let p = V.binderProvenance binder
  let maybeName = case V.namingForm (V.binderDisplayForm binder) of
        V.NameAndType n -> Just n
        V.OnlyName n -> Just n
        V.OnlyType -> Nothing
  return $ Binder p maybeName type'

toJBinders :: (MonadJSON m) => [V.Binder Ix TensorBuiltin] -> m [JBinder Ix]
toJBinders = \case
  [] -> return []
  (b : bs) -> do
    b' <- toJBinder b
    bs' <- addBinderToContext b $ toJBinders bs
    return $ b' : bs'

-- | TODO maybe move to Arity module.
functionArity ::
  forall m.
  (MonadJSON m) =>
  V.Expr Ix TensorBuiltin ->
  m Arity
functionArity = go
  where
    go :: (MonadJSON m) => V.Expr Ix TensorBuiltin -> m Arity
    go fun = do
      result <- case fun of
        V.App _ fn args -> do
          arity <- go fn
          return $ arity - length (NonEmpty.filter V.isExplicit args)
        V.Universe {} -> illTypedError currentPass (prettyVerbose fun)
        V.Pi {} -> illTypedError currentPass (prettyVerbose fun)
        V.Meta {} -> illTypedError currentPass (prettyVerbose fun)
        V.Hole {} -> illTypedError currentPass (prettyVerbose fun)
        V.FreeVar _p ident -> getFreeVarArity ident
        V.BoundVar _ ix -> getBoundVarArity ix
        V.Lam _ binder body -> addBinderToContext binder ((1 +) <$> go body)
        V.Builtin _ b -> do
          return $ arityOf b
        V.Let _ _bound binder body ->
          addBinderToContext binder $ go body
      return result

addBinderToContext :: (MonadJSON m) => V.Binder Ix TensorBuiltin -> m a -> m a
addBinderToContext binder =
  local (second (explicitArityFromType (typeOf binder) :))

addDeclToContext :: (MonadJSON m) => V.Decl Ix TensorBuiltin -> m a -> m a
addDeclToContext decl =
  local (second (explicitArityFromType (typeOf decl) :))

getFreeVarArity :: (MonadJSON m) => V.Identifier -> m Arity
getFreeVarArity ident = lookupInFreeCtx currentPass ident =<< asks fst

getBoundVarArity :: (MonadJSON m) => Ix -> m Arity
getBoundVarArity ix = lookupIxInBoundCtx currentPass ix =<< asks snd

resourceError :: (MonadCompile m) => V.DefAbstractSort -> m a
resourceError resourceType =
  compilerDeveloperError $
    "All"
      <+> quotePretty resourceType
      <+> "declarations should have been removed before"
      <+> squotes currentPass

arityError :: (MonadCompile m) => V.Expr Ix TensorBuiltin -> Arity -> [V.Expr Ix TensorBuiltin] -> [JExpr Ix] -> m a
arityError fun arity explicitArgs args =
  compilerDeveloperError $
    "Number of args is greater than arity:"
      <> line
      <> indent
        2
        ( "fun:"
            <+> prettyVerbose fun
            <> line
            <> "fun-arity:"
              <+> prettyVerbose arity
            <> line
            <> "args:"
              <+> prettyVerbose explicitArgs
            <> line
            <> "args-len:"
              <+> prettyVerbose (length args)
        )
