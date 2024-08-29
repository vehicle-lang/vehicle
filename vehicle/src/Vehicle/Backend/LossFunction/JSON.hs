{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.LossFunction.JSON
  ( compileProgToJSON,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Aeson (KeyValue (..), ToJSON (..), genericToJSON)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Aeson.Types (object)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty qualified as NonEmpty (filter, toList)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Vehicle.Compile.Arity
import Vehicle.Compile.Descope (MonadDescope, ixToProperName, runMonadDescopeT)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude (BinderNamingForm (..), DefAbstractSort (..), Doc, GenericBoundCtx, GenericDecl (..), GenericFreeCtx, HasName, HasType (..), Identifier, Name, Position, Provenance (..), Range (..), getExplicitArg, prettyJSONConfig)
import Vehicle.Compile.Prelude qualified as S
import Vehicle.Compile.Print
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.DeBruijn
import Vehicle.Data.Universe (UniverseLevel (..))
import Vehicle.Prelude (HasName (..), Position (..), Pretty (..), indent, jsonOptions, line, quotePretty, squotes, (<+>))
import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Public method

compileProgToJSON ::
  forall m a.
  (MonadCompile m) =>
  S.Prog TensorBuiltin ->
  m (Doc a)
compileProgToJSON prog = logCompilerPass MinDetail currentPass $ do
  jProg <- runReaderT (runMonadDescopeT (toJProg prog)) mempty
  let json = toJSON jProg
  return $ pretty $ unpack $ encodePretty' prettyJSONConfig json

--------------------------------------------------------------------------------
-- Conversion of JExpr to JSON

--------------------------------------------------------------------------------
-- Relevant expressions

-- This file contains an AST that ideally only contains computationally relevant
-- information suitable for exporting to the Python interpreter. This ambition
-- is not yet quite achieved, as it still contains universes and full pi-types,
-- but ideally this should be fixed once we get irrelevance up and running.

newtype JProg
  = Main [JDecl]
  deriving (Generic)

data JDecl
  = Postulate Provenance Name JExpr
  | Function Provenance Name JExpr JExpr
  deriving (Generic)

data JExpr
  = Universe Provenance Int
  | App JExpr [JExpr]
  | -- | Because we're probably not going to a functional language we
    -- need to mark partial applications as such to avoid excessive currying.
    PartialApp Arity JExpr [JExpr]
  | Pi Provenance JBinder JExpr
  | Builtin Provenance TensorBuiltin
  | BoundVar Provenance Name
  | FreeVar Provenance Name
  | Let Provenance JExpr JBinder JExpr
  | Lam Provenance [JBinder] JExpr
  deriving (Generic)

data JBinder = Binder Provenance (Maybe Name) JExpr
  deriving (Generic)

instance HasName JBinder (Maybe Name) where
  nameOf (Binder _ name _) = name

--------------------------------------------------------------------------------
-- Utils

instance ToJSON JProg where
  toJSON = genericToJSON jsonOptions

instance ToJSON JDecl where
  toJSON = genericToJSON jsonOptions

instance ToJSON JExpr where
  toJSON = genericToJSON jsonOptions

instance ToJSON JBinder where
  toJSON = genericToJSON jsonOptions

instance ToJSON TensorBuiltin where
  toJSON = genericToJSON jsonOptions

instance ToJSON Position where
  toJSON = genericToJSON jsonOptions

instance ToJSON UniverseLevel where
  toJSON = genericToJSON jsonOptions

instance ToJSON Provenance where
  toJSON (Provenance (Range start end) _) =
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
    MonadReader (GenericFreeCtx Arity, GenericBoundCtx Arity) m,
    MonadDescope m
  )

toJProg :: (MonadJSON m) => S.Prog TensorBuiltin -> m JProg
toJProg (S.Main ds) = Main <$> toJDecls ds

toJDecls :: (MonadJSON m) => [S.Decl TensorBuiltin] -> m [JDecl]
toJDecls [] = return []
toJDecls (decl : decls) = do
  decl' <- logCompilerPass MinDetail (currentPass <+> "of" <+> quotePretty (S.identifierOf decl)) $ case decl of
    DefAbstract p i s t -> case s of
      NetworkDef -> resourceError s
      DatasetDef -> resourceError s
      ParameterDef {} -> resourceError s
      PostulateDef {} -> Postulate p (S.nameOf i) <$> toJExpr t
    DefFunction p i _anns t e -> do
      t' <- toJExpr t
      e' <- toJExpr e
      return $ Function p (nameOf i) t' e'

  decls' <- addDeclToContext decl (toJDecls decls)
  return $ decl' : decls'

toJExpr :: forall m. (MonadJSON m) => S.Expr TensorBuiltin -> m JExpr
toJExpr expr = do
  showEntry expr
  result <- case expr of
    S.Hole {} -> resolutionError currentPass "Hole"
    S.Meta {} -> resolutionError currentPass "Meta"
    S.Universe p (UniverseLevel l) -> return $ Universe p l
    S.Builtin p b -> return $ Builtin p b
    S.FreeVar p v -> return $ FreeVar p $ S.nameOf v
    S.BoundVar p v -> do
      n <- ixToProperName p v
      return $ BoundVar p n
    S.App fun args -> do
      fun' <- toJExpr fun
      let explicitArgs = mapMaybe getExplicitArg (NonEmpty.toList args)
      args' <- traverse toJExpr explicitArgs
      arity <- functionArity fun
      case args' of
        [] -> return fun'
        _ : _
          | arity == length args' -> return $ App fun' args'
          | arity > length args' -> return $ PartialApp arity fun' args'
          | otherwise -> arityError fun arity explicitArgs args'
    S.Pi p binder body ->
      Pi p <$> toJBinder binder <*> addBinderToContext binder (toJExpr body)
    S.Lam p binder _ -> do
      (foldedBinders, body) <- foldLamBinders expr
      (jBinders, jBody) <- toJBinders (binder : foldedBinders) (toJExpr body)
      return $ Lam p jBinders jBody
    S.Let p bound binder body ->
      Let p <$> toJExpr bound <*> toJBinder binder <*> addBinderToContext binder (toJExpr body)
  showExit result
  return result

foldLamBinders ::
  (MonadJSON m) =>
  S.Expr TensorBuiltin ->
  m ([S.Binder TensorBuiltin], S.Expr TensorBuiltin)
foldLamBinders = \case
  S.Lam _ binder body -> addBinderToContext binder (first (binder :) <$> foldLamBinders body)
  expr -> return ([], expr)

toJBinder :: (MonadJSON m) => S.Binder TensorBuiltin -> m JBinder
toJBinder binder = do
  type' <- toJExpr $ typeOf binder
  let p = S.binderProvenance binder
  let maybeName = case S.namingForm (S.binderDisplayForm binder) of
        NameAndType n -> Just n
        OnlyName n -> Just n
        OnlyType -> Nothing
  return $ Binder p maybeName type'

toJBinders :: (MonadJSON m) => [S.Binder TensorBuiltin] -> m JExpr -> m ([JBinder], JExpr)
toJBinders [] body = ([],) <$> body
toJBinders (b : bs) body = do
  b' <- toJBinder b
  (bs', body') <- addBinderToContext b $ toJBinders bs body
  return (b' : bs', body')

-- | TODO maybe move to Arity module.
functionArity ::
  forall m.
  (MonadJSON m) =>
  S.Expr TensorBuiltin ->
  m Arity
functionArity = go
  where
    go :: (MonadJSON m) => S.Expr TensorBuiltin -> m Arity
    go fun = do
      case fun of
        S.App fn args -> do
          arity <- go fn
          return $ arity - length (NonEmpty.filter S.isExplicit args)
        S.Universe {} -> illTypedError currentPass (prettyVerbose fun)
        S.Pi {} -> illTypedError currentPass (prettyVerbose fun)
        S.Meta {} -> illTypedError currentPass (prettyVerbose fun)
        S.Hole {} -> illTypedError currentPass (prettyVerbose fun)
        S.FreeVar _p ident -> getFreeVarArity ident
        S.BoundVar _ ix -> getBoundVarArity ix
        S.Lam _ binder body -> addBinderToContext binder ((1 +) <$> go body)
        S.Builtin _ b -> do
          return $ arityOf b
        S.Let _ _bound binder body ->
          addBinderToContext binder $ go body

addBinderToContext :: (MonadJSON m) => S.Binder TensorBuiltin -> m a -> m a
addBinderToContext binder =
  local (second (explicitArityFromType (typeOf binder) :))

addDeclToContext :: (MonadJSON m) => S.Decl TensorBuiltin -> m a -> m a
addDeclToContext decl =
  local (second (explicitArityFromType (typeOf decl) :))

getFreeVarArity :: (MonadJSON m) => Identifier -> m Arity
getFreeVarArity ident = lookupInFreeCtx currentPass ident =<< asks fst

getBoundVarArity :: (MonadJSON m) => Ix -> m Arity
getBoundVarArity ix = asks (lookupIxInBoundCtx currentPass ix . snd)

resourceError :: (MonadCompile m) => DefAbstractSort -> m a
resourceError resourceType =
  compilerDeveloperError $
    "All"
      <+> quotePretty resourceType
      <+> "declarations should have been removed before"
      <+> squotes currentPass

arityError :: (MonadCompile m) => S.Expr TensorBuiltin -> Arity -> [S.Expr TensorBuiltin] -> [JExpr] -> m a
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

showEntry :: (MonadJSON m) => S.Expr TensorBuiltin -> m ()
showEntry e = do
  (_, ctx) <- ask
  logDebug MaxDetail $ "tensor-enter:" <+> pretty (length ctx) <+> prettyVerbose e
  incrCallDepth

showExit :: (MonadJSON m) => JExpr -> m ()
showExit _e = decrCallDepth
