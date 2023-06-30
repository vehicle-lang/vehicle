module Vehicle.Compile.FunctionaliseResources
  ( functionaliseResources,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Aeson (ToJSON (..))
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as LinkedHashMap (empty, filterWithKey, insert, member, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (insert, lookup)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member, singleton)
import Vehicle.Compile.Error (MonadCompile, internalScopingError, resolutionError)
import Vehicle.Compile.Prelude.Contexts (DeclCtx)
import Vehicle.Prelude (traverseListLocal)
import Vehicle.Prelude.Logging
import Vehicle.Prelude.Prettyprinter
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Public interface

-- | This method traverses the program removing the resource declarations and
-- instead changing functions that use them so that they take the resources
-- explicitly as arguments instead. e.g.
--
--    @network
--    f : Rat -> Rat
--
--    @property
--    prop : Bool
--    prop = forall x . f x > 0
--
-- becomes:
--
--    @property
--    prop : (f : Rat -> Rat) -> Bool
--    prop f = forall x . f x > 0
--
-- Note that the semantics of properties therefore change slightly as they
-- are no longer guaranteed to be of type `Bool`.
functionaliseResources ::
  (MonadCompile m, ToJSON builtin) =>
  Prog Name builtin ->
  m (Prog Name builtin)
functionaliseResources prog =
  logCompilerPass MidDetail currentPass $ do
    runReaderT (functionaliseProg prog) (FuncState LinkedHashMap.empty mempty)

--------------------------------------------------------------------------------
-- Conversion Expr to JExpr

currentPass :: Doc a
currentPass = "resource functionalisation"

data FuncState builtin = FuncState
  { resourceDeclarations :: LinkedHashMap Name (Type Name builtin),
    resourceUsageDeclCtx :: DeclCtx [Name]
  }

addResourceDeclaration :: Identifier -> Type Name builtin -> FuncState builtin -> FuncState builtin
addResourceDeclaration resource typ FuncState {..} =
  FuncState
    { resourceDeclarations = LinkedHashMap.insert (nameOf resource) typ resourceDeclarations,
      ..
    }

addResourceUsage :: Identifier -> [Name] -> FuncState builtin -> FuncState builtin
addResourceUsage ident newArgNames FuncState {..} =
  FuncState
    { resourceUsageDeclCtx = Map.insert ident newArgNames resourceUsageDeclCtx,
      ..
    }

type MonadJSON m builtin =
  ( MonadCompile m,
    MonadReader (FuncState builtin) m
  )

type MonadJSONExpr m builtin =
  ( MonadJSON m builtin,
    MonadWriter (Set Name) m
  )

functionaliseProg :: (MonadJSON m builtin) => Prog Name builtin -> m (Prog Name builtin)
functionaliseProg (Main ds) = Main . catMaybes <$> traverseListLocal functionaliseDecl ds

functionaliseDecl ::
  (MonadJSON m builtin) =>
  Decl Name builtin ->
  m (FuncState builtin -> FuncState builtin, Maybe (Decl Name builtin))
functionaliseDecl d = case d of
  DefAbstract p i s t -> do
    (t', typeResourceUsage) <- runWriterT $ functionaliseExpr t
    case s of
      NetworkDef -> return (addResourceDeclaration i t', Nothing)
      DatasetDef -> return (addResourceDeclaration i t', Nothing)
      ParameterDef _ -> return (addResourceDeclaration i t', Nothing)
      PostulateDef -> do
        (binderNames, binders) <- unzip <$> createBinders p typeResourceUsage
        let boundType = abstractOverBinders t' binders
        return (addResourceUsage i binderNames, Just (DefAbstract p i PostulateDef boundType))
  DefFunction p i anns t e -> do
    (t', typeResourceUsage) <- runWriterT $ functionaliseExpr t
    (e', bodyResourceUsage) <- runWriterT $ functionaliseExpr e
    let resourceUsage = typeResourceUsage <> bodyResourceUsage
    (binderNames, binders) <- unzip <$> createBinders p resourceUsage
    let boundType = abstractOverBinders t' binders
    let boundExpr = abstractOverBinders e' binders
    let fun = DefFunction p i anns boundType boundExpr
    logDebug MaxDetail $ "Prepending decl" <+> quotePretty i <+> "binders" <+> pretty binderNames
    return (addResourceUsage i binderNames, Just fun)

functionaliseExpr :: (MonadJSONExpr m builtin) => Expr Name builtin -> m (Expr Name builtin)
functionaliseExpr = \case
  Hole {} -> resolutionError currentPass "Hole"
  Meta {} -> resolutionError currentPass "Meta"
  Ann {} -> resolutionError currentPass "Ann"
  Universe p l -> return $ Universe p l
  Builtin p b -> return $ Builtin p b
  BoundVar p v -> return $ BoundVar p v
  FreeVar p v -> do
    FuncState {..} <- ask
    let name = nameOf v
    if name `LinkedHashMap.member` resourceDeclarations
      then do
        tell (Set.singleton name)
        return $ BoundVar p name
      else case v `Map.lookup` resourceUsageDeclCtx of
        Nothing -> internalScopingError currentPass v
        Just [] -> return $ FreeVar p v
        Just (arg : args) -> do
          tell (Set.fromList (arg : args))
          let extraArgs = fmap (ExplicitArg p . BoundVar p) (arg :| args)
          return $ App p (FreeVar p v) extraArgs
  App p fun args -> do
    fun' <- functionaliseExpr fun
    args' <- traverse (traverse functionaliseExpr) args
    return $ App p fun' args'
  Pi p binder body
    | isExplicit binder -> Pi p <$> functionaliseBinder binder <*> functionaliseExpr body
    | otherwise -> functionaliseExpr body
  Lam p binder body
    | isExplicit binder -> Lam p <$> functionaliseBinder binder <*> functionaliseExpr body
    | otherwise -> functionaliseExpr body
  Let p bound binder body ->
    Let p <$> functionaliseExpr bound <*> functionaliseBinder binder <*> functionaliseExpr body

functionaliseBinder :: (MonadJSONExpr m builtin) => Binder Name builtin -> m (Binder Name builtin)
functionaliseBinder = traverse functionaliseExpr

createBinders :: (MonadJSON m builtin) => Provenance -> Set Name -> m [(Name, Binder Name builtin)]
createBinders p idents = do
  FuncState {..} <- ask
  let identsAndTypes = LinkedHashMap.filterWithKey (\i _ -> Set.member i idents) resourceDeclarations
  let identsAndTypesList = LinkedHashMap.toList identsAndTypes
  let mkBindingForm ident = BinderDisplayForm (NameAndType (nameOf ident)) False
  let mkBinder (ident, typ) = (nameOf ident, Binder p (mkBindingForm ident) Explicit Relevant typ)
  return $ fmap mkBinder identsAndTypesList

abstractOverBinders :: Expr Name builtin -> [Binder Name builtin] -> Expr Name builtin
abstractOverBinders = foldr (\binder expr -> Lam (provenanceOf binder) binder expr)
