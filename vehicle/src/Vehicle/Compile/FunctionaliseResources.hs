module Vehicle.Compile.FunctionaliseResources
  ( functionaliseResources,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as LinkedHashMap (empty, filterWithKey, insert, member, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (insert, lookup)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set (fromList, member, singleton)
import Vehicle.Compile.Descope (DescopeNamed (..))
import Vehicle.Compile.Error (MonadCompile, internalScopingError, resolutionError, runCompileMonadSilently)
import Vehicle.Compile.Prelude.Contexts (DeclCtx)
import Vehicle.Compile.Print (PrintableBuiltin, prettyFriendly)
import Vehicle.Compile.Scope (scopeCheck)
import Vehicle.Expr.DeBruijn (Ix)
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
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog Ix builtin ->
  m (Prog Ix builtin)
functionaliseResources prog =
  logCompilerPass MidDetail currentPass $ do
    -- The scoping and descoping here is a massive hack.
    let namedProg = descopeNamed prog
    result <- runReaderT (functionaliseProg namedProg) (FuncState LinkedHashMap.empty mempty)
    return $ runCompileMonadSilently "scoping" $ scopeCheck mempty result

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
    MonadReader (FuncState builtin) m,
    PrintableBuiltin builtin
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
functionaliseDecl d = logCompilerPass MaxDetail ("functionalising" <+> pretty (nameOf d)) $ case d of
  DefAbstract p i s t -> do
    (t', typeResourceUsage) <- runWriterT $ functionaliseExpr t
    case s of
      NetworkDef -> return (addResourceDeclaration i t', Nothing)
      DatasetDef -> return (addResourceDeclaration i t', Nothing)
      ParameterDef _ -> return (addResourceDeclaration i t', Nothing)
      PostulateDef -> do
        (absType, binderNames) <- createBinders True p typeResourceUsage t'
        return (addResourceUsage i binderNames, Just (DefAbstract p i PostulateDef absType))
  DefFunction p i anns t e -> do
    (t', typeResourceUsage) <- runWriterT $ functionaliseExpr t
    (e', bodyResourceUsage) <- runWriterT $ functionaliseExpr e
    let resourceUsage = typeResourceUsage <> bodyResourceUsage
    (absType, binderNames) <- createBinders True p resourceUsage t'
    (absExpr, _) <- createBinders False p resourceUsage e'
    let fun = DefFunction p i anns absType absExpr
    logDebug MaxDetail $ "Prepending resources" <+> pretty binderNames
    logDebug MaxDetail $ prettyFriendly fun
    return (addResourceUsage i binderNames, Just fun)

functionaliseExpr :: (MonadJSONExpr m builtin) => Expr Name builtin -> m (Expr Name builtin)
functionaliseExpr = \case
  Hole {} -> resolutionError currentPass "Hole"
  Meta {} -> resolutionError currentPass "Meta"
  Ann p e t -> Ann p <$> functionaliseExpr e <*> functionaliseExpr t
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
          let extraArgs = fmap (RelevantExplicitArg p . BoundVar p) (arg :| args)
          return $ App p (FreeVar p v) extraArgs
  App p fun args -> do
    fun' <- functionaliseExpr fun
    args' <- traverse (traverse functionaliseExpr) args
    return $ App p fun' args'
  Pi p binder body -> Pi p <$> functionaliseBinder binder <*> functionaliseExpr body
  Lam p binder body -> Lam p <$> functionaliseBinder binder <*> functionaliseExpr body
  Let p bound binder body ->
    Let p <$> functionaliseExpr bound <*> functionaliseBinder binder <*> functionaliseExpr body

functionaliseBinder :: (MonadJSONExpr m builtin) => Binder Name builtin -> m (Binder Name builtin)
functionaliseBinder = traverse functionaliseExpr

createBinders ::
  (MonadJSON m builtin) =>
  Bool ->
  Provenance ->
  Set Name ->
  Expr Name builtin ->
  m (Expr Name builtin, [Name])
createBinders isType p idents expr = do
  FuncState {..} <- ask
  let identsAndTypes = LinkedHashMap.filterWithKey (\i _ -> Set.member i idents) resourceDeclarations
  let identsAndTypesList = LinkedHashMap.toList identsAndTypes
  let mkBindingForm ident
        | isType = BinderDisplayForm (NameAndType (nameOf ident)) True
        | otherwise = BinderDisplayForm (OnlyName (nameOf ident)) True
  let mkBinder (ident, typ) = (nameOf ident, Binder p (mkBindingForm ident) Explicit Relevant typ)
  let (binderNames, binders) = unzip $ fmap mkBinder identsAndTypesList
  let constructor
        | isType = Pi p
        | otherwise = Lam p
  let absExpr = foldr constructor expr binders
  return (absExpr, binderNames)
