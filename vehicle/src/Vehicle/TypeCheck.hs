module Vehicle.TypeCheck
  ( TypeCheckOptions (..),
    typeCheck,
    typeCheckUserProg,
    runCompileMonad,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), gets, modify)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency (AdjacencyGraph, emptyAdjacencyGraph, insertEdge, insertNode, topologicalSort)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (DeclarationFilter, monomorphise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error
import Vehicle.Compile.Scope (scopeModuleDecls)
import Vehicle.Compile.Serialise (readObjectFile, writeObjectFile)
import Vehicle.Compile.Type
import Vehicle.Compile.Type.Core (emptyInstanceDatabase)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Loss (LossMode (..))
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Instances (standardBuiltinInstances)
import Vehicle.Data.Builtin.Standard.Type ()
import Vehicle.Data.Code.ModuleInterface (ImportedModuleContext, ModuleInterface (..), mergeImportedFreeCtxs, typedModule)
import Vehicle.Libraries (ensureLatestVersionOfLibraryInstalled, resolveLibrary)
import Vehicle.Libraries.Core (ResolvedLibrary (..))
import Vehicle.Libraries.StandardLibrary (standardLibIdent, standardLibrary, standardLibraryContent, standardLibraryDefinitionsModulePath, standardLibraryName)
import Vehicle.Prelude.Logging.Instance
import Vehicle.Verify.Specification.IO (readSpecification)

data TypeCheckOptions = TypeCheckOptions
  { specification :: FilePath,
    secondaryTypeSystem :: Maybe SecondaryTypeSystem,
    declarationsToCompile :: DeclarationNames
  }
  deriving (Eq, Show)

typeCheck :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> TypeCheckOptions -> IO ()
typeCheck loggingSettings outputAsJSON options@TypeCheckOptions {..} =
  runCompileMonad loggingSettings outputAsJSON $ do
    prog <- typeCheckUserProg options
    case secondaryTypeSystem of
      Nothing -> return ()
      Just typeSystem -> case typeSystem of
        LinearityTypes -> printPropertyTypes =<< linearityTypeCheck prog mempty
        PolarityTypes -> printPropertyTypes =<< polarityTypeCheck prog mempty
        DecidabilityTypes -> printPropertyTypes . Right =<< decidabilityTypeCheck prog
        GradientCarryingTypes -> printPropertyTypes . Right =<< gradientTypeCheck @_ @'Train Train (standardLibIdent (nameOf $ BuiltinLogic VehicleLoss)) prog

--------------------------------------------------------------------------------
-- Useful functions that apply to multiple compiler passes

typeCheckUserProg ::
  (MonadStdIO m, MonadCompile m) =>
  TypeCheckOptions ->
  m (Prog Builtin)
typeCheckUserProg TypeCheckOptions {..} = do
  ensureLatestVersionOfLibraryInstalled standardLibrary standardLibraryContent

  -- Load builtins and definitions
  (userProg, importedModules, moduleGraph) <- loadUserSpecification specification

  -- Post-process the program to simplify it
  keepUnusedDeclarationFn <- checkDeclarationNamesPresent userProg declarationsToCompile
  monomorphisedProg <- monomorphise userProg keepUnusedDeclarationFn

  prog <- flattenProgram monomorphisedProg importedModules moduleGraph
  castFreeProg <- resolveInstanceArgumentsAndCasts prog
  return castFreeProg

checkDeclarationNamesPresent ::
  (MonadCompile m) =>
  Prog Builtin ->
  DeclarationNames ->
  m (DeclarationFilter Builtin)
checkDeclarationNamesPresent (Main decls) requestedDeclNames = do
  let actualDeclNames = Set.fromList $ fmap nameOf decls
  let missingNames = Set.toList $ Set.fromList requestedDeclNames `Set.difference` actualDeclNames
  case missingNames of
    [] -> return ()
    n : ns ->
      throwError $
        MissingRequestedDeclarations (n :| ns)

  let isRootDecl :: Decl Builtin -> Bool
      isRootDecl
        | null requestedDeclNames = \d -> isUserCode d
        | otherwise = do
            let declsToCompile = Set.fromList requestedDeclNames
            \d -> Set.member (nameOf $ identifierOf d) declsToCompile

  return $ \d ->
    -- Keep the declarations the users requested
    isRootDecl d
      ||
      -- Keep tensor coercions as they may be inserted by Loss or Solver backends.
      isTensorCoercionDecl d

printPropertyTypes ::
  (MonadStdIO m, MonadCompile m, PrintableBuiltin builtin) =>
  Either CompileError (Prog builtin) ->
  m ()
printPropertyTypes = \case
  Left err -> throwError err
  Right (Main decls) -> do
    let propertyDocs = mapMaybe toPropertySummary decls
    let outputDoc = concatWith (\a b -> a <> line <> b) propertyDocs
    programOutput outputDoc
    where
      toPropertySummary :: (PrintableBuiltin builtin) => Decl builtin -> Maybe (Doc a)
      toPropertySummary = \case
        DefFunction _ ident sort typ _ | isAnnotatedAsProperty sort -> do
          let propertyName = pretty $ identifierName ident
          let propertyType = prettyFriendlyEmptyCtx typ
          Just $ propertyName <+> ":" <+> propertyType
        _ -> Nothing

runCompileMonad ::
  forall m a.
  (MonadStdIO m) =>
  LoggingSettings ->
  OutputAsJSON ->
  (forall n. (MonadStdIO n, MonadLogger n) => ExceptT CompileError n a) ->
  m a
runCompileMonad loggingSettings outputAsJSON x = do
  errorOrResult <- runLoggerT loggingSettings (logCompileError x)
  case errorOrResult of
    Left err -> fatalError $ prettyCompileError outputAsJSON err
    Right val -> return val

--------------------------------------------------------------------------------
-- Monad

data ModuleStatus
  = Unchanged
  | Changed

instance Semigroup ModuleStatus where
  Changed <> _ = Changed
  _ <> Changed = Changed
  Unchanged <> Unchanged = Unchanged

data ModuleInfo = ModuleInfo
  { moduleInterface :: ModuleInterface Builtin,
    moduleFreeCtx :: FreeCtx Builtin,
    moduleStatus :: ModuleStatus
  }

-- | The full state of the Vehicle program
data ProgramContext = ProgramContext
  { moduleGraph :: AdjacencyGraph ModulePath,
    loadedModules :: Map ModulePath ModuleInfo,
    availableModules :: Map ModulePath FilePath
  }

lookupModuleCertain ::
  Prog Builtin ->
  Map ModulePath [Decl Builtin] ->
  ModulePath ->
  [Decl Builtin]
lookupModuleCertain userProg importedModules modulePath
  | modulePath == userModulePath = programDeclarations userProg
  | otherwise = do
      case Map.lookup modulePath importedModules of
        Nothing -> developerError $ "Missing module" <+> quotePretty modulePath
        Just result -> result

flattenProgram ::
  (MonadCompile m) =>
  Prog Builtin ->
  Map ModulePath [Decl Builtin] ->
  AdjacencyGraph ModulePath ->
  m (Prog Builtin)
flattenProgram userProg importedModules moduleGraph = do
  let sortedModulePaths = topologicalSort userModulePath moduleGraph
  let moduleDecls = fmap (lookupModuleCertain userProg importedModules) sortedModulePaths
  return $ Main $ concat moduleDecls

data ModuleStack = ModuleStack
  { stackCurrentModule :: ModulePath,
    stackRemainingModules :: [ModulePath]
  }

type MonadTCMProg m =
  ( MonadState ProgramContext m,
    MonadReader ModuleStack m,
    MonadStdIO m,
    MonadCompile m
  )

getCurrentModulePath :: (MonadTCMProg m) => m ModulePath
getCurrentModulePath = asks stackCurrentModule

lookupModule :: (MonadTCMProg m) => ModulePath -> m (Maybe ModuleInfo)
lookupModule modulePath = gets (Map.lookup modulePath . loadedModules)

lookupModuleFilePath :: (MonadTCMProg m) => ModulePath -> m FilePath
lookupModuleFilePath modulePath = do
  maybeFilePath <- gets (Map.lookup modulePath . availableModules)
  case maybeFilePath of
    Nothing -> missingImportError modulePath
    Just moduleFile -> return moduleFile

enterModule :: (MonadTCMProg m) => ModulePath -> m a -> m a
enterModule newModule action = do
  ModuleStack {..} <- ask

  -- Add an edge to the dependency graph
  modify $ \ProgramContext {..} ->
    ProgramContext
      { moduleGraph = insertEdge (stackCurrentModule, newModule) $ insertNode newModule moduleGraph,
        ..
      }

  -- Check for import loops
  let previousStack = stackCurrentModule : stackRemainingModules
  when (newModule `elem` previousStack) $ do
    cyclicImportsError newModule previousStack

  -- Run the action under the updated
  let newStack = ModuleStack newModule previousStack
  local (const newStack) action

storeModule :: (MonadTCMProg m) => ModulePath -> ModuleInfo -> m ()
storeModule modulePath moduleInfo =
  modify $ \ProgramContext {..} ->
    ProgramContext
      { loadedModules = Map.insert modulePath moduleInfo loadedModules,
        ..
      }

--------------------------------------------------------------------------------
-- Algorithm

loadUserSpecification ::
  (MonadCompile m, MonadStdIO m) =>
  FilePath ->
  m (Prog Builtin, Map ModulePath [Decl Builtin], AdjacencyGraph ModulePath)
loadUserSpecification specificationFile = do
  availableModules <- loadLibraries specificationFile

  let initialContext =
        ProgramContext
          { moduleGraph = emptyAdjacencyGraph,
            loadedModules = mempty,
            availableModules = availableModules
          }
  let initialStack =
        ModuleStack
          { stackCurrentModule = userModulePath,
            stackRemainingModules = []
          }
  let implicitImports = ImportStatement <$> [standardLibraryDefinitionsModulePath]
  let action = loadUnloadedModule implicitImports userModulePath
  (_status, programCtx) <- runStateT (runReaderT action initialStack) initialContext

  let declsMap = fmap (moduleDeclarations . typedModule . moduleInterface) (loadedModules programCtx)
  let (maybeUserModule, builtinModules) = Map.updateLookupWithKey (\_ _ -> Nothing) userModulePath declsMap
  let userModule = maybe (developerError "missing user module") Main maybeUserModule
  return (userModule, builtinModules, moduleGraph programCtx)

loadLibraries ::
  (MonadCompile m, MonadIO m) =>
  FilePath ->
  m (Map ModulePath FilePath)
loadLibraries specificationFile = do
  let resolvedUserLibrary =
        ResolvedLibrary
          { resolvedModules = [(userModulePath, specificationFile)]
          }
  resolvedStandardLibrary <- resolveLibrary standardLibraryName
  let libraries = [resolvedStandardLibrary, resolvedUserLibrary] :: [ResolvedLibrary]
  let availableModules = concatMap resolvedModules libraries
  return $ Map.fromListWithKey duplicateModuleError availableModules

-- | Loads a module into the program state and returning `True` if
-- the module .
loadModule ::
  (MonadTCMProg m) =>
  ModulePath ->
  m ModuleInfo
loadModule modulePath =
  enterModule modulePath $ do
    alreadyLoadedModuleInfo <- lookupModule modulePath
    case alreadyLoadedModuleInfo of
      Just info -> return info
      Nothing -> loadUnloadedModule mempty modulePath

loadUnloadedModule ::
  (MonadTCMProg m) =>
  [ImportStatement] ->
  ModulePath ->
  m ModuleInfo
loadUnloadedModule implicitImports modulePath = do
  logCompilerSection2 MidDetail ("loading module" <+> quotePretty modulePath) $ do
    moduleFile <- lookupModuleFilePath modulePath
    moduleText <- readSpecification moduleFile
    interfaceFileResult <- readObjectFile moduleFile moduleText
    moduleInfo <- case interfaceFileResult of
      Just cachedModule -> loadCachedModule moduleFile implicitImports moduleText cachedModule
      Nothing -> parseAndTypeCheckModule moduleFile implicitImports moduleText
    storeModule modulePath moduleInfo
    return moduleInfo

loadCachedModule ::
  (MonadTCMProg m) =>
  FilePath ->
  [ImportStatement] ->
  ModuleText ->
  ModuleInterface Builtin ->
  m ModuleInfo
loadCachedModule moduleFile implicitImports moduleText moduleInterface = do
  let Module imports decls = typedModule moduleInterface
  (status, importedCtx) <- loadImports imports
  case status of
    Changed -> parseAndTypeCheckModule moduleFile implicitImports moduleText
    Unchanged -> do
      freeCtx <- calculateModuleCtx importedCtx decls
      return $
        ModuleInfo
          { moduleInterface = moduleInterface,
            moduleFreeCtx = freeCtx,
            moduleStatus = Unchanged
          }

loadImports ::
  (MonadTCMProg m) =>
  [ImportStatement] ->
  m (ModuleStatus, ImportedModuleContext Builtin)
loadImports imports = do
  results <- forM imports $ \importStatement -> do
    let modulePath = importPath importStatement
    ModuleInfo {..} <- loadModule modulePath
    return (moduleStatus, (modulePath, moduleInterface, moduleFreeCtx))

  let (statuses, importedCtx) = unzipF results
  let finalStatus = foldr (<>) Unchanged statuses
  return (finalStatus, importedCtx)

parseAndTypeCheckModule ::
  (MonadTCMProg m) =>
  FilePath ->
  [ImportStatement] ->
  ModuleText ->
  m ModuleInfo
parseAndTypeCheckModule moduleFile implicitImports moduleText = do
  modulePath <- getCurrentModulePath

  Module imports decls <- parseModuleText (modulePath, moduleFile) moduleText
  let finalImports = implicitImports <> imports
  (_status, importedCtx) <- loadImports finalImports

  let instances =
        if modulePath == standardLibraryDefinitionsModulePath
          then standardBuiltinInstances
          else emptyInstanceDatabase
  (scopedDecls, scopingInterface) <- scopeModuleDecls modulePath importedCtx decls
  (typedDecls, typingInterface, moduleEnv) <- typeCheckModuleDecls modulePath instances importedCtx scopedDecls
  let typedModule = Module finalImports typedDecls

  let moduleInterface =
        ModuleInterface
          { scopingInterface = scopingInterface,
            typingInterface = typingInterface,
            typedModule = typedModule
          }

  writeObjectFile moduleFile moduleText moduleInterface

  return $
    ModuleInfo
      { moduleInterface = moduleInterface,
        moduleFreeCtx = moduleEnv,
        moduleStatus = Changed
      }

calculateModuleCtx ::
  forall m.
  (MonadCompile m) =>
  ImportedModuleContext Builtin ->
  [Decl Builtin] ->
  m (FreeCtx Builtin)
calculateModuleCtx importedCtx = go (mergeImportedFreeCtxs importedCtx)
  where
    go :: FreeCtx Builtin -> [Decl Builtin] -> m (FreeCtx Builtin)
    go env = \case
      [] -> return env
      d : ds -> go (Map.insert (identifierOf d) d env) ds

cyclicImportsError :: (MonadTCMProg m) => ModulePath -> [ModulePath] -> m a
cyclicImportsError newModule previousModules =
  developerError $
    "cyclic module imports not yet handled correctly:"
      <> lineIndent (pretty $ newModule : previousModules)

missingImportError :: (MonadTCMProg m) => ModulePath -> m a
missingImportError modulePath = do
  allModules <- gets availableModules
  developerError $
    "unable to find module" <+> quotePretty modulePath <+> "in imported modules:"
      <> lineIndent (prettyMap pretty pretty allModules)

duplicateModuleError :: ModulePath -> FilePath -> FilePath -> a
duplicateModuleError modulePath filePath1 filePath2 =
  developerError $
    "duplicate files found for module" <+> quotePretty modulePath
      <> ":"
      <> lineIndent (prettyMultiLineList (fmap pretty [filePath1, filePath2]))
