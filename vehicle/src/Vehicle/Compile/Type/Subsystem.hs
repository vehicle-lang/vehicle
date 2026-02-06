module Vehicle.Compile.Type.Subsystem
  ( polarityTypeCheck,
    linearityTypeCheck,
    decidabilityTypeCheck,
    parseModuleText,
  )
where

import Control.Monad.Except (MonadError (..), runExcept, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency (pruneUnusedDeclarations)
import Vehicle.Compile.Error
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Print.Error (errorInSubsystemMessage)
import Vehicle.Compile.Sugar.Desugar (elabModule)
import Vehicle.Compile.Type (typeCheckModuleDecls)
import Vehicle.Compile.Type.Core (InstanceDatabase, emptyInstanceDatabase)
import Vehicle.Compile.Type.Irrelevance
import Vehicle.Compile.Type.System
import Vehicle.Data.AST.Expr.Desugared qualified as S
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..))
import Vehicle.Data.Builtin.Decidability.Instances (decidabilityBuiltinInstances)
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity (PolarityBuiltin)
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ModuleInterface (ImportedModuleContext, ModuleInterface (..), emptyModuleScopingInterface, emptyModuleTypingInterface)
import Vehicle.Libraries.StandardLibrary (standardLibraryBuiltinModulePath)
import Vehicle.Syntax.Parse (parseExternalModule)

polarityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  Set Identifier ->
  m (Either CompileError (Prog PolarityBuiltin))
polarityTypeCheck prog declarationsToCompile = do
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (`Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  typeCheckWithSubsystem PolarityTypes emptyInstanceDatabase implicitFreeProg

linearityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  Set Identifier ->
  m (Either CompileError (Prog LinearityBuiltin))
linearityTypeCheck prog declarationsToCompile = do
  let keepUnused = if Set.null declarationsToCompile then isUserCode else (`Set.member` declarationsToCompile)
  monomorphisedProg <- monomorphise prog keepUnused
  irrelevantFreeProg <- removeIrrelevantCodeFromProg monomorphisedProg
  implicitFreeProg <- removeImplicitArgs irrelevantFreeProg
  typeCheckWithSubsystem LinearityTypes emptyInstanceDatabase implicitFreeProg

decidabilityTypeCheck ::
  (MonadIO m, MonadCompile m) =>
  Prog Builtin ->
  m (Prog DecidabilityBuiltin)
decidabilityTypeCheck prog = do
  prunedProg <- pruneUnusedDeclarations prog
  errorOrDecProg <- typeCheckWithSubsystem DecidabilityTypes decidabilityBuiltinInstances prunedProg
  decProg <- case errorOrDecProg of
    Left err -> developerError $ errorInSubsystemMessage "determine the decidability of the program for export to ITP" err
    Right decProg -> return decProg

  monomorphise decProg isUserCode

typeCheckWithSubsystem ::
  (MonadIO m, MonadCompile m, HasTypeSystem builtin) =>
  SecondaryTypeSystem ->
  InstanceDatabase builtin ->
  Prog Builtin ->
  m (Either CompileError (Prog builtin))
typeCheckWithSubsystem typingSystem instanceCandidates prog = do
  callDepth <- getCallDepth
  logCompilerSection2 MinDetail ("typing using" <+> quotePretty typingSystem <+> "type subsystem") $ do
    logCompilerPass TypingSubsystem $ do
      builtinModuleCtx <- loadTypeSystemBuiltins typingSystem instanceCandidates
      errorOrResult <- runExceptT $ typeCheckModuleDecls userModulePath instanceCandidates builtinModuleCtx (programDeclarations prog)
      -- Need to reset the call depth explicitly as type-checking may have errored.
      setCallDepth (callDepth + 1)
      return $ case errorOrResult of
        Left err -> Left err
        Right (decls, _, _) -> Right $ Main decls

loadTypeSystemBuiltins ::
  (MonadIO m, MonadCompile m, HasTypeSystem builtin) =>
  SecondaryTypeSystem ->
  InstanceDatabase builtin ->
  m (ImportedModuleContext builtin)
loadTypeSystemBuiltins typeSystem _instanceCandidates = do
  -- Locate the builtin module file
  let builtinModulePath = standardLibraryBuiltinModulePath (Just typeSystem)
  -- standardLibraryPath <- getLibraryPath standardLibraryName
  -- let builtinModuleFile = calculateModuleFilePath standardLibraryPath builtinModulePath

  -- Parse the builtin file
  -- builtinModuleText <- readSpecification builtinModuleFile
  -- builtinModule <- parseModuleText (builtinModulePath, builtinModuleFile) builtinModuleText
  -- let builtinModuleDecls = moduleDeclarations builtinModule

  -- Scope and type the builtin file
  -- (scopedDecls, scopingInterface) <- scopeModuleDecls builtinModulePath mempty builtinModuleDecls
  -- (typedDecls, typingInterface, freeEnv) <- typeCheckModuleDecls builtinModulePath instanceCandidates mempty scopedDecls
  let freeEnv = mempty
  let typedDecls = mempty

  -- Add in the builtins
  let finalInterface =
        ModuleInterface
          { scopingInterface = emptyModuleScopingInterface,
            typingInterface = emptyModuleTypingInterface,
            typedModule = Module mempty typedDecls
          }
  return [(builtinModulePath, finalInterface, freeEnv)]

removeImplicitArgs ::
  forall m builtin.
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
removeImplicitArgs prog =
  logCompilerSection2 MaxDetail "removal of implicit arguments" $ do
    result <- traverse go prog
    logCompilerPassOutput $ prettyExternal result
    return result
  where
    go :: Expr builtin -> m (Expr builtin)
    go expr = case expr of
      App fun args -> do
        fun' <- go fun
        let nonImplicitArgs = NonEmpty.filter (not . isImplicit) args
        nonImplicitArgs' <- traverse (traverse go) nonImplicitArgs
        return $ normAppList fun' nonImplicitArgs'
      BoundVar {} -> return expr
      FreeVar {} -> return expr
      Universe {} -> return expr
      Meta {} -> return expr
      Hole {} -> return expr
      Builtin {} -> return expr
      Pi p binder res -> Pi p <$> traverse go binder <*> go res
      Lam p binder body -> Lam p <$> traverse go binder <*> go body
      Let p bound binder body -> Let p <$> go bound <*> traverse go binder <*> go body
      Record p ident fields -> Record p ident <$> traverseRecordFields go fields
      RecordProj p recordType record field -> RecordProj p <$> go recordType <*> go record <*> pure field

parseModuleText :: (MonadCompile m) => ParseLocation -> Text -> m (S.Module Builtin)
parseModuleText location txt = do
  case runExcept (readAndParseModule location txt) of
    Left err -> throwError $ ParseError location err
    Right modul -> return modul

readAndParseModule :: (MonadError ParseError m) => ParseLocation -> Text -> m (S.Module Builtin)
readAndParseModule modul txt = castBNFCError (elabModule modul) (parseExternalModule txt)

castBNFCError :: (MonadError ParseError m) => (a -> m b) -> Either String a -> m b
castBNFCError f = \case
  Left err -> throwError $ RawParseError err
  Right value -> f value
