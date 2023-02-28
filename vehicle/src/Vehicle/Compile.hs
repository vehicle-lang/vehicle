module Vehicle.Compile
  ( CompileOptions (..),
    compile,
    typeCheckExpr,
    parseAndTypeCheckExpr,
    loadLibrary,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text as T (Text)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (LDecl, writeLossFunctionFiles)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency.Analysis
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ObjectFile
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print
import Vehicle.Compile.Queries
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (typeCheckWithSubsystem)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck, typeCheckExpr)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Linearity.Core (LinearityType)
import Vehicle.Compile.Type.Subsystem.Polarity.Core (PolarityType)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised
import Vehicle.Libraries (Library (..), LibraryInfo (..), findLibraryContentFile)
import Vehicle.Libraries.StandardLibrary (standardLibrary)
import Vehicle.Syntax.Parse
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)

--------------------------------------------------------------------------------
-- Interface

data CompileOptions = CompileOptions
  { target :: Backend,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String,
    proofCache :: Maybe FilePath,
    noStdlib :: Bool
  }
  deriving (Eq, Show)

compile :: LoggingSettings -> CompileOptions -> IO ()
compile loggingSettings CompileOptions {..} = runCompileMonad loggingSettings $ do
  typeCheckingResult@(_, typedProg) <- typeCheckUserProg specification declarationsToCompile noStdlib
  let resources = Resources networkLocations datasetLocations parameterValues
  case target of
    TypeCheck mode -> case mode of
      Standard -> return ()
      Linearity -> printPropertyTypes =<< typeCheckWithSubsystem @LinearityType typedProg
      Polarity -> printPropertyTypes =<< typeCheckWithSubsystem @PolarityType typedProg
    ITP Agda -> do
      let agdaOptions = AgdaOptions proofCache outputFile moduleName
      compileToAgda agdaOptions typeCheckingResult outputFile
    VerifierBackend verifierIdentifier -> do
      _ <- compileToVerifier typeCheckingResult resources verifierIdentifier outputFile
      return ()
    LossFunction differentiableLogic -> do
      _ <- compileToLossFunction typeCheckingResult resources differentiableLogic outputFile
      return ()

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToVerifier ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  Resources ->
  VerifierIdentifier ->
  Maybe FilePath ->
  m ()
compileToVerifier (imports, typedProg) resources verifierIdentifier outputFile = do
  let mergedProg = mergeImports imports typedProg
  let verifier = verifiers verifierIdentifier
  queries <- compileToQueries verifier mergedProg resources
  outputVerificationQueries verifier outputFile queries

compileToLossFunction ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  Resources ->
  DifferentiableLogic ->
  Maybe FilePath ->
  m [LDecl]
compileToLossFunction (_, typedProg) resources differentiableLogic outputFile = do
  lossFunction <- LossFunction.compile resources differentiableLogic typedProg
  writeLossFunctionFiles outputFile differentiableLogic lossFunction
  return lossFunction

compileToAgda ::
  (MonadCompile m, MonadIO m) =>
  AgdaOptions ->
  (ImportedModules, StandardGluedProg) ->
  Maybe FilePath ->
  m ()
compileToAgda agdaOptions (_, typedProg) outputFile = do
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile outputFile agdaCode

--------------------------------------------------------------------------------
-- Useful functions that apply to multiple compiler passes

parseAndTypeCheckExpr :: (MonadIO m, MonadCompile m) => Text -> m TypeCheckedExpr
parseAndTypeCheckExpr expr = do
  standardLibraryProg <- loadLibrary standardLibrary
  let imports = [standardLibraryProg]
  vehicleExpr <- parseExprText expr
  scopedExpr <- scopeCheckClosedExpr vehicleExpr
  typedExpr <- typeCheckExpr imports (convertToNormalisableBuiltins scopedExpr)
  return typedExpr

parseExprText :: MonadCompile m => Text -> m InputExpr
parseExprText txt =
  case runExcept (parseExpr User =<< readExpr txt) of
    Left err -> throwError $ ParseError err
    Right expr -> return expr

typeCheckUserProg ::
  (MonadIO m, MonadCompile m) =>
  FilePath ->
  DeclarationNames ->
  Bool ->
  m (ImportedModules, StandardGluedProg)
typeCheckUserProg spec declarationsToCompile noStdlib = do
  imports <-
    if noStdlib
      then return []
      else (: []) <$> loadLibrary standardLibrary
  typedProg <- typeCheckOrLoadProg User imports spec declarationsToCompile
  return (imports, typedProg)

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProg ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  ImportedModules ->
  SpecificationText ->
  DeclarationNames ->
  m StandardGluedProg
typeCheckProg modul imports spec declarationsToCompile = do
  vehicleProg <- parseProgText modul spec
  (scopedProg, dependencyGraph) <- scopeCheck imports vehicleProg
  prunedProg <- analyseDependenciesAndPrune scopedProg dependencyGraph declarationsToCompile
  typedProg <- typeCheck imports (fmap convertToNormalisableBuiltins prunedProg)
  return typedProg

mergeImports :: ImportedModules -> StandardGluedProg -> StandardGluedProg
mergeImports imports userProg = Main $ concatMap (\(Main ds) -> ds) (imports <> [userProg])

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckOrLoadProg ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  ImportedModules ->
  FilePath ->
  DeclarationNames ->
  m StandardGluedProg
typeCheckOrLoadProg modul imports specificationFile declarationsToCompile = do
  spec <- readSpecification specificationFile
  interfaceFileResult <- readObjectFile specificationFile spec
  case interfaceFileResult of
    Just result -> return result
    Nothing -> do
      result <- typeCheckProg modul imports spec declarationsToCompile
      writeObjectFile specificationFile spec result
      return result

parseProgText :: MonadCompile m => Module -> Text -> m InputProg
parseProgText modul txt = do
  case runExcept (readAndParseProg modul txt) of
    Left err -> throwError $ ParseError err
    Right prog -> case traverseDecls (parseDecl modul) prog of
      Left err -> throwError $ ParseError err
      Right prog' -> return prog'

loadLibrary :: (MonadIO m, MonadCompile m) => Library -> m StandardGluedProg
loadLibrary library = do
  let libname = libraryName $ libraryInfo library
  logCompilerSection MinDetail ("Loading library" <+> quotePretty libname) $ do
    libraryFile <- findLibraryContentFile library
    typeCheckOrLoadProg StdLib mempty libraryFile mempty

printPropertyTypes :: (MonadIO m, MonadCompile m, PrintableBuiltin builtin) => GluedProg builtin -> m ()
printPropertyTypes (Main decls) = do
  let properties = filter isPropertyDecl decls
  let propertyDocs = fmap propertySummary properties
  let outputDoc = concatWith (\a b -> a <> line <> b) propertyDocs
  programOutput outputDoc
  where
    propertySummary :: PrintableBuiltin builtin => GluedDecl builtin -> Doc a
    propertySummary decl = do
      let propertyName = pretty $ identifierName $ identifierOf decl
      let propertyType = prettyExternal (WithContext (unnormalised $ typeOf decl) emptyDBCtx)
      propertyName <+> ":" <+> propertyType

runCompileMonad ::
  MonadIO m =>
  LoggingSettings ->
  ExceptT CompileError (ImmediateLoggerT m) a ->
  m a
runCompileMonad loggingSettings x =
  fromEitherIO =<< runImmediateLogger loggingSettings (logCompileError x)

fromEitherIO :: MonadIO m => Either CompileError a -> m a
fromEitherIO = \case
  Left err -> fatalError $ pretty $ details err
  Right val -> return val
