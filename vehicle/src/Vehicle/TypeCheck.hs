module Vehicle.TypeCheck
  ( TypeCheckOptions (..),
    typeCheck,
    typeCheckExpr,
    parseAndTypeCheckExpr,
    typeCheckUserProg,
    loadLibrary,
    runCompileMonad,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text as T (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ObjectFile
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (typeCheckWithSubsystem)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheckExpr, typeCheckProg)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Linearity.Core (LinearityType)
import Vehicle.Compile.Type.Subsystem.Polarity.Core (PolarityType)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised
import Vehicle.Libraries (Library (..), LibraryInfo (..), findLibraryContentFile)
import Vehicle.Libraries.StandardLibrary (standardLibrary)
import Vehicle.Syntax.Parse
import Vehicle.Verify.Specification.IO

data TypeCheckOptions = TypeCheckOptions
  { specification :: FilePath,
    typingSystem :: TypingSystem
  }
  deriving (Eq, Show)

typeCheck :: LoggingSettings -> TypeCheckOptions -> IO ()
typeCheck loggingSettings options@TypeCheckOptions {..} = runCompileMonad loggingSettings $ do
  (_, typedProg) <- typeCheckUserProg options
  case typingSystem of
    Standard -> return ()
    Linearity -> printPropertyTypes =<< typeCheckWithSubsystem @LinearityType typedProg
    Polarity -> printPropertyTypes =<< typeCheckWithSubsystem @PolarityType typedProg

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

parseExprText :: (MonadCompile m) => Text -> m (Expr Name Builtin)
parseExprText txt =
  case runExcept (parseExpr User =<< readExpr txt) of
    Left err -> throwError $ ParseError err
    Right expr -> return expr

typeCheckUserProg ::
  (MonadIO m, MonadCompile m) =>
  TypeCheckOptions ->
  m (ImportedModules, StandardGluedProg)
typeCheckUserProg TypeCheckOptions {..} = do
  imports <- (: []) <$> loadLibrary standardLibrary
  typedProg <- typeCheckOrLoadProg User imports specification
  return (imports, typedProg)

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProgram ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  ImportedModules ->
  SpecificationText ->
  m StandardGluedProg
typeCheckProgram modul imports spec = do
  vehicleProg <- parseProgText modul spec
  scopedProg <- scopeCheck imports vehicleProg
  typedProg <- typeCheckProg imports (fmap convertToNormalisableBuiltins scopedProg)
  return typedProg

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckOrLoadProg ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  ImportedModules ->
  FilePath ->
  m StandardGluedProg
typeCheckOrLoadProg modul imports specificationFile = do
  spec <- readSpecification specificationFile
  interfaceFileResult <- readObjectFile specificationFile spec
  case interfaceFileResult of
    Just result -> return result
    Nothing -> do
      result <- typeCheckProgram modul imports spec
      writeObjectFile specificationFile spec result
      return result

parseProgText :: (MonadCompile m) => Module -> Text -> m (Prog Name Builtin)
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
    typeCheckOrLoadProg StdLib mempty libraryFile

printPropertyTypes :: (MonadIO m, MonadCompile m, PrintableBuiltin builtin) => GluedProg builtin -> m ()
printPropertyTypes (Main decls) = do
  let properties = filter isPropertyDecl decls
  let propertyDocs = fmap propertySummary properties
  let outputDoc = concatWith (\a b -> a <> line <> b) propertyDocs
  programOutput outputDoc
  where
    propertySummary :: (PrintableBuiltin builtin) => GluedDecl builtin -> Doc a
    propertySummary decl = do
      let propertyName = pretty $ identifierName $ identifierOf decl
      let propertyType = prettyExternal (WithContext (unnormalised $ typeOf decl) emptyDBCtx)
      propertyName <+> ":" <+> propertyType

runCompileMonad ::
  (MonadIO m) =>
  LoggingSettings ->
  ExceptT CompileError (ImmediateLoggerT m) a ->
  m a
runCompileMonad loggingSettings x = do
  errorOrResult <- runImmediateLogger loggingSettings (logCompileError x)
  case errorOrResult of
    Left err -> fatalError $ pretty $ details err
    Right val -> return val
