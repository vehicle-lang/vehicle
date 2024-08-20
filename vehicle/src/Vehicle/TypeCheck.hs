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
import Data.Data (Proxy (..))
import Data.Text as T (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ObjectFile
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheckExpr, typeCheckProg)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Data.Builtin.Linearity.Core (LinearityBuiltin)
import Vehicle.Data.Builtin.Polarity.Core (PolarityBuiltin)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.InstanceBuiltins
import Vehicle.Libraries (Library (..), LibraryInfo (..), findLibraryContentFile)
import Vehicle.Libraries.StandardLibrary (standardLibrary)
import Vehicle.Prelude.Logging.Instance
import Vehicle.Syntax.Parse
import Vehicle.Verify.Specification.IO

data TypeCheckOptions = TypeCheckOptions
  { specification :: FilePath,
    typingSystem :: TypingSystem
  }
  deriving (Eq, Show)

typeCheck :: (MonadStdIO IO) => LoggingSettings -> TypeCheckOptions -> IO ()
typeCheck loggingSettings options@TypeCheckOptions {..} = runCompileMonad loggingSettings $ do
  (imports, typedProg) <- typeCheckUserProg options
  let mergedProg = mergeImports imports typedProg
  case typingSystem of
    Standard -> return ()
    Linearity -> printPropertyTypes =<< typeCheckWithSubsystem @LinearityBuiltin mempty throwError mergedProg
    Polarity -> printPropertyTypes =<< typeCheckWithSubsystem @PolarityBuiltin mempty throwError mergedProg

--------------------------------------------------------------------------------
-- Useful functions that apply to multiple compiler passes

parseAndTypeCheckExpr :: (MonadIO m, MonadCompile m) => (FilePath, Text) -> m (Expr Ix Builtin)
parseAndTypeCheckExpr expr = do
  standardLibraryProg <- loadLibrary standardLibrary
  freeCtx <- createFreeCtx [standardLibraryProg]
  vehicleExpr <- parseExprText expr
  scopedExpr <- scopeCheckClosedExpr vehicleExpr
  typedExpr <- typeCheckExpr standardBuiltinInstances freeCtx scopedExpr
  convertBackToStandardBuiltin typedExpr

parseExprText :: (MonadCompile m) => (FilePath, Text) -> m (Expr Name Builtin)
parseExprText (file, txt) = do
  let location = (ModulePath [User], file)
  case runExcept (parseExpr location =<< readExpr txt) of
    Left err -> throwError $ ParseError location err
    Right expr -> return expr

typeCheckUserProg ::
  (MonadIO m, MonadCompile m) =>
  TypeCheckOptions ->
  m (Imports, Prog Ix Builtin)
typeCheckUserProg TypeCheckOptions {..} = do
  imports <- (: []) <$> loadLibrary standardLibrary
  typedProg <- typeCheckOrLoadProg User imports specification
  return (imports, typedProg)

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProgram ::
  (MonadIO m, MonadCompile m) =>
  ParseLocation ->
  Imports ->
  SpecificationText ->
  m (Prog Ix Builtin)
typeCheckProgram modul imports spec = do
  vehicleProg <- parseProgText modul spec
  scopedProg <- scopeCheck imports vehicleProg
  freeCtx <- createFreeCtx imports
  typedProg <- typeCheckProg standardBuiltinInstances freeCtx scopedProg
  traverse convertBackToStandardBuiltin typedProg

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckOrLoadProg ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  Imports ->
  FilePath ->
  m (Prog Ix Builtin)
typeCheckOrLoadProg modl imports specificationFile = do
  spec <- readSpecification specificationFile
  interfaceFileResult <- readObjectFile specificationFile spec
  case interfaceFileResult of
    Just result -> return result
    Nothing -> do
      result <- typeCheckProgram (ModulePath [modl], specificationFile) imports spec
      writeObjectFile specificationFile spec result
      return result

parseProgText :: (MonadCompile m) => ParseLocation -> Text -> m (Prog Name Builtin)
parseProgText location txt = do
  case runExcept (readAndParseProg location txt) of
    Left err -> throwError $ ParseError location err
    Right prog -> case traverseDecls (parseDecl location) prog of
      Left err -> throwError $ ParseError location err
      Right prog' -> return prog'

loadLibrary :: (MonadIO m, MonadCompile m) => Library -> m (Prog Ix Builtin)
loadLibrary library = do
  let libname = libraryName $ libraryInfo library
  logCompilerSection MinDetail ("Loading library" <+> quotePretty libname) $ do
    libraryFile <- findLibraryContentFile library
    typeCheckOrLoadProg StdLib mempty libraryFile

printPropertyTypes :: (MonadIO m, MonadCompile m, PrintableBuiltin builtin) => Prog Ix builtin -> m ()
printPropertyTypes (Main decls) = do
  let properties = filter isPropertyDecl decls
  let propertyDocs = fmap propertySummary properties
  let outputDoc = concatWith (\a b -> a <> line <> b) propertyDocs
  programOutput outputDoc
  where
    propertySummary :: (PrintableBuiltin builtin) => Decl Ix builtin -> Doc a
    propertySummary decl = do
      let propertyName = pretty $ identifierName $ identifierOf decl
      let propertyType = prettyFriendlyEmptyCtx (typeOf decl)
      propertyName <+> ":" <+> propertyType

runCompileMonad ::
  forall m a.
  (MonadStdIO m) =>
  LoggingSettings ->
  (forall n. (MonadStdIO n, MonadLogger n) => ExceptT CompileError n a) ->
  m a
runCompileMonad loggingSettings x = do
  errorOrResult <- runLoggerT loggingSettings (logCompileError x)
  case errorOrResult of
    Left err -> fatalError $ pretty $ details err
    Right val -> return val

convertBackToStandardBuiltin ::
  (MonadCompile m) =>
  Expr Ix Builtin ->
  m (Expr Ix Builtin)
convertBackToStandardBuiltin = traverseBuiltinsM $
  \p b args -> return $ normAppList (Builtin p b) args

createFreeCtx ::
  (MonadCompile m) =>
  Imports ->
  m (FreeCtx Builtin)
createFreeCtx imports = do
  let decls = [d | imp <- imports, let Main ds = imp, d <- ds]
  convertedDecls <- traverse (traverse (traverseBuiltinsM convertToTypingBuiltins)) decls
  runFreshFreeContextT (Proxy @Builtin) (calculateCtx convertedDecls)
  where
    calculateCtx ::
      (MonadFreeContext Builtin m) =>
      [Decl Ix Builtin] ->
      m (FreeCtx Builtin)
    calculateCtx = \case
      [] -> getFreeCtx (Proxy @Builtin)
      d : ds -> addDeclToContext (Proxy @Builtin) d $ calculateCtx ds
