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
import Vehicle.Backend.Queries.Error.Linearity.Core (LinearityBuiltin)
import Vehicle.Backend.Queries.Error.Polarity.Core (PolarityBuiltin)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ObjectFile
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheckExpr, typeCheckProg)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.InstanceBuiltins
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

parseAndTypeCheckExpr :: (MonadIO m, MonadCompile m) => Text -> m (Expr Ix Builtin)
parseAndTypeCheckExpr expr = do
  standardLibraryProg <- loadLibrary standardLibrary
  freeCtx <- createFreeCtx [standardLibraryProg]
  vehicleExpr <- parseExprText expr
  scopedExpr <- scopeCheckClosedExpr vehicleExpr
  typedExpr <- typeCheckExpr standardBuiltinInstances freeCtx scopedExpr
  convertBackToStandardBuiltin typedExpr

parseExprText :: (MonadCompile m) => Text -> m (Expr Name Builtin)
parseExprText txt =
  case runExcept (parseExpr User =<< readExpr txt) of
    Left err -> throwError $ ParseError User err
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
  Module ->
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
    Left err -> throwError $ ParseError modul err
    Right prog -> case traverseDecls (parseDecl modul) prog of
      Left err -> throwError $ ParseError modul err
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
  Expr Ix StandardTypingBuiltin ->
  m (Expr Ix Builtin)
convertBackToStandardBuiltin = traverseBuiltinsM $
  \p b args -> case b of
    StandardBuiltin c -> return $ normAppList (Builtin p c) args

createFreeCtx ::
  (MonadCompile m) =>
  Imports ->
  m (FreeCtx StandardTypingBuiltin)
createFreeCtx imports = do
  let decls = [d | imp <- imports, let Main ds = imp, d <- ds]
  convertedDecls <- traverse (traverse (traverseBuiltinsM convertToTypingBuiltins)) decls
  runFreshFreeContextT (Proxy @StandardTypingBuiltin) (calculateCtx convertedDecls)
  where
    calculateCtx ::
      (MonadFreeContext StandardTypingBuiltin m) =>
      [Decl Ix StandardTypingBuiltin] ->
      m (FreeCtx StandardTypingBuiltin)
    calculateCtx = \case
      [] -> getFreeCtx (Proxy @StandardTypingBuiltin)
      d : ds -> addDeclToContext d $ calculateCtx ds
