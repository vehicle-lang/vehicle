module Vehicle.TypeCheck
  ( TypeCheckOptions (..),
    typeCheck,
    typeCheckSolitaryExpr,
    parseAndTypeCheckExpr,
    typeCheckUserProg,
    loadLibrary,
    runCompileMonad,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Data (Proxy (..))
import Data.Text as T (Text)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Print.Error
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Serialise
import Vehicle.Compile.Type (typeCheckProg, typeCheckSolitaryExpr)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.Builtin.Linearity.Type ()
import Vehicle.Data.Builtin.Polarity.Type ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Instances
import Vehicle.Data.Builtin.Standard.Type ()
import Vehicle.Data.Variable.Free.Context
import Vehicle.Libraries (Library (..), LibraryInfo (..), findLibraryContentFile)
import Vehicle.Libraries.StandardLibrary (standardLibrary)
import Vehicle.Prelude.Logging.Instance
import Vehicle.Syntax.AST.Expr qualified as S
import Vehicle.Syntax.Parse
import Vehicle.Verify.Specification.IO

data TypeCheckOptions = TypeCheckOptions
  { specification :: FilePath,
    secondaryTypeSystem :: Maybe SecondaryTypeSystem
  }
  deriving (Eq, Show)

typeCheck :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> TypeCheckOptions -> IO ()
typeCheck loggingSettings outputAsJSON options@TypeCheckOptions {..} = runCompileMonad loggingSettings outputAsJSON $ do
  (imports, typedProg) <- typeCheckUserProg options
  let mergedProg = mergeImports imports typedProg
  case secondaryTypeSystem of
    Nothing -> return ()
    Just LinearityTypes -> printPropertyTypes =<< linearityTypeCheck mergedProg
    Just PolarityTypes -> printPropertyTypes =<< polarityTypeCheck mergedProg
    Just DecidabilityTypes -> printPropertyTypes . Right =<< decidabilityTypeCheck mergedProg

--------------------------------------------------------------------------------
-- Useful functions that apply to multiple compiler passes

parseAndTypeCheckExpr :: (MonadIO m, MonadCompile m) => (FilePath, Text) -> m (Expr Builtin)
parseAndTypeCheckExpr expr = do
  standardLibraryProg <- loadLibrary standardLibrary
  freeCtx <- createFreeCtx [standardLibraryProg]
  vehicleExpr <- parseExprText expr
  scopedExpr <- scopeCheckClosedExpr vehicleExpr
  typedExpr <- typeCheckSolitaryExpr standardBuiltinInstances freeCtx scopedExpr
  convertBackToStandardBuiltin typedExpr

parseExprText :: (MonadCompile m) => (FilePath, Text) -> m S.Expr
parseExprText (file, txt) = do
  let location = (ModulePath [User], file)
  case runExcept (parseExpr location =<< readExpr txt) of
    Left err -> throwError $ ParseError location err
    Right expr -> return expr

typeCheckUserProg ::
  (MonadIO m, MonadCompile m) =>
  TypeCheckOptions ->
  m (Imports, Prog Builtin)
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
  S.Prog ->
  m (Prog Builtin)
typeCheckProgram modl imports vehicleProg = do
  scopedProg <- scopeCheck imports vehicleProg
  freeCtx <- createFreeCtx imports
  typedProg <- typeCheckProg modl standardBuiltinInstances freeCtx scopedProg
  traverse convertBackToStandardBuiltin typedProg

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckOrLoadProg ::
  (MonadIO m, MonadCompile m) =>
  Module ->
  Imports ->
  FilePath ->
  m (Prog Builtin)
typeCheckOrLoadProg modl imports specificationFile = do
  spec <- readSpecification specificationFile
  interfaceFileResult <- readObjectFile specificationFile spec
  case interfaceFileResult of
    Just result -> return result
    Nothing -> do
      vehicleProg <- parseProgText (ModulePath [modl], specificationFile) spec
      result <- typeCheckProgram modl imports vehicleProg
      writeObjectFile specificationFile spec result
      return result

parseProgText :: (MonadCompile m) => ParseLocation -> Text -> m S.Prog
parseProgText location txt = do
  case runExcept (readAndParseProg location txt) of
    Left err -> throwError $ ParseError location err
    Right prog -> case traverseDecls (parseDecl location) prog of
      Left err -> throwError $ ParseError location err
      Right prog' -> return prog'

loadLibrary :: (MonadIO m, MonadCompile m) => Library -> m (Prog Builtin)
loadLibrary library = do
  let libname = libraryName $ libraryInfo library
  logCompilerSection MinDetail ("Loading library" <+> quotePretty libname) $ do
    libraryFile <- findLibraryContentFile library
    typeCheckOrLoadProg StdLib mempty libraryFile

printPropertyTypes :: (MonadIO m, MonadCompile m, PrintableBuiltin builtin) => Either CompileError (Prog builtin) -> m ()
printPropertyTypes = \case
  Left err -> throwError err
  Right (Main decls) -> do
    let properties = filter isPropertyDecl decls
    let propertyDocs = fmap propertySummary properties
    let outputDoc = concatWith (\a b -> a <> line <> b) propertyDocs
    programOutput outputDoc
    where
      propertySummary :: (PrintableBuiltin builtin) => Decl builtin -> Doc a
      propertySummary decl = do
        let propertyName = pretty $ identifierName $ identifierOf decl
        let propertyType = prettyFriendlyEmptyCtx (typeOf decl)
        propertyName <+> ":" <+> propertyType

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
    Left err -> do
      let vehicleError = details err
      let outputError = if outputAsJSON then pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON vehicleError else pretty vehicleError
      fatalError outputError
    Right val -> return val

convertBackToStandardBuiltin ::
  (MonadCompile m) =>
  Expr Builtin ->
  m (Expr Builtin)
convertBackToStandardBuiltin = traverseBuiltinsM $
  \p b args -> return $ normAppList (Builtin p b) args

createFreeCtx ::
  (MonadCompile m) =>
  Imports ->
  m (FreeCtx Builtin)
createFreeCtx imports = do
  let decls = [d | imp <- imports, let Main ds = imp, d <- ds]
  runFreshFreeContextT (Proxy @Builtin) (calculateCtx decls)
  where
    calculateCtx ::
      (MonadFreeContext Builtin m) =>
      [Decl Builtin] ->
      m (FreeCtx Builtin)
    calculateCtx = \case
      [] -> getFreeCtx (Proxy @Builtin)
      d : ds -> addDeclToContext d $ calculateCtx ds
