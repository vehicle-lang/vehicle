module Vehicle.Compile
  ( CompileOptions(..)
  , compile
  , compileToAgda
  , compileToVerifier
  , typeCheck
  , typeCheckExpr
  , parseAndTypeCheckExpr
  , readSpecification
  , runCompileMonad
  ) where

import Control.Monad.Except ( ExceptT, MonadError(..), runExcept)
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Set (Set)
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
<<<<<<< HEAD
=======
import System.Exit (exitFailure)
import System.IO (hPutStrLn)
import System.FilePath (takeExtension, dropExtension)
>>>>>>> Add interface files to cache type-checking

import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (LDecl, writeLossFunctionFiles)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency.Analysis
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ExpandResources
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Queries (QueryData, compileToQueries)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck, typeCheckExpr)
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Parse
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)
import Vehicle.Verify.Verifier.Interface

data CompileOptions = CompileOptions
  { target                :: Backend
  , specification         :: FilePath
  , declarationsToCompile :: DeclarationNames
  , networkLocations      :: NetworkLocations
  , datasetLocations      :: DatasetLocations
  , parameterValues       :: ParameterValues
  , outputFile            :: Maybe FilePath
  , moduleName            :: Maybe String
  , proofCache            :: Maybe FilePath
  } deriving (Eq, Show)

compile :: LoggingSettings -> CompileOptions -> IO ()
compile loggingSettings CompileOptions{..} = do
  let resources = Resources networkLocations datasetLocations parameterValues
<<<<<<< HEAD
  spec <- readSpecification specification
=======
  spec <- readSpecification loggingOptions specification
  specInterface <- readSpecificationInterface loggingOptions specification
>>>>>>> Add interface files to cache type-checking
  case target of
    TypeCheck -> do
      _ <- runCompileMonad loggingSettings $ typeCheckProg spec declarationsToCompile
      return ()

    ITP Agda -> do
      let agdaOptions = AgdaOptions proofCache outputFile moduleName
      agdaCode <- compileToAgda loggingSettings agdaOptions spec declarationsToCompile resources
      writeAgdaFile outputFile agdaCode

    VerifierBackend verifierIdentifier -> do
      let verifier = verifiers verifierIdentifier
      compiledSpecification <- compileToVerifier loggingSettings spec declarationsToCompile resources verifier
      case outputFile of
        Nothing     -> outputSpecification compiledSpecification
        Just folder -> writeSpecificationFiles verifier folder compiledSpecification

    LossFunction differentiableLogic -> do
      lossFunction <- compileToLossFunction loggingSettings spec declarationsToCompile resources differentiableLogic
      writeLossFunctionFiles outputFile differentiableLogic lossFunction


--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToVerifier :: LoggingSettings
                  -> SpecificationText
                  -> PropertyNames
                  -> Resources
                  -> Verifier
                  -> IO (Specification QueryData)
compileToVerifier loggingSettings spec properties resources verifier =
  runCompileMonad loggingSettings $ do
    (typedProg, propertyCtx) <- typeCheckProg spec properties
    (networkCtx, finalProg) <- expandResources resources True typedProg
    compileToQueries verifier finalProg propertyCtx networkCtx


compileToLossFunction :: LoggingSettings
                      -> SpecificationText
                      -> DeclarationNames
                      -> Resources
                      -> DifferentiableLogic
                      -> IO [LDecl]
compileToLossFunction loggingSettings spec declarationsToCompile resources differentiableLogic = do
  runCompileMonad loggingSettings $ do
    (typedProg, propertyCtx) <- typeCheckProg spec declarationsToCompile
    (networkCtx, finalProg) <- expandResources resources True typedProg
    LossFunction.compile differentiableLogic finalProg propertyCtx networkCtx

compileToAgda :: LoggingSettings
              -> AgdaOptions
              -> SpecificationText
              -> PropertyNames
              -> Resources
              -> IO (Doc a)
compileToAgda loggingSettings agdaOptions spec properties _resources =
  runCompileMonad loggingSettings $ do
    (prog, propertyCtx) <- typeCheckProg spec properties
    compileProgToAgda (fmap unnormalised prog) propertyCtx agdaOptions

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

<<<<<<< HEAD
readSpecification :: MonadIO m => FilePath -> m SpecificationText
readSpecification inputFile = do
  liftIO $ TIO.readFile inputFile `catch` \ (e :: IOException) ->
    fatalError $ "Error occured while reading input file:" <+> line <>
      indent 2 (pretty (show e))
=======
readSpecification :: MonadIO m => VehicleIOSettings -> FilePath -> m SpecificationText
readSpecification VehicleIOSettings{..} inputFile
  | takeExtension inputFile == vehicleFileExtension = liftIO $ do
    hPutStrLn errorHandle $
      "Specification file does not have the correct extension (" <> vehicleFileExtension <> ")"
    exitFailure
  | otherwise = liftIO $
    TIO.readFile inputFile `catch` \ (e :: IOException) -> do
      hPutStrLn errorHandle $
        "Error occured while reading specification file: \n  " <> show e
      exitFailure

readSpecificationInterface :: MonadIO m
                           => VehicleIOSettings
                           -> FilePath
                           -> m (Maybe CheckedProg)
readSpecificationInterface VehicleIOSettings{..} inputFile = do
  let interfaceFile = dropExtension inputFile <> vehicleInterfaceFileExtension
  interfaceContent <- liftIO $ do
    (Just <$> TIO.readFile interfaceFile) `catch` \ (_ :: IOException) -> return Nothing

  return _
>>>>>>> Add interface files to cache type-checking

parseAndTypeCheckExpr :: MonadCompile m => Text -> m CheckedExpr
parseAndTypeCheckExpr expr = do
  vehicleExpr <- parseExprText expr
  scopedExpr  <- scopeCheckClosedExpr vehicleExpr
  typedExpr   <- typeCheckExpr scopedExpr
  return typedExpr

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProg :: MonadCompile m
              => SpecificationText
              -> DeclarationNames
              -> m (GluedProg, PropertyContext)
typeCheckProg spec declarationsToCompile = do
<<<<<<< HEAD
  (vehicleProg, uncheckedPropertyCtx) <- parseProgText spec
  (scopedProg, dependencyGraph) <- scopeCheck vehicleProg
  prunedProg <- analyseDependenciesAndPrune scopedProg uncheckedPropertyCtx dependencyGraph declarationsToCompile
  (typedProg, propertyContext) <- typeCheck prunedProg uncheckedPropertyCtx
  return (typedProg, propertyContext)
=======
  interfaceFileResult <- _
  case interfaceFileResult of
    Just _  -> _
    Nothing -> do
      (vehicleProg, uncheckedPropertyCtx) <- parseProgText spec
      (scopedProg, dependencyGraph) <- scopeCheck vehicleProg
      prunedProg <- analyseDependenciesAndPrune scopedProg uncheckedPropertyCtx dependencyGraph declarationsToCompile
      (typedProg, propertyContext) <- typeCheck prunedProg uncheckedPropertyCtx
      _
      return (typedProg, propertyContext, dependencyGraph)

-- | Parses, expands parameters and datasets, type-checks and then
-- checks the network types from disk. Used during compilation to
-- verification queries.
typeCheckProgAndLoadResources :: (MonadIO m, MonadCompile m)
                              => SpecificationText
                              -> DeclarationNames
                              -> Resources
                              -> m (CheckedProg, PropertyContext, NetworkContext, DependencyGraph)
typeCheckProgAndLoadResources spec declarationsToCompile resources = do
  (typedProg, propertyCtx, depGraph) <- typeCheckProg spec declarationsToCompile
  (networkCtx, finalProg) <- expandResources resources True typedProg
  return (finalProg, propertyCtx, networkCtx, depGraph)
>>>>>>> Add interface files to cache type-checking

parseExprText :: MonadCompile m => Text -> m InputExpr
parseExprText txt =
  case runExcept (parseExpr =<< readExpr txt) of
    Left  err  -> throwError $ ParseError err
    Right expr -> return expr

parseProgText :: MonadCompile m => Text -> m (InputProg, Set Identifier)
parseProgText txt = do
  case runExcept (readAndParseProg txt) of
    Left err                 -> throwError $ ParseError err
    Right (prog, properties) -> case traverse parseExpr prog of
      Left err    -> throwError $ ParseError err
      Right prog' -> return (prog', properties)

runCompileMonad :: MonadIO m
                => LoggingSettings
                -> ExceptT CompileError (ImmediateLoggerT m) a
                -> m a
runCompileMonad loggingSettings x =
  fromEitherIO =<< runImmediateLogger loggingSettings (logCompileError x)

fromEitherIO :: MonadIO m => Either CompileError a -> m a
fromEitherIO = \case
  Left  err -> fatalError $ pretty $ details err
  Right val -> return val
