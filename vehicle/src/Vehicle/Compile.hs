module Vehicle.Compile
  ( CompileOptions(..)
  , compile
  , compileToAgda
  , compileToVerifier
  , typeCheckOrLoadProg
  , typeCheckProg
  , typeCheckExpr
  , parseAndTypeCheckExpr
  , readSpecification
  , runCompileMonad
  ) where

import Control.Exception (IOException, catch)
import Control.Monad.Except (ExceptT, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
import System.FilePath (takeExtension)

import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (LDecl, writeLossFunctionFiles)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency.Analysis
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.ObjectFile
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Queries (QueryData, compileToQueries)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (TypedProg, typeCheck, typeCheckExpr)
import Vehicle.Syntax.Parse
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)

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
compile loggingSettings CompileOptions{..} = runCompileMonad loggingSettings $ do
  let resources = Resources networkLocations datasetLocations parameterValues
  typeCheckingResult <- typeCheckOrLoadProg specification declarationsToCompile

  case target of
    TypeCheck -> return ()

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

compileToVerifier :: (MonadCompile m, MonadIO m)
                  => TypedProg
                  -> Resources
                  -> VerifierIdentifier
                  -> Maybe FilePath
                  -> m (Specification QueryData)
compileToVerifier typedProg resources verifierIdentifier outputFile = do
  let verifier = verifiers verifierIdentifier
  compiledSpecification <- compileToQueries verifier typedProg resources
  case outputFile of
    Nothing     -> outputSpecification compiledSpecification
    Just folder -> writeSpecificationFiles verifier folder compiledSpecification
  return compiledSpecification

compileToLossFunction :: (MonadCompile m, MonadIO m)
                      => TypedProg
                      -> Resources
                      -> DifferentiableLogic
                      -> Maybe FilePath
                      -> m [LDecl]
compileToLossFunction typedProg resources differentiableLogic outputFile = do
  lossFunction <- LossFunction.compile resources differentiableLogic typedProg
  writeLossFunctionFiles outputFile differentiableLogic lossFunction
  return lossFunction

compileToAgda :: (MonadCompile m, MonadIO m)
              => AgdaOptions
              -> TypedProg
              -> Maybe FilePath
              -> m ()
compileToAgda agdaOptions typedProg outputFile = do
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile outputFile agdaCode

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

readSpecification :: MonadIO m => FilePath -> m SpecificationText
readSpecification inputFile
  | takeExtension inputFile /= vehicleFileExtension = do
    fatalError $
      "Specification" <+> quotePretty inputFile <+> "has unsupported" <+>
      "extension" <+> quotePretty (takeExtension inputFile) <> "." <+>
      "Only files with a" <+> quotePretty vehicleFileExtension <+>
      "extension are supported."
  | otherwise = liftIO $
    TIO.readFile inputFile `catch` \ (e :: IOException) -> do
      fatalError $
        "Error occured while reading specification" <+>
        quotePretty inputFile <> ":" <> line <>
          indent 2 (pretty (show e))

parseAndTypeCheckExpr :: MonadCompile m => Text -> m CheckedExpr
parseAndTypeCheckExpr expr = do
  vehicleExpr <- parseExprText expr
  scopedExpr  <- scopeCheckClosedExpr vehicleExpr
  typedExpr   <- typeCheckExpr scopedExpr
  return typedExpr

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProg :: (MonadIO m, MonadCompile m)
              => SpecificationText
              -> DeclarationNames
              -> m TypedProg
typeCheckProg spec declarationsToCompile = do
  vehicleProg <- parseProgText spec
  (scopedProg, dependencyGraph) <- scopeCheck vehicleProg
  prunedProg <- analyseDependenciesAndPrune scopedProg dependencyGraph declarationsToCompile
  result <- typeCheck prunedProg
  return result

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckOrLoadProg :: (MonadIO m, MonadCompile m)
                    => FilePath
                    -> DeclarationNames
                    -> m TypedProg
typeCheckOrLoadProg specificationFile declarationsToCompile = do
  spec <- readSpecification specificationFile
  interfaceFileResult <- readObjectFile specificationFile spec
  case interfaceFileResult of
    Just result -> return result
    Nothing     -> do
      result <- typeCheckProg spec declarationsToCompile
      writeObjectFile specificationFile spec result
      return result

parseExprText :: MonadCompile m => Text -> m InputExpr
parseExprText txt =
  case runExcept (parseExpr =<< readExpr txt) of
    Left  err  -> throwError $ ParseError err
    Right expr -> return expr

parseProgText :: MonadCompile m => Text -> m InputProg
parseProgText txt = do
  case runExcept (readAndParseProg txt) of
    Left  err  -> throwError $ ParseError err
    Right prog -> case traverse parseExpr prog of
      Left err    -> throwError $ ParseError err
      Right prog' -> return prog'

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
