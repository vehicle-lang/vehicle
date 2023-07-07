module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Backend.Agda
import Vehicle.Backend.JSON (compileProgToJSON)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Compile.Dependency (analyseDependenciesAndPrune)
import Vehicle.Compile.Descope (DescopeNamed (descopeNamed))
import Vehicle.Compile.Error
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Queries
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (removeLiteralCoercions, resolveInstanceArguments)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised (GluedExpr (..))
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification (VerificationPlan (VerificationPlan))
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (queryFormats)

--------------------------------------------------------------------------------
-- Interface

data CompileOptions = CompileOptions
  { target :: Target,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String,
    proofCache :: Maybe FilePath,
    outputAsJSON :: Bool
  }
  deriving (Eq, Show)

compile :: LoggingSettings -> CompileOptions -> IO ()
compile loggingSettings CompileOptions {..} = runCompileMonad loggingSettings $ do
  (imports, prog) <-
    typeCheckUserProg $
      TypeCheckOptions
        { specification = specification,
          typingSystem = Standard
        }

  prunedProg <- analyseDependenciesAndPrune unnormalised prog declarationsToCompile
  let result = (imports, prunedProg)

  let resources = Resources specification networkLocations datasetLocations parameterValues
  case target of
    VerifierQueries queryFormatID ->
      compileToQueryFormat result resources queryFormatID outputFile
    LossFunction differentiableLogic ->
      compileToLossFunction result differentiableLogic outputFile outputAsJSON
    ITP Agda -> do
      let agdaOptions = AgdaOptions proofCache outputFile moduleName
      compileToAgda agdaOptions result outputFile
    ExplicitVehicle -> do
      compileDirect result outputFile outputAsJSON

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  Resources ->
  QueryFormatID ->
  Maybe FilePath ->
  m ()
compileToQueryFormat (imports, typedProg) resources queryFormatID outputFile = do
  let mergedProg = mergeImports imports typedProg
  let verifier = queryFormats queryFormatID
  queryData <- compileToQueries verifier mergedProg resources
  let (queryStructure, queryText) = (NonEmpty.unzip . fmap (NonEmpty.unzip . fmap NonEmpty.unzip)) queryData
  integrityInfo <- generateResourcesIntegrityInfo resources
  let verificationPlan = VerificationPlan queryStructure integrityInfo
  outputCompilationResults queryFormatID outputFile (verificationPlan, queryText)

compileToAgda ::
  (MonadCompile m, MonadIO m) =>
  AgdaOptions ->
  (ImportedModules, StandardGluedProg) ->
  Maybe FilePath ->
  m ()
compileToAgda agdaOptions (_, typedProg) outputFile = do
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile outputFile agdaCode

compileToLossFunction ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  DifferentiableLogicID ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction (imports, typedProg) differentiableLogic outputFile outputAsJSON = do
  let mergedProg = unnormalised <$> mergeImports imports typedProg
  functionalisedProg <- functionaliseResources mergedProg
  resolvedProg <- resolveInstanceArguments functionalisedProg
  lossProg <- LossFunction.compile differentiableLogic resolvedProg
  compileToJSON lossProg outputFile outputAsJSON

compileDirect ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  Maybe FilePath ->
  Bool ->
  m ()
compileDirect (imports, typedProg) outputFile outputAsJSON = do
  let mergedProg = unnormalised <$> mergeImports imports typedProg
  functionalisedProg <- functionaliseResources mergedProg
  resolvedProg <- resolveInstanceArguments functionalisedProg
  compileToJSON resolvedProg outputFile outputAsJSON

compileToJSON ::
  (MonadCompile m, MonadIO m) =>
  StandardProg ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToJSON prog outputFile outputAsJSON = do
  literalCoercionFreeProg <- removeLiteralCoercions prog
  monomorphiseProg <- monomorphise (\d -> moduleOf (identifierOf d) == User) literalCoercionFreeProg
  let namedProg = descopeNamed monomorphiseProg
  result <-
    if outputAsJSON
      then do
        compileProgToJSON namedProg
      else do
        return $ prettyFriendly namedProg
  writeResultToFile Nothing outputFile result
