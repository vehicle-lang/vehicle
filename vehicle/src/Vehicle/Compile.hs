module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Data.Hashable (Hashable)
import Vehicle.Backend.Agda
import Vehicle.Backend.JSON (ToJBuiltin, compileProgToJSON)
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.Prelude
import Vehicle.Backend.Queries
import Vehicle.Backend.Tensors.Clean (cleanUpHigherOrderStuff)
import Vehicle.Compile.Dependency (analyseDependenciesAndPrune)
import Vehicle.Compile.Error
import Vehicle.Compile.EtaConversion (etaExpandProg)
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Monomorphisation (hoistInferableParameters, monomorphise, removeLiteralCoercions)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Irrelevance (removeIrrelevantCodeFromProg)
import Vehicle.Compile.Type.Subsystem (resolveInstanceArguments)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)
import Vehicle.Verify.QueryFormat

--------------------------------------------------------------------------------
-- Interface

data CompileOptions = CompileOptions
  { target :: Target,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    output :: Maybe FilePath,
    moduleName :: Maybe String,
    verificationCache :: Maybe FilePath,
    outputAsJSON :: Bool
  }
  deriving (Eq, Show)

compile :: (MonadStdIO IO) => LoggingSettings -> CompileOptions -> IO ()
compile loggingSettings options@CompileOptions {..} = runCompileMonad loggingSettings $ do
  (imports, prog) <-
    typeCheckUserProg $
      TypeCheckOptions
        { specification = specification,
          typingSystem = Standard
        }

  prunedProg <- analyseDependenciesAndPrune prog declarationsToCompile
  let result = (imports, prunedProg)

  let resources = Resources specification networkLocations datasetLocations parameterValues
  case target of
    VerifierQueries queryFormatID ->
      compileToQueryFormat result resources queryFormatID output
    LossFunction differentiableLogic ->
      compileToLossFunction result differentiableLogic output outputAsJSON
    ITP Agda ->
      compileToAgda options result
    ExplicitVehicle ->
      compileDirect result output outputAsJSON

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadStdIO m) =>
  (Imports, Prog Ix Builtin) ->
  Resources ->
  QueryFormatID ->
  Maybe FilePath ->
  m ()
compileToQueryFormat (imports, typedProg) resources queryFormatID output = do
  let mergedProg = mergeImports imports typedProg
  let verifier = queryFormats queryFormatID
  compileToQueries verifier mergedProg resources output

compileToAgda ::
  (MonadCompile m, MonadStdIO m) =>
  CompileOptions ->
  (Imports, Prog Ix Builtin) ->
  m ()
compileToAgda CompileOptions {..} (_, typedProg) = do
  let agdaOptions = AgdaOptions verificationCache output moduleName
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile output agdaCode

compileToLossFunction ::
  (MonadCompile m, MonadStdIO m) =>
  (Imports, Prog Ix Builtin) ->
  DifferentiableLogicID ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction (imports, typedProg) differentiableLogic output outputAsJSON = do
  let mergedProg = mergeImports imports typedProg
  resolvedProg <- resolveInstanceArguments mergedProg
  lossProg <- LossFunction.compile differentiableLogic resolvedProg
  compileToTensors lossProg output outputAsJSON

compileDirect ::
  (MonadCompile m, MonadStdIO m) =>
  (Imports, Prog Ix Builtin) ->
  Maybe FilePath ->
  Bool ->
  m ()
compileDirect (imports, typedProg) outputFile outputAsJSON = do
  let mergedProg = mergeImports imports typedProg
  resolvedProg <- resolveInstanceArguments mergedProg
  compileToTensors resolvedProg outputFile outputAsJSON

compileToTensors ::
  forall builtin m.
  (MonadCompile m, MonadStdIO m, HasStandardData builtin, TypableBuiltin builtin, Hashable builtin, ToJBuiltin builtin) =>
  Prog Ix builtin ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToTensors prog outputFile outputAsJSON = do
  relevantProg <- removeIrrelevantCodeFromProg prog
  let monomorphiseIf = isPropertyDecl
  monomorphiseProg <- monomorphise monomorphiseIf "_" relevantProg
  literalFreeProg <- removeLiteralCoercions "_" monomorphiseProg
  cleanedProg <- cleanUpHigherOrderStuff literalFreeProg

  hoistedProg <- hoistInferableParameters cleanedProg
  functionalisedProg <- functionaliseResources hoistedProg
  etaExpandedProg <- etaExpandProg functionalisedProg
  result <-
    if outputAsJSON
      then do
        compileProgToJSON etaExpandedProg
      else do
        return $ prettyFriendly etaExpandedProg
  writeResultToFile Nothing outputFile result
