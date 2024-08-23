module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (convertToLossTensors)
import Vehicle.Backend.LossFunction.JSON
import Vehicle.Backend.LossFunction.LogicCompilation (compileLogic)
import Vehicle.Backend.LossFunction.Logics (dslFor)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Queries
import Vehicle.Compile.Dependency (analyseDependenciesAndPrune)
import Vehicle.Compile.Error
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Monomorphisation (hoistInferableParameters)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Builtin.Standard
import Vehicle.Prelude.Logging
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
      compileToLossFunction differentiableLogic result output outputAsJSON
    ITP Agda ->
      compileToAgda options result

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadStdIO m) =>
  (Imports, Prog Builtin) ->
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
  (Imports, Prog Builtin) ->
  m ()
compileToAgda CompileOptions {..} (_, typedProg) = do
  let agdaOptions = AgdaOptions verificationCache output moduleName
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile output agdaCode

compileToLossFunction ::
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  DifferentiableLogicID ->
  (Imports, Prog Builtin) ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction logicID (imports, typedProg) outputFile outputAsJSON = do
  let mergedProg = mergeImports imports typedProg
  hoistedProg <- hoistInferableParameters mergedProg
  functionalisedProg <- functionaliseResources hoistedProg
  let logic = dslFor logicID
  compiledLogic <- compileLogic logicID logic
  lossTensorProg <- convertToLossTensors compiledLogic functionalisedProg
  jsonProg <- convertToJSONProg lossTensorProg
  let outputText
        | outputAsJSON = pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON jsonProg
        | otherwise = prettyFriendly (convertFromJSONProg jsonProg)
  writeResultToFile Nothing outputFile outputText
