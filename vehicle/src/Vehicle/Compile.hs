module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction.Convert
import Vehicle.Backend.LossFunction.JSON (compileProgToJSON)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Queries
import Vehicle.Compile.Dependency (analyseDependenciesAndPrune)
import Vehicle.Compile.Error
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Monomorphisation (hoistInferableParameters)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem.Standard
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
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  DifferentiableLogicID ->
  (Imports, Prog Ix Builtin) ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction differentiableLogicID (imports, typedProg) outputFile outputAsJSON = do
  let mergedProg = mergeImports imports typedProg
  -- Deciding on the best ordering of converting to loss functions and converting to tensor code
  -- is tricky. There are three approaches:
  --
  -- Loss functions -> Tensors
  --    Disadvantages
  --        - Vector representation contains higher order structure (e.g. folds) that
  --          can be converted to tensor operations, but which `not` cannot easily
  --          be pushed through in general for DL2 loss. e.g. in
  --          fold (\ x -> \ y -> x and y) True (foreachIndex 2 (\ i -> - 3.25 <= x ! i and x ! i <= 3.25))
  --
  -- Tensors -> Loss functions
  --    Disadvantages
  --        - Loss functions need to be specified in terms of tensors.
  --
  tensorProg <- convertToTensors differentiableLogicID mergedProg
  hoistedProg <- hoistInferableParameters tensorProg
  functionalisedProg <- functionaliseResources hoistedProg
  result <-
    if outputAsJSON
      then do
        compileProgToJSON functionalisedProg
      else do
        return $ prettyFriendly functionalisedProg
  writeResultToFile Nothing outputFile result
