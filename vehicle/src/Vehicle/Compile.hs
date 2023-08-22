module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Hashable (Hashable)
import Data.Map qualified as Map
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
import Vehicle.Expr.BuiltinInterface
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
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String,
    verificationCache :: Maybe FilePath,
    outputAsJSON :: Bool
  }
  deriving (Eq, Show)

compile :: LoggingSettings -> CompileOptions -> IO ()
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
      compileToQueryFormat result resources queryFormatID outputFile
    LossFunction differentiableLogic ->
      compileToLossFunction result differentiableLogic outputFile outputAsJSON
    ITP Agda ->
      compileToAgda options result
    ExplicitVehicle ->
      compileDirect result outputFile outputAsJSON

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadIO m) =>
  (Imports, Prog Ix Builtin) ->
  Resources ->
  QueryFormatID ->
  Maybe FilePath ->
  m ()
compileToQueryFormat (imports, typedProg) resources queryFormatID outputFile = do
  let mergedProg = mergeImports imports typedProg
  let verifier = queryFormats queryFormatID
  compileToQueries verifier mergedProg resources outputFile

compileToAgda ::
  (MonadCompile m, MonadIO m) =>
  CompileOptions ->
  (Imports, Prog Ix Builtin) ->
  m ()
compileToAgda options@CompileOptions {..} (_, typedProg) = do
  warnIfResourcesProvidedToITP Agda options
  let agdaOptions = AgdaOptions verificationCache outputFile moduleName
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile outputFile agdaCode

compileToLossFunction ::
  (MonadCompile m, MonadIO m) =>
  (Imports, Prog Ix Builtin) ->
  DifferentiableLogicID ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction (imports, typedProg) differentiableLogic outputFile outputAsJSON = do
  let mergedProg = mergeImports imports typedProg
  resolvedProg <- resolveInstanceArguments mergedProg
  lossProg <- LossFunction.compile differentiableLogic resolvedProg
  compileToTensors lossProg outputFile outputAsJSON

compileDirect ::
  (MonadCompile m, MonadIO m) =>
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
  (MonadCompile m, MonadIO m, HasStandardData builtin, TypableBuiltin builtin, Hashable builtin, ToJBuiltin builtin) =>
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

warnIfResourcesProvided :: (MonadCompile m) => Target -> Doc a -> CompileOptions -> m ()
warnIfResourcesProvided itp src CompileOptions {..} = do
  let parameters = fmap (Parameter,) (Map.keys parameterValues)
  let datasets = fmap (Dataset,) (Map.keys datasetLocations)
  let networks = fmap (Network,) (Map.keys networkLocations)
  let resources = parameters <> datasets <> networks
  let resourceDocs = vsep (fmap (\(r, n) -> pretty r <+> pretty n) resources)
  unless (null resources) $ do
    logWarning $
      "The following provided resources:"
        <> line
        <> line
        <> indent 2 resourceDocs
        <> line
        <> line
        <> "will be ignored as when compiling to" <+> pretty itp <+> src

warnIfResourcesProvidedToITP :: (MonadCompile m) => ITP -> CompileOptions -> m ()
warnIfResourcesProvidedToITP itp =
  warnIfResourcesProvided
    (ITP itp)
    "their values will be taken directly from the verification cache."
