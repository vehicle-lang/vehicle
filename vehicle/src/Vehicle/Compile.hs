module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (ToJSON (..))
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (convertToLossTensors)
import Vehicle.Backend.LossFunction.JSON
import Vehicle.Backend.LossFunction.LogicCompilation (compileLogic)
import Vehicle.Backend.LossFunction.Logics (dslFor)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Rocq
import Vehicle.Backend.Solver
import Vehicle.Compile.Dependency
import Vehicle.Compile.Error
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Monomorphisation (MonomorphisationSettings (..), hoistInferableParameters, monomorphise)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Data.Builtin.Decidability.Type ()
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
    verificationCache :: Maybe FilePath
  }
  deriving (Eq, Show)

compile :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> CompileOptions -> IO ()
compile loggingSettings outputAsJSON options@CompileOptions {..} =
  runCompileMonad loggingSettings outputAsJSON $ do
    (imports, prog) <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specification,
            secondaryTypeSystem = Nothing
          }

    checkDeclarationNamesPresent prog declarationsToCompile
    simplifiedProg <- simplifyProgram prog declarationsToCompile

    case target of
      VerifierQueries queryFormatID -> do
        let resources = Resources specification networkLocations datasetLocations parameterValues
        let mergedProg = mergeImports imports simplifiedProg
        compileToQueryFormat mergedProg resources queryFormatID output
      LossFunction differentiableLogic -> do
        let mergedProg = mergeImports imports simplifiedProg
        compileToLossFunction differentiableLogic mergedProg output outputAsJSON
      ITP itp ->
        compileToITP itp options simplifiedProg

simplifyProgram ::
  (MonadCompile m) =>
  Prog Builtin ->
  DeclarationNames ->
  m (Prog Builtin)
simplifyProgram prog declarationsToCompile = do
  let keepUnusedDeclaration =
        if null declarationsToCompile
          then const True
          else do
            let declsToCompile = Set.fromList declarationsToCompile
            \ident -> Set.member (nameOf ident) declsToCompile
  monomorphisedProg <-
    monomorphise prog $
      MonoSettings
        { isMonomorphisableBinder = not . isExplicit,
          keepUnusedDeclaration = keepUnusedDeclaration
        }
  castFreeProgram <- resolveInstanceArgumentsAndCasts monomorphisedProg
  return castFreeProgram

checkDeclarationNamesPresent :: (MonadCompile m) => Prog Builtin -> DeclarationNames -> m ()
checkDeclarationNamesPresent (Main decls) requestedDeclNames = do
  let actualDeclNames = Set.fromList $ fmap nameOf decls
  let missingNames = Set.toList $ Set.fromList requestedDeclNames `Set.difference` actualDeclNames
  case missingNames of
    [] -> return ()
    n : ns ->
      throwError $
        MissingRequestedDeclarations (n :| ns)

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadStdIO m) =>
  Prog Builtin ->
  Resources ->
  QueryFormatID ->
  Maybe FilePath ->
  m ()
compileToQueryFormat typedProg resources queryFormatID output = do
  logCompilerPass QueryBackend $ do
    let verifier = queryFormats queryFormatID
    compileToQueries verifier typedProg resources output

compileToITP ::
  (MonadCompile m, MonadStdIO m) =>
  ITP ->
  CompileOptions ->
  Prog Builtin ->
  m ()
compileToITP itp CompileOptions {..} typedProg@(Main ds) =
  logCompilerPass ITPBackend $ do
    -- Prune all standard-library declarations that aren't used.
    let declsToCompile = mapMaybe (\d -> if isUserCode d then Just (nameOf d) else Nothing) ds
    let dependencyGraph = createDependencyGraph typedProg
    prunedProg <- pruneUnusedDeclarations typedProg dependencyGraph declsToCompile

    -- Analyse the program to find out which `Bool`s are decidable and which aren't.
    decProg <- decidabilityTypeCheck prunedProg

    -- Compile depending on the ITP
    case itp of
      Agda -> do
        let agdaOptions = AgdaOptions verificationCache output moduleName
        agdaCode <- compileProgToAgda decProg agdaOptions
        writeAgdaFile output agdaCode
      Rocq -> do
        let rocqOptions = RocqOptions output moduleName
        rocqCode <- compileProgToRocq decProg rocqOptions
        writeRocqFile output rocqCode

compileToLossFunction ::
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  DifferentiableLogicID ->
  Prog Builtin ->
  Maybe FilePath ->
  Bool ->
  m ()
compileToLossFunction logicID typedProg outputFile outputAsJSON =
  logCompilerPass LossBackend $ do
    hoistedProg <- hoistInferableParameters typedProg
    functionalisedProg <- functionaliseResources hoistedProg
    compiledLogic <- compileLogic logicID (dslFor logicID)
    lossTensorProg <- convertToLossTensors compiledLogic functionalisedProg
    jsonProg <- convertToJSONProg lossTensorProg
    let outputText
          | outputAsJSON = pretty $ unpack $ encodePretty' prettyJSONConfig $ toJSON jsonProg
          | otherwise = prettyFriendly (convertFromJSONProg jsonProg)
    writeResultToFile Nothing outputFile outputText
