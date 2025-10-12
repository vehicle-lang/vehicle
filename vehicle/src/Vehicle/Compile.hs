module Vehicle.Compile
  ( CompileOptions (..),
    compile,
  )
where

import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction (convertToLossTensors)
import Vehicle.Backend.LossFunction.JSON
import Vehicle.Backend.LossFunction.LogicCompilation (compileLogic)
import Vehicle.Backend.LossFunction.Logics (dslFor)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Rocq
import Vehicle.Backend.Solver
import Vehicle.Compile.Error
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
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
    prog <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specification,
            secondaryTypeSystem = Nothing,
            declarationsToCompile = declarationsToCompile
          }

    case target of
      VerifierQueries queryFormat -> compileToQueryFormat options queryFormat prog
      LossFunction logic -> compileToLossFunction logic prog output outputAsJSON
      ITP itp -> compileToITP itp options prog

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadStdIO m) =>
  CompileOptions ->
  QueryFormatID ->
  Prog Builtin ->
  m ()
compileToQueryFormat CompileOptions {..} queryFormatID typedProg = do
  logCompilerPass QueryBackend $ do
    let verifier = queryFormats queryFormatID
    let resources = Resources specification networkLocations datasetLocations parameterValues
    compileToQueries verifier typedProg resources output

compileToITP ::
  (MonadCompile m, MonadStdIO m) =>
  ITP ->
  CompileOptions ->
  Prog Builtin ->
  m ()
compileToITP itp CompileOptions {..} typedProg =
  logCompilerPass ITPBackend $ do
    -- Analyse the program to find out which `Bool`s are decidable and which aren't.
    decProg <- decidabilityTypeCheck typedProg

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
          | outputAsJSON = prettyAsJSON jsonProg
          | otherwise = prettyFriendly (convertFromJSONProg jsonProg)
    writeResultToFile Nothing outputFile outputText

hoistInferableParameters :: (MonadCompile m) => Prog builtin -> m (Prog builtin)
hoistInferableParameters (Main ds) = do
  (otherDecls, inferableParameters) <- runWriterT (goDecls ds)
  return $ Main (inferableParameters <> otherDecls)
  where
    goDecls :: (MonadWriter [Decl builtin] m) => [Decl builtin] -> m [Decl builtin]
    goDecls [] = return []
    goDecls (decl : decls) = do
      decls' <- goDecls decls
      case decl of
        DefAbstract _ _ (ParameterDef Inferable) _ -> do
          tell [decl]
          return decls'
        _ -> return $ decl : decls'
