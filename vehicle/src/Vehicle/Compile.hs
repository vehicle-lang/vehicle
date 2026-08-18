module Vehicle.Compile
  ( CompileOptions (..),
    LossOptions (..),
    QueryOptions (..),
    ITPOptions (..),
    compile,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..))
import Data.Set qualified as Set
import System.Directory (makeAbsolute)
import Vehicle.Backend.ITP.Agda
import Vehicle.Backend.ITP.Imandra
import Vehicle.Backend.ITP.Isabelle
import Vehicle.Backend.ITP.Rocq
import Vehicle.Backend.Loss.JSON
import Vehicle.Backend.LossSearch (SearchDecl (..), SearchProg (..), convertToSearchLoss)
import Vehicle.Backend.LossTraining (convertToLossTensors)
import Vehicle.Backend.Prelude
import Vehicle.Backend.Solver
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.FunctionaliseResources (functionaliseResources)
import Vehicle.Compile.Prelude as CompilePrelude hiding (programDeclarations)
import Vehicle.Compile.Print (prettyFriendly, prettyFriendlyEmptyCtx)
import Vehicle.Compile.Type.Subsystem
import Vehicle.Data.Builtin.Decidability.Type ()
import Vehicle.Data.Builtin.Interface.Print (ConvertableBuiltin (..), PrintableBuiltin)
import Vehicle.Data.Builtin.Standard
import Vehicle.Prelude.Logging
import Vehicle.TypeCheck (TypeCheckOptions (..), runCompileMonad, typeCheckUserProg)
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification (traverseProperty)

--------------------------------------------------------------------------------
-- Interface

data CompileOptions
  = ITPTarget ITPOptions
  | QueryTarget QueryOptions
  | LossTarget LossOptions
  deriving (Show, Eq)

data LossOptions = LossOptions
  { lossFunctionMode :: LossFunctionMode,
    differentiableLogicID :: DifferentiableLogicID,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    outputFile :: Maybe FilePath
  }
  deriving (Show, Eq)

data QueryOptions = QueryOptions
  { queryFormatID :: QueryFormatID,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    outputFolder :: Maybe FilePath,
    verificationCache :: Maybe FilePath
  }
  deriving (Show, Eq)

data ITPOptions = ITPOptions
  { itp :: InteractiveTheoremProverID,
    specification :: FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String,
    verificationCache :: Maybe FilePath,
    constructiveReals :: Bool
  }
  deriving (Show, Eq)

specificationOf :: CompileOptions -> FilePath
specificationOf = \case
  LossTarget LossOptions {..} -> specification
  QueryTarget QueryOptions {..} -> specification
  ITPTarget ITPOptions {..} -> specification

declarationsOf :: CompileOptions -> DeclarationNames
declarationsOf = \case
  LossTarget LossOptions {..} -> declarationsToCompile
  QueryTarget QueryOptions {..} -> declarationsToCompile
  ITPTarget ITPOptions {..} -> declarationsToCompile

compile :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> CompileOptions -> IO ()
compile loggingSettings outputAsJSON options =
  runCompileMonad loggingSettings outputAsJSON $ do
    prog <-
      typeCheckUserProg $
        TypeCheckOptions
          { specification = specificationOf options,
            secondaryTypeSystem = Nothing,
            declarationsToCompile = declarationsOf options
          }

    case options of
      LossTarget lossOptions -> compileToLossFunction lossOptions prog outputAsJSON
      QueryTarget queryOptions -> compileToQueryFormat queryOptions prog
      ITPTarget itpOptions -> compileToITP itpOptions prog

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToQueryFormat ::
  (MonadCompile m, MonadStdIO m) =>
  QueryOptions ->
  Prog Builtin ->
  m ()
compileToQueryFormat QueryOptions {..} typedProg = do
  logCompilerPass Solver $ do
    let verifier = queryFormats queryFormatID
    let resources = Resources specification networkLocations datasetLocations parameterValues
    compileToQueries verifier typedProg resources outputFolder

compileToITP ::
  (MonadCompile m, MonadStdIO m) =>
  ITPOptions ->
  Prog Builtin ->
  m ()
compileToITP ITPOptions {..} typedProg = do
  resources <- mkExternalResources specification networkLocations datasetLocations parameterValues
  (expandedProg, _, _, _, _) <- expandResources resources typedProg
  -- Analyse the program to find out which `Bool`s are decidable and which aren't.
  decProg <- decidabilityTypeCheck expandedProg

  -- Make the cache path absolute so that `compile` can be invoked
  -- from any working directory.
  absCache <- liftIO $ traverse makeAbsolute verificationCache

  -- Compile depending on the ITP
  logCompilerPass ITP $
    case itp of
      Agda -> do
        let agdaOptions = AgdaOptions absCache outputFile moduleName
        agdaCode <- compileProgToAgda decProg agdaOptions
        writeAgdaFile outputFile agdaCode
      Rocq -> do
        let rocqOptions = RocqOptions absCache outputFile moduleName constructiveReals
        rocqCode <- compileProgToRocq decProg rocqOptions
        writeRocqFile outputFile rocqCode
      Isabelle -> do
        let isabelleOptions = IsabelleOptions outputFile moduleName
        isabelleCode <- compileProgToIsabelle decProg isabelleOptions
        writeIsabelleFile outputFile isabelleCode
      Imandra -> do
        let imandraOptions = ImandraOptions outputFile moduleName
        imandraCode <- compileProgToImandra decProg imandraOptions
        writeImandraFile outputFile imandraCode

compileToLossFunction ::
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  LossOptions ->
  Prog Builtin ->
  OutputAsJSON ->
  m ()
compileToLossFunction LossOptions {..} typedProg outputAsJSON =
  if lossFunctionMode == Training
    then compileToTrainingLoss differentiableLogicID outputFile declarationsToCompile typedProg outputAsJSON
    else compileToSearchLoss differentiableLogicID outputFile declarationsToCompile typedProg outputAsJSON

compileToTrainingLoss ::
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  DifferentiableLogicID ->
  Maybe FilePath ->
  DeclarationNames ->
  Prog Builtin ->
  OutputAsJSON ->
  m ()
compileToTrainingLoss differentiableLogicID outputFile declsToCompile typedProg outputAsJSON = do
  let requestedDecls = Set.fromList declsToCompile
  lossTensorProg <- convertToLossTensors differentiableLogicID requestedDecls typedProg
  hoistedProg <- hoistInferableParameters lossTensorProg
  functionalisedProg <- functionaliseResources hoistedProg
  builtinProg <- traverse (traverseBuiltinsM toStandardBuiltins) functionalisedProg
  jsonProg <- convertToJSONProg builtinProg
  let outputText
        | outputAsJSON = prettyAsJSON jsonProg
        | otherwise = prettyFriendly (convertFromJSONProg jsonProg)
  writeResultToFile Nothing outputFile outputText
  where
    toStandardBuiltins p b args =
      return $ normAppList (convertBuiltin p b :: Expr Builtin) args

compileToSearchLoss ::
  forall m.
  (MonadCompile m, MonadStdIO m) =>
  DifferentiableLogicID ->
  Maybe FilePath ->
  DeclarationNames ->
  Prog Builtin ->
  OutputAsJSON ->
  m ()
compileToSearchLoss differentiableLogicID outputFile declsToCompile typedProg outputAsJSON = do
  let requestedDecls = Set.fromList declsToCompile
  lossSearchProg <- convertToSearchLoss differentiableLogicID requestedDecls typedProg
  builtinProg <- SearchMain <$> traverse convertToBuiltin (programDeclarations lossSearchProg)
  jsonSearchProg <- convertToJSONSearchProg builtinProg
  let outputText
        | outputAsJSON = prettyAsJSON jsonSearchProg
        | otherwise = prettyFriendly (convertFromJSONSearchProg jsonSearchProg)
  writeResultToFile Nothing outputFile outputText
  where
    toStandardBuiltins p b args =
      return $ normAppList (convertBuiltin p b :: Expr Builtin) args

    convertToBuiltin decl = case decl of
      StandardDecl d -> StandardDecl <$> traverse (traverseBuiltinsM toStandardBuiltins) d -- I think this is redundant
      PropertyDecl prov ident prop -> do
        prop' <- traverseProperty (traverseBuiltinsM toStandardBuiltins) prop
        return $ PropertyDecl prov ident prop'

hoistInferableParameters ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  Prog builtin ->
  m (Prog builtin)
hoistInferableParameters (Main ds) =
  logCompilerSection2 MinDetail "hoisting inferable parameters" $ do
    (otherDecls, inferableParameters) <- runWriterT (goDecls ds)
    logDebug MaxDetail $ "Hoisted parameters:" <> lineIndent (vsep $ fmap prettyFriendly inferableParameters)
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

mkExternalResources ::
  (MonadIO m) =>
  FilePath ->
  NetworkLocations ->
  DatasetLocations ->
  ParameterValues ->
  m Resources
mkExternalResources specification networkLocations datasetLocations parameterValues = do
  absSpecificationLocation <- liftIO $ makeAbsolute specification
  absNetworkLocations <- liftIO $ traverse makeAbsolute networkLocations
  absDatasetLocations <- liftIO $ traverse makeAbsolute datasetLocations
  return $ Resources absSpecificationLocation absNetworkLocations absDatasetLocations parameterValues
