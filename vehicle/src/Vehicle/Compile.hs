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
import Vehicle.Compile.Monomorphisation (monomorphise)
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Queries
import Vehicle.Compile.Queries.LinearityAndPolarityErrors (resolveInstanceArguments)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
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
    proofCache :: Maybe FilePath
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
      compileToLossFunction result differentiableLogic outputFile
    ITP Agda -> do
      let agdaOptions = AgdaOptions proofCache outputFile moduleName
      compileToAgda agdaOptions result outputFile
    JSON -> do
      compileToJSON result outputFile

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

mergeImports :: ImportedModules -> StandardGluedProg -> StandardGluedProg
mergeImports imports userProg = Main $ concatMap (\(Main ds) -> ds) (imports <> [userProg])

compileToLossFunction ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  DifferentiableLogicID ->
  Maybe FilePath ->
  m ()
compileToLossFunction (imports, typedProg) differentiableLogic outputFile = do
  let mergedProg = mergeImports imports typedProg
  result <- LossFunction.compile differentiableLogic mergedProg
  writeResultToFile Nothing outputFile result

compileToAgda ::
  (MonadCompile m, MonadIO m) =>
  AgdaOptions ->
  (ImportedModules, StandardGluedProg) ->
  Maybe FilePath ->
  m ()
compileToAgda agdaOptions (_, typedProg) outputFile = do
  agdaCode <- compileProgToAgda typedProg agdaOptions
  writeAgdaFile outputFile agdaCode

compileToJSON ::
  (MonadCompile m, MonadIO m) =>
  (ImportedModules, StandardGluedProg) ->
  Maybe FilePath ->
  m ()
compileToJSON (imports, typedProg) outputFile = do
  let mergedProg = mergeImports imports typedProg
  let unnormalisedProg = fmap unnormalised mergedProg
  resolvedProg <- resolveInstanceArguments unnormalisedProg
  monomorphiseProg <- monomorphise False resolvedProg
  literalCoercionFreeProg <- removeLiteralCoercions monomorphiseProg
  let namedProg = descopeNamed literalCoercionFreeProg
  result <- compileProgToJSON namedProg
  writeResultToFile Nothing outputFile result
  where
    removeLiteralCoercions :: forall m. (MonadCompile m) => StandardProg -> m StandardProg
    removeLiteralCoercions = traverse (traverseBuiltinsM update)
      where
        update p1 p2 b args = case b of
          (CFunction (FromNat dom)) -> case (dom, args) of
            (FromNatToIndex, [_, ExplicitArg _ (NatLiteral p n), _]) -> return $ IndexLiteral p n
            (FromNatToNat, [e, _]) -> return $ argExpr e
            (FromNatToInt, [ExplicitArg _ (NatLiteral p n), _]) -> return $ IntLiteral p n
            (FromNatToRat, [ExplicitArg _ (NatLiteral p n), _]) -> return $ RatLiteral p (fromIntegral n)
            _ -> compilerDeveloperError $ "Found partially applied `FromNat`:" <+> pretty b <+> pretty (show args)
          (CFunction (FromRat dom)) -> case (dom, args) of
            (FromRatToRat, [e]) -> return $ argExpr e
            _ -> compilerDeveloperError $ "Found partially applied `FromRat`:" <+> pretty b <+> prettyVerbose args
          _ -> return $ normAppList p1 (Builtin p2 b) args
