module Vehicle.Verify.Specification.Execute
  ( VerificationSettings (..),
    verifySpecification,
  )
where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (MonadError (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.IDX (encodeIDXFile)
import Data.IDX.Internal
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Set qualified as Set (difference, fromList, null)
import Data.Vector qualified as BoxedVector
import Data.Vector.Unboxed qualified as Vector (fromList)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import System.Random
import Vehicle.Backend.Solver.UserVariableElimination.VariableReconstruction (reconstructUserVars)
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor as Tensor (HasShape (..), toVector)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Execute.Reporting
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

type MonadVerify m =
  ( MonadLogger m,
    MonadStdIO m,
    MonadProgressReporter m,
    MonadReader VerificationSettings m
  )

--------------------------------------------------------------------------------
-- Verification of properties

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  (MonadLogger m, MonadStdIO m) =>
  Bool ->
  VerificationSettings ->
  m ()
verifySpecification outputAsJSON verifierSettings
  | outputAsJSON = runReaderT (runJSONProgressReporterT verifySpecificationActual) verifierSettings
  | otherwise = runReaderT (runTextProgressReporterT verifySpecificationActual) verifierSettings

verifySpecificationActual :: (MonadVerify m) => m ()
verifySpecificationActual = logCompilerPass Verification $ do
  settings <- ask
  let verificationPlanFile = specificationCacheIndexFileName (specificationCache settings)
  SpecificationCacheIndex {..} <- readSpecificationCacheIndex verificationPlanFile

  maybeIntegrityError <- checkIntegrityOfResources resourcesIntegrityInfo
  case maybeIntegrityError of
    Just err -> writeStderrLn $ layoutAsText $ "Resource error:" <+> pretty err
    Nothing -> do
      forM_ properties $ \(name, multiProperty) ->
        reportMultiProperty name $ verifyMultiproperty multiProperty

verifyMultiproperty ::
  (MonadVerify m) =>
  MultiProperty PropertyAddress ->
  m ()
verifyMultiproperty = \case
  MultiProperty properties -> forM_ properties verifyMultiproperty
  SingleProperty address -> verifyProperty address

verifyProperty ::
  (MonadVerify m) =>
  PropertyAddress ->
  m ()
verifyProperty address = do
  -- Read the verification plan for the property
  settings <- ask
  let propertyPlanFile = propertyPlanFileName (specificationCache settings) address
  PropertyVerificationPlan {..} <- readPropertyVerificationPlan propertyPlanFile

  -- Determine number of queries and initialise progress bar
  result <- reportProperty address (propertySize queryMetaData) $ case queryMetaData of
    Trivial status ->
      return $ Trivial status
    NonTrivial structure -> logCompilerSection MinDetail ("Verifying property" <+> quotePretty address) $ do
      -- Verify all queries in reader with full context
      NonTrivial <$> verifyPropertyBooleanStructure structure

  outputPropertyResult address result

type MonadVerifyProperty m =
  ( MonadVerify m,
    MonadError (QueryMetaData, VerifierError) m
  )

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyPropertyBooleanStructure ::
  forall m.
  (MonadVerifyProperty m) =>
  BooleanExpr QuerySet ->
  m (BooleanExpr QuerySetResult)
verifyPropertyBooleanStructure expr = fst <$> go expr
  where
    go ::
      BooleanExpr QuerySet ->
      m (BooleanExpr QuerySetResult, Either VerifierError Bool)
    go = \case
      Atom qs -> do
        (result, boolResult) <- verifyQuerySet qs
        return (Atom result, boolResult)
      Disjunct (DisjunctAll xs) -> do
        (result, boolResult) <- goDisjunct xs
        return (disjunctExprs $ DisjunctAll result, boolResult)
      Conjunct (ConjunctAll xs) -> do
        (result, boolResult) <- goConjunct xs
        return (conjunctExprs $ ConjunctAll result, boolResult)

    goConjunct :: NonEmpty (BooleanExpr QuerySet) -> m (NonEmpty (BooleanExpr QuerySetResult), Either VerifierError Bool)
    goConjunct (x :| []) = first (:| []) <$> go x
    goConjunct (x :| y : ys) = do
      (result, boolResult) <- go x
      case boolResult of
        Right True -> first (result <|) <$> goConjunct (y :| ys)
        errorOrFalse -> return (result :| [], errorOrFalse)

    goDisjunct :: NonEmpty (BooleanExpr QuerySet) -> m (NonEmpty (BooleanExpr QuerySetResult), Either VerifierError Bool)
    goDisjunct (x :| []) = first (:| []) <$> go x
    goDisjunct (x :| y : ys) = do
      (result, boolResult) <- go x
      case boolResult of
        Right False -> first (result <|) <$> goDisjunct (y :| ys)
        errorOrTrue -> return (result :| [], errorOrTrue)

verifyQuerySet ::
  (MonadVerifyProperty m) =>
  QuerySet ->
  m (QuerySetResult, Either VerifierError Bool)
verifyQuerySet (QuerySet negated disjuncts) = do
  result <- verifyDisjunctAll disjuncts
  return (_ negated result)

verifyDisjunctAll ::
  forall m.
  (MonadVerifyProperty m) =>
  DisjunctAll QueryMetaData ->
  m QueryResult
verifyDisjunctAll (DisjunctAll ys) = go ys
  where
    go ::
      NonEmpty QueryMetaData ->
      m QueryResult
    go (x :| []) = verifyQuery x
    go (x :| y : xs) = do
      r <- verifyQuery x
      if isVerified r
        then return r
        else go (y :| xs)

--------------------------------------------------------------------------------
-- Verification of queries

type MonadVerifyQuery m =
  ( MonadLogger m,
    MonadStdIO m,
    MonadReader VerificationSettings m,
    MonadError VerifierError m
  )

verifyQuery ::
  (MonadVerifyProperty m) =>
  QueryMetaData ->
  m QueryResult
verifyQuery queryMetaData@(QueryMetaData queryAddress metaNetwork variables reconstruction) = logCompilerSection MidDetail ("Verifying query" <+> quotePretty queryAddress) $ do
  verifierSettings <- ask
  let queryFile = calculateQueryFileName (specificationCache verifierSettings) queryAddress

  errorOrResult <- runExceptT $
    reportQuery queryAddress $ do
      result <- invokeVerifier verifierSettings metaNetwork queryFile
      case result of
        QueryUnSAT -> do
          logDebug MidDetail $ "Query is UnSAT" <> line
          return QueryUnSAT
        QuerySAT maybeWitness -> case maybeWitness of
          Nothing -> do
            logDebug MidDetail $ "Query is SAT (no witness)" <> line
            return $ QuerySAT Nothing
          Just witness -> do
            logDebug MidDetail $ "Query is SAT (witness provided)" <> line
            checkWitness (getQueryVariables variables) witness
            return $ QuerySAT $ Just witness
        QueryErrored err -> _

  case errorOrResult of
    Left err -> return $ QueryErrored err
    Right result -> traverse (reconstructUserVars variables reconstruction) result

invokeVerifier ::
  (MonadVerifyQuery m) =>
  VerificationSettings ->
  MetaNetwork ->
  QueryFile ->
  m QueryResult
invokeVerifier VerificationSettings {..} metaNetworkEntries queryFile = do
  -- Prepare the command
  let args = prepareArgs verifier metaNetworkEntries queryFile <> verifierExtraArgs
  let command = unwords (verifierExecutable : args)

  -- Run the verification command
  logDebug MidDetail $ "Running verification command: " <> lineIndent (pretty command) <> line
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode verifierExecutable args ""
  logDebug MinDetail $ "Command status:" <+> pretty (show exitCode) <> line
  logDebug MinDetail $ "Command stdout:" <> lineIndent (pretty out) <> line
  logDebug MinDetail $ "Command stderr:" <> lineIndent (pretty err) <> line

  -- Check for errors
  case exitCode of
    ExitFailure exitValue
      -- Killed by the system.
      -- See System.Process.html#waitForProcess documentation
      | exitValue < 0 -> throwError $ VerifierTerminatedByOS (-exitValue)
      | otherwise -> throwError $ VerifierError (if null err then out else err)
    -- Parse the result
    _ -> parseOutput verifier out

checkWitness :: (MonadVerifyQuery m) => [QueryVariable] -> QueryVariablesAssignment -> m ()
checkWitness queryVariables witness = do
  let allVariables = Set.fromList queryVariables
  let providedVariables = Set.fromList $ fmap fst witness
  let missingVariables = Set.difference allVariables providedVariables
  unless (Set.null missingVariables) $
    throwError $
      VerifierIncompleteWitness missingVariables

--------------------------------------------------------------------------------
-- Errors

createReproducer ::
  (MonadVerify m) =>
  Verifier ->
  VerifierExecutable ->
  FilePath ->
  MetaNetwork ->
  QueryAddress ->
  m (Doc a)
createReproducer verifier verifierExecutable verificationCache metaNetwork queryAddress = do
  -- Create the reproducer directory
  vehiclePath <- getVehiclePath
  randomNumber <- liftIO (randomIO :: IO Int)
  let reproducerDir = vehiclePath </> "reproducers" </> show (abs randomNumber)
  liftIO $ createDirectoryIfMissing True reproducerDir

  -- Function to copy a file over
  let copyOverFile file = do
        let fileName = takeFileName file
        let resultName = reproducerDir </> fileName
        copyFile file resultName
        return resultName

  -- Copy the query file over
  let queryFile = calculateQueryFileName verificationCache queryAddress
  copiedQueryFile <- liftIO $ copyOverFile queryFile

  -- Copy the network files over
  copiedMetaNetwork <- liftIO $ forM metaNetwork $ \(name, NetworkContextInfo {..}, apps) -> do
    newNetworkFilePath <- copyOverFile networkFilepath
    return (name, NetworkContextInfo {networkFilepath = newNetworkFilePath, ..}, apps)

  let command = unwords (verifierExecutable : prepareArgs verifier copiedMetaNetwork copiedQueryFile)

  -- Return the explanatory text
  return $
    line
      <> "A reproducer has been created at:"
      <> line
      <> line
      <> indent 2 (pretty reproducerDir)
      <> line
      <> line
      <> "which can be run using:"
      <> line
      <> line
      <> indent 2 (pretty command)

--------------------------------------------------------------------------------
-- Assignments

outputPropertyResult ::
  (MonadVerify m) =>
  PropertyAddress ->
  PropertyResult ->
  m ()
outputPropertyResult address result = do
  VerificationSettings {..} <- ask

  -- Write the result to the cache
  writePropertyResult specificationCache address (isVerified result)

  -- Output any additional information
  _

{-
case result of
  PropertyCompleted status -> case status of
    NonTrivial (_, SAT (Just assignment)) -> writeWitnessToFile specificationCache address assignment
    _ -> return ()
  PropertyErrored (QueryMetaData {..}, err) -> do
    let VerificationErrorAction {..} = convertVerificationError verifier queryAddress err

    reproducerMessage <-
      if reproducerIsUseful
        then createReproducer verifier verifierExecutable specificationCache metaNetwork queryAddress
        else return ""

    unless (isTimeoutError err) $ do
      let finalMessage = "\nError: " <> verificationErrorMessage <> reproducerMessage
      writeStderrLn (layoutAsText finalMessage)
-}

writeWitnessToFile :: (MonadVerify m) => FilePath -> PropertyAddress -> UserVariablesAssignment -> m ()
writeWitnessToFile verificationCache address (UserVariablesAssignment assignments) = do
  let witnessFolder = verificationCache </> layoutAsString (pretty address) <> "-assignments"
  liftIO $ createDirectoryIfMissing True witnessFolder
  forM_ assignments $ \(var, tensor) -> do
    let file = witnessFolder </> layoutAsString (pretty var)
    let dims = Vector.fromList (shapeOf tensor)
    -- TODO got to be a better way to do this conversion...
    let unboxedVector = Vector.fromList $ BoxedVector.toList (fmap realToFrac (Tensor.toVector tensor))
    let idxData = IDXDoubles IDXDouble dims unboxedVector
    liftIO $ encodeIDXFile idxData file
