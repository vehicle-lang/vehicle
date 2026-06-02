{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use forM_" #-}
module Vehicle.Verify.Specification.Execute
  ( VerificationSettings (..),
    verifySpecification,
  )
where

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (MonadError (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.IDX (encodeIDXFile)
import Data.IDX.Internal
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set (difference, fromList, null)
import Data.Vector qualified as BoxedVector
import Data.Vector.Unboxed qualified as Vector (fromList)
import GHC.Base (NonEmpty (..))
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import System.Random
import Vehicle.Backend.Solver.UserVariableElimination.VariableReconstruction (reconstructUserVars)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core (Quantifier (..))
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor as Tensor (HasShape (..), toVector)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Execute.Reporting
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier
import Vehicle.Verify.Verifier.Core (SolverResult (..))

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
  SingleProperty address -> verifyPropertyAt address

verifyPropertyAt ::
  (MonadVerify m) =>
  PropertyAddress ->
  m ()
verifyPropertyAt address = do
  -- Read the verification plan for the property
  settings <- ask
  let propertyPlanFile = propertyPlanFileName (specificationCache settings) address
  PropertyVerificationPlan property <- readPropertyVerificationPlan propertyPlanFile
  result <-
    reportProperty address (propertySize property) $ do
      logCompilerSection MinDetail ("Verifying property" <+> quotePretty address) $ do
        runWriterT $ do
          verifyProperty property
  outputPropertyResult address result

type MonadVerifyProperty m =
  ( MonadVerify m,
    MonadWriter [UnknownQuery] m
  )

verifyProperty ::
  (MonadVerifyProperty m) =>
  Property ->
  m PropertyResult
verifyProperty = \case
  Trivial b -> return $ PropertyResult b
  NonTrivial e -> verifyPropertyBooleanStructure e

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyPropertyBooleanStructure ::
  forall m.
  (MonadVerifyProperty m) =>
  BooleanExpr QuerySet ->
  m PropertyResult
verifyPropertyBooleanStructure = go
  where
    go ::
      BooleanExpr QuerySet ->
      m PropertyResult
    go = \case
      Atom qs -> goAtom qs
      Disjunct (DisjunctAll xs) -> goDisjunct xs
      Conjunct (ConjunctAll xs) -> goConjunct xs

    goAtom :: QuerySet -> m PropertyResult
    goAtom querySet = do
      querySetResult <- verifyQuerySet querySet
      return $ case querySetResult of
        SATQuery polarity _ _ -> PropertyResult $ polarity == Exists
        NoSATQueries polarity -> PropertyResult $ polarity == Forall
        UnknownIfSATQuery -> PropertyUnknown

    goConjunct :: NonEmpty (BooleanExpr QuerySet) -> m PropertyResult
    goConjunct (x :| []) = go x
    goConjunct (x :| y : ys) = do
      result <- go x
      case result of
        PropertyResult False -> return $ PropertyResult False
        PropertyResult True -> goConjunct (y :| ys)
        PropertyUnknown -> do
          recResult <- goConjunct (y :| ys)
          case recResult of
            PropertyResult False -> return $ PropertyResult False
            _ -> return PropertyUnknown

    goDisjunct :: NonEmpty (BooleanExpr QuerySet) -> m PropertyResult
    goDisjunct (x :| []) = go x
    goDisjunct (x :| y : ys) = do
      result <- go x
      case result of
        PropertyResult True -> return $ PropertyResult True
        PropertyResult False -> goDisjunct (y :| ys)
        PropertyUnknown -> do
          recResult <- goDisjunct (y :| ys)
          case recResult of
            PropertyResult True -> return $ PropertyResult True
            _ -> return PropertyUnknown

verifyQuerySet ::
  (MonadVerifyProperty m) =>
  QuerySet ->
  m QuerySetResult
verifyQuerySet (QuerySet polarity disjuncts) = do
  verifyDisjunctAll polarity disjuncts

verifyDisjunctAll ::
  forall m.
  (MonadVerifyProperty m) =>
  QuerySetPolarity ->
  DisjunctAll QueryMetaData ->
  m QuerySetResult
verifyDisjunctAll polarity ys = go $ disjunctsToList ys
  where
    go ::
      [QueryMetaData] ->
      m QuerySetResult
    go [] = return $ NoSATQueries polarity
    go (x : xs) = do
      (queryResult, reconstructWitness) <- verifyQuery x
      case queryResult of
        QueryUnknown {} -> do
          furtherResult <- go xs
          case furtherResult of
            SATQuery {} -> return furtherResult
            _ -> return UnknownIfSATQuery
        QueryUnSAT -> go xs
        QuerySAT maybeQueryWitness -> do
          maybeUserWitness <- traverse reconstructWitness maybeQueryWitness
          writeWitnessToFile _ _ _
          return $ SATQuery polarity (queryAddress x) maybeUserWitness

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
  -- This should return a QueryResult when we get our story about
  -- compilation traces sorted out.
  m (QueryResult, QueryVariablesAssignment -> m UserVariablesAssignment)
verifyQuery metaData@(QueryMetaData queryAddress metaNetwork variables reconstruction) =
  logCompilerSection MidDetail ("Verifying query" <+> quotePretty queryAddress) $ do
    verifierSettings <- ask
    let queryFile = calculateQueryFileName (specificationCache verifierSettings) queryAddress

    errorOrResult <- runExceptT $
      reportQuery queryAddress $ do
        result <- invokeVerifier verifierSettings metaNetwork queryFile
        case result of
          TimedOut -> handleUnknownQuery metaData SolverTimedOut
          ReturnedUnknown -> handleUnknownQuery metaData SolverReportedUnknown
          ReturnedUnSAT -> return QueryUnSAT
          ReturnedSAT maybeWitness -> do
            case maybeWitness of
              Just witness -> checkWitness (getQueryVariables variables) witness
              Nothing -> return ()
            return $ QuerySAT maybeWitness

    finalResult <- case errorOrResult of
      Left err -> handleUnknownQuery metaData $ SolverErrored err
      Right result -> return result

    let reconstructWitness = reconstructUserVars variables reconstruction
    return (finalResult, reconstructWitness)

handleUnknownQuery :: (MonadVerifyProperty m) => QueryMetaData -> UnknownReason -> m QueryResult
handleUnknownQuery metaData reason = do
  tell [UnknownQuery metaData reason]
  return QueryUnknown

{-
    case result of
  NoSATQueries {} -> return ()
  SATQuery _ address maybeWitness -> case maybeWitness of
    Nothing -> return ()
    Just witness -> writeWitnessToFile specificationCache address witness
  ErroredQuery address err -> do
-}

invokeVerifier ::
  (MonadVerifyQuery m) =>
  VerificationSettings ->
  MetaNetwork ->
  QueryFile ->
  m SolverResult
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

handleQueryError ::
  (MonadVerify m) =>
  MetaNetwork ->
  QueryAddress ->
  VerifierError ->
  m ()
handleQueryError metaNetwork queryAddress err = do
  VerificationSettings {..} <- ask
  reproducerMessage <- createReproducer metaNetwork queryAddress
  let verificationErrorMessage = printVerifierError verifier queryAddress err
  let finalMessage = "\nError: " <> verificationErrorMessage <> reproducerMessage
  writeStderrLn (layoutAsText finalMessage)

createReproducer ::
  (MonadVerify m) =>
  MetaNetwork ->
  QueryAddress ->
  m (Doc ())
createReproducer metaNetwork queryAddress = do
  VerificationSettings {..} <- ask
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
  let queryFile = calculateQueryFileName specificationCache queryAddress
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
  writePropertyResult specificationCache address result

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
