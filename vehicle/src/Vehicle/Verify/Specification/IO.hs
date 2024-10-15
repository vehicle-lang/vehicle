module Vehicle.Verify.Specification.IO
  ( VerifierSettings (..),
    readSpecification,
    writeSpecificationCache,
    readSpecificationCacheIndex,
    writeVerificationQuery,
    writePropertyVerificationPlan,
    readPropertyResult,
    verifySpecification,
    specificationCacheIndexFileName,
    isValidQueryFolder,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (MonadError (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy qualified as BIO
import Data.IDX (encodeIDXFile)
import Data.IDX.Internal
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set (difference, fromList, null)
import Data.Text (intercalate, pack)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as LazyText
import Data.Vector qualified as BoxedVector
import Data.Vector.Unboxed qualified as Vector (fromList)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeExtension, takeFileName, (<.>), (</>))
import System.IO (stdout)
import System.Process (readProcessWithExitCode)
import System.ProgressBar
import System.Random
import Vehicle.Backend.Agda.Interact (writeResultToFile)
import Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction (reconstructUserVars)
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.QuantifiedVariable (UserVariableAssignment (..))
import Vehicle.Data.Tensor (Tensor (..))
import Vehicle.Prelude.IO qualified as VIO (MonadStdIO (writeStdoutLn))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier
import Vehicle.Verify.Verifier.Core (QueryVariableAssignment (..))

--------------------------------------------------------------------------------
-- Specification

readSpecification :: (MonadIO m) => FilePath -> m SpecificationText
readSpecification inputFile
  | takeExtension inputFile /= specificationFileExtension = do
      fatalError $
        "Specification"
          <+> quotePretty inputFile
          <+> "has unsupported"
          <+> "extension"
          <+> quotePretty (takeExtension inputFile)
          <> "."
            <+> "Only files with a"
            <+> quotePretty specificationFileExtension
            <+> "extension are supported."
  | otherwise =
      liftIO $
        TIO.readFile inputFile `catch` \(e :: IOException) -> do
          fatalError $
            "Error occured while reading specification"
              <+> quotePretty inputFile
              <> ":"
              <> line
              <> indent 2 (pretty (show e))

--------------------------------------------------------------------------------
-- Verification plan output

writeSpecificationCache ::
  (MonadIO m) =>
  FilePath ->
  SpecificationCacheIndex ->
  m ()
writeSpecificationCache folder plan = do
  let planText = encodePretty' prettyJSONConfig plan
  let planFile = specificationCacheIndexFileName folder

  liftIO $
    catch
      (do BIO.writeFile planFile planText)
      ( \(err :: IOException) ->
          fatalError $
            "Unable to write the verification plan to file"
              <+> quotePretty planFile
              <> line
              <> indent 2 ("error:" <+> pretty (show err))
      )

readSpecificationCacheIndex ::
  (MonadIO m) =>
  FilePath ->
  m SpecificationCacheIndex
readSpecificationCacheIndex cacheFile = do
  errorOrResult <-
    liftIO $
      catch
        (Right <$> BIO.readFile cacheFile)
        (\(e :: IOException) -> return $ Left e)

  case errorOrResult of
    Left err ->
      fatalError $
        "Unable to read the verification cache from file"
          <+> quotePretty cacheFile
          <> line
          <> indent 2 ("error:" <+> pretty (show err))
    Right result -> case decode result of
      Nothing ->
        fatalError $
          "Unabled to decode the verification cache from file"
            <+> quotePretty cacheFile
            <> "."
              <+> ""
      Just plan -> return plan

writePropertyVerificationPlan ::
  (MonadLogger m, MonadStdIO m) =>
  FilePath ->
  PropertyAddress ->
  PropertyVerificationPlan ->
  m ()
writePropertyVerificationPlan folder propertyAddress plan = do
  let planFile = propertyPlanFileName folder propertyAddress
  let planText = encodePretty' prettyJSONConfig plan

  logDebug MinDetail $ "Creating file:" <+> pretty planFile

  liftIO $
    catch
      (BIO.writeFile planFile planText)
      ( \(err :: IOException) ->
          fatalError $
            "Unable to write the verification plan to file"
              <+> quotePretty planFile
              <> line
              <> indent 2 ("error:" <+> pretty (show err))
      )

readPropertyVerificationPlan ::
  (MonadLogger m, MonadIO m) =>
  FilePath ->
  m PropertyVerificationPlan
readPropertyVerificationPlan planFile = do
  errorOrResult <-
    liftIO $
      catch
        (Right <$> BIO.readFile planFile)
        (\(e :: IOException) -> return $ Left e)

  case errorOrResult of
    Left err ->
      fatalError $
        "Unable to read the verification plan from file"
          <+> quotePretty planFile
          <> line
          <> indent 2 ("error:" <+> pretty (show err))
    Right result -> case decode result of
      Nothing ->
        fatalError $
          "Unabled to decode verification plan from file"
            <+> quotePretty planFile
            <> "."
              <+> ""
      Just plan -> return plan

writeVerificationQuery ::
  (MonadLogger m, MonadIO m, MonadStdIO m) =>
  QueryFormat ->
  FilePath ->
  (QueryMetaData, QueryText) ->
  m ()
writeVerificationQuery queryFormat folder (queryMetaData, queryText) = do
  let queryOutputForm = queryOutputFormat queryFormat
  let queryFilePath = folder </> calculateQueryFileName (queryAddress queryMetaData)
  writeResultToFile (Just queryOutputForm) (Just queryFilePath) (pretty queryText)

writePropertyResult ::
  (MonadIO m) =>
  FilePath ->
  PropertyAddress ->
  Bool ->
  m ()
writePropertyResult verificationCache address result = do
  let resultFile = propertyResultFileName verificationCache address
  liftIO $ writeFile resultFile (show result)

readPropertyResult ::
  (MonadIO m) =>
  FilePath ->
  PropertyAddress ->
  m Bool
readPropertyResult verificationCache address = do
  let resultFile = propertyResultFileName verificationCache address
  value <- liftIO $ readFile resultFile
  return $ read value

isValidQueryFolder :: (MonadIO m) => FilePath -> m Bool
isValidQueryFolder folder = liftIO $ doesFileExist (specificationCacheIndexFileName folder)

specificationCacheIndexFileName :: FilePath -> FilePath
specificationCacheIndexFileName folder =
  folder
    </> ""
      <.> specificationCacheIndexFileExtension

propertyPlanFileName :: FilePath -> PropertyAddress -> FilePath
propertyPlanFileName folder propertyAddress =
  folder
    </> layoutAsString (pretty propertyAddress)
      <.> propertyVerificationPlanFileExtension

propertyResultFileName :: FilePath -> PropertyAddress -> FilePath
propertyResultFileName folder propertyAddress =
  folder
    </> layoutAsString (pretty propertyAddress)
      <.> propertyVerificationResultFileExtension

--------------------------------------------------------------------------------
-- Verification

data VerifierSettings = VerifierSettings
  { verifier :: Verifier,
    verifierExecutable :: VerifierExecutable,
    verifierExtraArgs :: [String]
  }

type MonadVerify m =
  ( MonadLogger m,
    MonadIO m,
    MonadStdIO m
  )

type MonadVerifyProperty m =
  ( MonadVerify m,
    MonadReader (VerifierSettings, FilePath, PropertyProgressBar) m,
    MonadWriter (Sum Int) m
  )

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  (MonadVerify m) =>
  VerifierSettings ->
  FilePath ->
  m ()
verifySpecification verifierSettings queryFolder = do
  programOutput "Verifying properties:"
  let verificationPlanFile = specificationCacheIndexFileName queryFolder
  SpecificationCacheIndex {..} <- readSpecificationCacheIndex verificationPlanFile
  maybeIntegrityError <- checkIntegrityOfResources resourcesIntegrityInfo
  case maybeIntegrityError of
    Just err -> programOutput $ "Resource error:" <+> pretty err
    Nothing -> do
      let propertyAddresses = concatMap (multiPropertyAddresses . snd) properties
      forM_ propertyAddresses $
        verifyProperty verifierSettings queryFolder

verifyProperty ::
  forall m.
  (MonadVerify m) =>
  VerifierSettings ->
  FilePath ->
  PropertyAddress ->
  m ()
verifyProperty verifierSettings verificationCache address =
  logCompilerSection MinDetail ("Verifying property" <+> quotePretty address) $ do
    -- Read the verification plan for the property
    let propertyPlanFile = propertyPlanFileName verificationCache address
    PropertyVerificationPlan {..} <- readPropertyVerificationPlan propertyPlanFile

    -- Perform the verification
    let numberOfQueries = propertySize queryMetaData
    progressBar <- createPropertyProgressBar address numberOfQueries
    let readerState = (verifierSettings, verificationCache, progressBar)
    (result, Sum numberOfQueriesExecuted) <- runWriterT (runReaderT (verifyPropertyBooleanStructure queryMetaData) readerState)

    -- Tidy up and output results
    when (numberOfQueriesExecuted < numberOfQueries) $ do
      -- The progress bar is only closed when all queries are run so in this
      -- case we have to close it manually.
      closePropertyProgressBar progressBar
    outputPropertyResult verificationCache address result

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyPropertyBooleanStructure ::
  forall m.
  (MonadVerifyProperty m) =>
  Property QueryMetaData ->
  m PropertyStatus
verifyPropertyBooleanStructure = \case
  Trivial status -> return $ PropertyStatus (Trivial status)
  NonTrivial structure -> do
    (negationStatus, status) <- go structure
    return $ PropertyStatus $ NonTrivial (negationStatus, status)
  where
    go ::
      BooleanExpr (QuerySet QueryMetaData) ->
      m (QuerySetNegationStatus, QueryResult UserVariableAssignment)
    go = \case
      Query qs -> verifyQuerySet qs
      Disjunct (DisjunctAll xs) -> goDisjunct xs
      Conjunct (ConjunctAll xs) -> goConjunct xs

    goConjunct :: NonEmpty (BooleanExpr (QuerySet QueryMetaData)) -> m (QuerySetNegationStatus, QueryResult UserVariableAssignment)
    goConjunct (x :| []) = go x
    goConjunct (x :| y : ys) = do
      result@(negated, status) <- go x
      if not (evaluateQuery negated status)
        then return result
        else goConjunct (y :| ys)

    goDisjunct :: NonEmpty (BooleanExpr (QuerySet QueryMetaData)) -> m (QuerySetNegationStatus, QueryResult UserVariableAssignment)
    goDisjunct (x :| []) = go x
    goDisjunct (x :| y : ys) = do
      result@(negated, status) <- go x
      if evaluateQuery negated status
        then return result
        else goDisjunct (y :| ys)

verifyQuerySet ::
  (MonadVerifyProperty m) =>
  QuerySet QueryMetaData ->
  m (QuerySetNegationStatus, QueryResult UserVariableAssignment)
verifyQuerySet (QuerySet negated disjuncts) = do
  result <- verifyDisjunctAll disjuncts
  return (negated, result)

verifyDisjunctAll ::
  forall m.
  (MonadVerifyProperty m) =>
  DisjunctAll QueryMetaData ->
  m (QueryResult UserVariableAssignment)
verifyDisjunctAll (DisjunctAll ys) = go ys
  where
    go ::
      NonEmpty QueryMetaData ->
      m (QueryResult UserVariableAssignment)
    go (x :| []) = verifyQuery x
    go (x :| y : xs) = do
      r <- verifyQuery x
      if isVerified r
        then return r
        else go (y :| xs)

verifyQuery ::
  (MonadVerifyProperty m) =>
  QueryMetaData ->
  m (QueryResult UserVariableAssignment)
verifyQuery (QueryMetaData queryAddress metaNetwork queryVariableMapping userVars) = do
  logCompilerSection MidDetail ("Verifying query" <+> quotePretty queryAddress) $ do
    (verifierSettings@VerifierSettings {..}, folder, progressBar) <- ask
    let queryFile = folder </> calculateQueryFileName queryAddress
    errorOrResult <- runExceptT $ do
      tell (Sum 1)
      result <- invokeVerifier verifierSettings metaNetwork queryFile
      liftIO $ incProgress progressBar 1

      case result of
        SAT Nothing -> do
          logDebug MidDetail $ "Query is SAT (no witness)" <> line
          return $ SAT Nothing
        SAT (Just witness) -> do
          logDebug MidDetail $ "Query is SAT (witness provided)" <> line
          checkWitness queryVariableMapping witness
          problemSpaceWitness <- reconstructUserVars queryVariableMapping userVars witness
          return $ SAT $ Just problemSpaceWitness
        UnSAT -> do
          logDebug MidDetail $ "Query is UnSAT" <> line
          return UnSAT

    case errorOrResult of
      Right result -> return result
      Left err -> do
        handleVerificationError verifier verifierExecutable metaNetwork queryAddress queryFile err

invokeVerifier ::
  (MonadVerifyProperty m, MonadError VerificationError m) =>
  VerifierSettings ->
  MetaNetwork ->
  QueryFile ->
  m (QueryResult QueryVariableAssignment)
invokeVerifier VerifierSettings {..} metaNetworkEntries queryFile = do
  -- Check query supported
  let usesMultipleNetworks = length metaNetworkEntries > 1
  when (usesMultipleNetworks && not (supportsMultipleNetworkApplications verifier)) $
    throwError $
      UnsupportedMultipleNetworks metaNetworkEntries

  -- Prepare the command
  let args = prepareArgs verifier metaNetworkEntries queryFile <> verifierExtraArgs
  let command = unwords (verifierExecutable : args)

  -- Run the verification command
  logDebug MidDetail $ "Running verification command: " <> line <> indent 2 (pretty command) <> line
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode verifierExecutable args ""
  logDebug MinDetail $ "Command status:" <+> pretty (show exitCode) <> line
  logDebug MinDetail $ "Command stdout:" <> line <> indent 2 (pretty out) <> line
  logDebug MinDetail $ "Command stderr:" <> line <> indent 2 (pretty err) <> line

  -- Check for errors
  case exitCode of
    ExitFailure exitValue
      -- Killed by the system.
      -- See System.Process.html#waitForProcess documentation
      | exitValue < 0 -> throwError $ VerifierTerminatedByOS (-exitValue)
      | otherwise -> throwError $ VerifierError (if null err then out else err)
    _ -> return ()

  -- Parse the result
  parseOutput verifier out

checkWitness :: (MonadError VerificationError m) => QueryVariableMapping -> QueryVariableAssignment -> m ()
checkWitness queryVariableMapping (QueryVariableAssignment witness) = do
  let allVariables = Set.fromList $ fmap fst queryVariableMapping
  let providedVariables = Map.keysSet witness
  let missingVariables = Set.difference allVariables providedVariables
  unless (Set.null missingVariables) $
    throwError $
      VerifierIncompleteWitness missingVariables

handleVerificationError ::
  (MonadVerifyProperty m) =>
  Verifier ->
  VerifierExecutable ->
  MetaNetwork ->
  QueryAddress ->
  QueryFile ->
  VerificationError ->
  m a
handleVerificationError verifier verifierExecutable metaNetwork queryAddress queryFile err = do
  let VerificationErrorAction {..} = convertVerificationError verifier queryAddress err

  reproducerMessage <-
    if reproducerIsUseful
      then createReproducer verifier verifierExecutable metaNetwork queryFile
      else return ""

  let finalMessage = "\n\nError: " <> verificationErrorMessage <> reproducerMessage
  writeStderrLn (layoutAsText finalMessage)
  liftIO exitFailure

createReproducer ::
  (MonadIO m) =>
  Verifier ->
  VerifierExecutable ->
  MetaNetwork ->
  QueryFile ->
  m (Doc a)
createReproducer verifier verifierExecutable metaNetwork queryFile = do
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
  copiedQueryFile <- liftIO $ copyOverFile queryFile

  -- Copy the network files over
  copiedMetaNetwork <- liftIO $ do
    forM metaNetwork $ \MetaNetworkEntry {metaNetworkEntryInfo = NetworkContextInfo {..}, ..} -> do
      newNetworkFilePath <- copyOverFile networkFilepath
      return $ MetaNetworkEntry {metaNetworkEntryInfo = NetworkContextInfo {networkFilepath = newNetworkFilePath, ..}, ..}

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
  (MonadIO m, MonadStdIO m) =>
  FilePath ->
  PropertyAddress ->
  PropertyStatus ->
  m ()
outputPropertyResult verificationCache address result@(PropertyStatus status) = do
  VIO.writeStdoutLn (layoutAsText $ "    result: " <> pretty result)
  writePropertyResult verificationCache address (isVerified result)
  case status of
    NonTrivial (_, SAT (Just (UserVariableAssignment assignments))) -> do
      -- Output assignments to command line
      let assignmentDocs = vsep (fmap prettyUserVariableAssignment assignments)
      let witnessDoc = indent 6 assignmentDocs
      liftIO $ TIO.hPutStrLn stdout (layoutAsText witnessDoc)

      -- Output assignments to file
      let witnessFolder = verificationCache </> layoutAsString (pretty address) <> "-assignments"
      liftIO $ createDirectoryIfMissing True witnessFolder
      forM_ assignments $ \(var, Tensor varDims value) -> do
        let file = witnessFolder </> show var
        let dims = Vector.fromList varDims
        -- TODO got to be a better way to do this conversion...
        let unboxedVector = Vector.fromList $ BoxedVector.toList (fmap realToFrac value)
        let idxData = IDXDoubles IDXDouble dims unboxedVector
        liftIO $ encodeIDXFile idxData file
    _ -> return ()

--------------------------------------------------------------------------------
-- Calculation of file paths

type PropertyProgressBar = ProgressBar ()

createPropertyProgressBar :: (MonadIO m) => PropertyAddress -> Int -> m PropertyProgressBar
createPropertyProgressBar (PropertyAddress _ name indices) numberOfQueries = do
  let propertyName = LazyText.fromStrict $ intercalate "!" (name : fmap (pack . show) indices)
  let style =
        defStyle
          { stylePrefix = msg ("  " <> propertyName),
            stylePostfix = exact <> msg " queries",
            styleWidth = ConstantWidth 80
          }
  let initialProgress = Progress 0 numberOfQueries ()
  liftIO $ hNewProgressBar stdout style 10 initialProgress

closePropertyProgressBar :: (MonadIO m, MonadStdIO m) => PropertyProgressBar -> m ()
closePropertyProgressBar _progressBar = VIO.writeStdoutLn ""
