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
import Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT)
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
import Prettyprinter (fill)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
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
import Vehicle.Data.Tensor as Tensor (HasShape (..), toVector)
import Vehicle.Prelude.IO qualified as VIO (MonadStdIO (writeStdoutLn))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.QueryFormat.Core
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
writeVerificationQuery queryFormat verificationCache (queryMetaData, queryText) = do
  let queryOutputForm = queryOutputFormat queryFormat
  let queryFilePath = calculateQueryFileName verificationCache (queryAddress queryMetaData)
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
-- Verification stats

data MultiPropertyStats = MultiPropertyStats
  { numberVerified :: Int,
    numberFalsified :: Int,
    numberTimedOut :: Int,
    numberErrored :: Int
  }

instance Semigroup MultiPropertyStats where
  s1 <> s2 =
    MultiPropertyStats
      { numberVerified = numberVerified s1 + numberVerified s2,
        numberFalsified = numberFalsified s1 + numberFalsified s2,
        numberTimedOut = numberTimedOut s1 + numberTimedOut s2,
        numberErrored = numberErrored s1 + numberErrored s2
      }

instance Monoid MultiPropertyStats where
  mempty =
    MultiPropertyStats
      { numberVerified = 0,
        numberFalsified = 0,
        numberTimedOut = 0,
        numberErrored = 0
      }

outputStats :: (MonadVerify m) => PropertyName -> MultiPropertyStats -> m ()
outputStats name MultiPropertyStats {..} = do
  let results =
        [ ("verified", numberVerified),
          ("falsified", numberFalsified),
          ("timed-out", numberTimedOut),
          ("errored", numberErrored)
        ] ::
          [(String, Int)]
  let totalSize = sum $ fmap snd results
  when (totalSize > 1) $ do
    let maxTextLength = maximum $ fmap (length . fst) results
    let prettyResult (t, x) = fill (maxTextLength + 1) (pretty t <> ":") <+> pretty x <> "/" <> pretty totalSize
    let finalDoc = pretty name <> ":" <> line <> indent 4 (vsep (fmap prettyResult results))
    programOutput $ "  " <> finalDoc

logPropertyStatus :: (MonadWriter MultiPropertyStats m) => PropertyStatus -> m ()
logPropertyStatus status = tell $ case status of
  PropertyErrored (_, VerifierTimedOut) -> mempty {numberTimedOut = 1}
  PropertyErrored _ -> mempty {numberErrored = 1}
  _
    | isVerified status -> mempty {numberVerified = 1}
    | otherwise -> mempty {numberFalsified = 1}

--------------------------------------------------------------------------------
-- Verification

data VerifierSettings = VerifierSettings
  { verifier :: Verifier,
    verifierExecutable :: VerifierExecutable,
    verifierExtraArgs :: [String],
    noSatPrint :: Bool
  }

type MonadVerify m =
  ( MonadLogger m,
    MonadIO m,
    MonadStdIO m
  )

type MonadVerifyProperty m =
  ( MonadVerify m,
    MonadReader (VerifierSettings, FilePath, PropertyProgressBar) m,
    MonadWriter (Sum Int) m,
    MonadError (QueryMetaData, VerificationError) m
  )

type MonadVerifyQuery m =
  ( MonadVerify m,
    MonadReader (VerifierSettings, FilePath, PropertyProgressBar) m,
    MonadError VerificationError m
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
    Nothing -> forM_ properties $ \(name, multiProperty) -> do
      stats <- execWriterT $ verifyMultiproperty verifierSettings queryFolder multiProperty
      outputStats name stats

verifyMultiproperty ::
  (MonadVerify m, MonadWriter MultiPropertyStats m) =>
  VerifierSettings ->
  FilePath ->
  MultiProperty () ->
  m ()
verifyMultiproperty settings queryFolder = \case
  MultiProperty properties -> forM_ properties (verifyMultiproperty settings queryFolder)
  SingleProperty address () -> verifyProperty settings queryFolder address

verifyProperty ::
  forall m.
  (MonadVerify m, MonadWriter MultiPropertyStats m) =>
  VerifierSettings ->
  FilePath ->
  PropertyAddress ->
  m ()
verifyProperty verifierSettings verificationCache address = do
  -- Read the verification plan for the property
  let propertyPlanFile = propertyPlanFileName verificationCache address
  PropertyVerificationPlan {..} <- readPropertyVerificationPlan propertyPlanFile

  result <- case queryMetaData of
    Trivial status -> return $ PropertyCompleted (Trivial status)
    NonTrivial structure -> do
      logCompilerSection MinDetail ("Verifying property" <+> quotePretty address) $ do
        -- Perform the verification
        let numberOfQueries = propertySize queryMetaData
        progressBar <- createPropertyProgressBar address numberOfQueries
        let readerState = (verifierSettings, verificationCache, progressBar)
        (errorOrResult, Sum numberOfQueriesExecuted) <-
          runWriterT (runExceptT (runReaderT (verifyPropertyBooleanStructure structure) readerState))

        -- The progress bar is only closed when all queries are run.
        -- Not all queries are run, (e.g. short-circuited counter-example found or error occured).
        -- In this case we have to close it manually.
        when (numberOfQueriesExecuted < numberOfQueries) $ do
          closePropertyProgressBar progressBar

        case errorOrResult of
          Left err -> return $ PropertyErrored err
          Right result -> return $ PropertyCompleted $ NonTrivial result

  outputPropertyResult verifierSettings verificationCache address result

  logPropertyStatus result

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyPropertyBooleanStructure ::
  forall m.
  (MonadVerifyProperty m) =>
  BooleanExpr (QuerySet QueryMetaData) ->
  m (QuerySetNegationStatus, QueryResult UserVariableAssignment)
verifyPropertyBooleanStructure = go
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
verifyQuery queryMetaData@(QueryMetaData queryAddress metaNetwork variables reconstruction) = do
  logCompilerSection MidDetail ("Verifying query" <+> quotePretty queryAddress) $ do
    (verifierSettings, verificationCache, progressBar) <- ask
    let queryFile = calculateQueryFileName verificationCache queryAddress

    errorOrResult <- runExceptT $ do
      result <- invokeVerifier verifierSettings metaNetwork queryFile
      liftIO $ incProgress progressBar 1

      case result of
        SAT Nothing -> do
          logDebug MidDetail $ "Query is SAT (no witness)" <> line
          return $ SAT Nothing
        SAT (Just witness) -> do
          logDebug MidDetail $ "Query is SAT (witness provided)" <> line
          checkWitness (getQueryVariables variables) witness
          problemSpaceWitness <- reconstructUserVars variables reconstruction witness
          return $ SAT $ Just problemSpaceWitness
        UnSAT -> do
          logDebug MidDetail $ "Query is UnSAT" <> line
          return UnSAT

    case errorOrResult of
      Left err -> throwError (queryMetaData, err)
      Right result -> do
        tell (Sum 1)
        return result

invokeVerifier ::
  (MonadVerifyQuery m) =>
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

checkWitness :: (MonadVerifyQuery m) => [QueryVariable] -> QueryVariableAssignment -> m ()
checkWitness queryVariables (QueryVariableAssignment witness) = do
  let allVariables = Set.fromList queryVariables
  let providedVariables = Map.keysSet witness
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
  copiedMetaNetwork <- liftIO $ do
    forM metaNetwork $ \(name, NetworkContextInfo {..}, apps) -> do
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
  VerifierSettings ->
  FilePath ->
  PropertyAddress ->
  PropertyStatus ->
  m ()
outputPropertyResult verifierSettings verificationCache address result = do
  let VerifierSettings {..} = verifierSettings

  -- Write the result to the cache
  writePropertyResult verificationCache address (isVerified result)

  -- Print result to command line
  let verifierName = pretty (verifierID verifier)
  let (verified, evidenceText) = case result of
        PropertyCompleted maybeResult -> do
          case maybeResult of
            Trivial status -> (Just status, "(trivial)")
            NonTrivial (negated, status) -> do
              let witnessText = if negated then "counterexample" else "witness"
              case status of
                UnSAT -> (Just negated, verifierName <+> "proved no" <+> witnessText <+> "exists")
                SAT Nothing -> (Just (not negated), verifierName <+> "found no" <> witnessText)
                SAT Just {} -> (Just (not negated), verifierName <+> "found a" <+> witnessText)
        PropertyErrored (_, err) -> do
          let cause = if isTimeoutError err then "timed out" else "errored"
          (Nothing, verifierName <+> cause)
  VIO.writeStdoutLn (layoutAsText $ "    result: " <> pretty (statusSymbol verified) <+> "-" <+> evidenceText)

  -- Output any additional information
  case result of
    PropertyCompleted status -> case status of
      NonTrivial (_, SAT (Just (UserVariableAssignment assignments))) -> do
        -- Output assignments to command line
        unless noSatPrint $ do
          let assignmentDocs = vsep (fmap prettyUserVariableAssignment assignments)
          let witnessDoc = indent 6 assignmentDocs
          liftIO $ TIO.hPutStrLn stdout (layoutAsText witnessDoc)

        -- Output assignments to file
        let witnessFolder = verificationCache </> layoutAsString (pretty address) <> "-assignments"
        liftIO $ createDirectoryIfMissing True witnessFolder
        forM_ assignments $ \(var, tensor) -> do
          let file = witnessFolder </> show var
          let dims = Vector.fromList (shapeOf tensor)
          -- TODO got to be a better way to do this conversion...
          let unboxedVector = Vector.fromList $ BoxedVector.toList (fmap realToFrac (Tensor.toVector tensor))
          let idxData = IDXDoubles IDXDouble dims unboxedVector
          liftIO $ encodeIDXFile idxData file
      _ -> return ()
    PropertyErrored (QueryMetaData {..}, err) -> do
      let VerificationErrorAction {..} = convertVerificationError verifier queryAddress err

      reproducerMessage <-
        if reproducerIsUseful
          then createReproducer verifier verifierExecutable verificationCache metaNetwork queryAddress
          else return ""

      unless (isTimeoutError err) $ do
        let finalMessage = "\nError: " <> verificationErrorMessage <> reproducerMessage
        writeStderrLn (layoutAsText finalMessage)

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
