module Vehicle.Verify.Specification.IO
  ( readSpecification,
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
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy qualified as BIO
import Data.IDX (encodeIDXFile)
import Data.IDX.Internal
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (intercalate, pack, unpack)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as LazyText
import Data.Vector.Unboxed qualified as Vector (fromList)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (<.>), (</>))
import System.IO (stderr, stdout)
import System.ProgressBar
import Vehicle.Backend.Prelude (writeResultToFile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.Variable (UserVariable (..))
import Vehicle.Compile.Queries.VariableReconstruction (reconstructUserVars)
import Vehicle.Expr.Boolean
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status

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
  (MonadLogger m, MonadIO m) =>
  FilePath ->
  PropertyAddress ->
  PropertyVerificationPlan ->
  m ()
writePropertyVerificationPlan folder propertyAddress plan = do
  let planFile = propertyPlanFileName folder propertyAddress
  let planText = encodePretty' prettyJSONConfig plan

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
  (MonadLogger m, MonadIO m) =>
  QueryFormat ->
  FilePath ->
  (QueryAddress, QueryText) ->
  m ()
writeVerificationQuery queryFormat folder (queryAddress, queryText) = do
  let queryOutputForm = queryOutputFormat queryFormat
  let queryFilePath = folder </> calculateQueryFileName queryAddress
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

type MonadVerify m =
  ( MonadLogger m,
    MonadIO m
  )

type MonadVerifyProperty m =
  ( MonadVerify m,
    MonadReader (Verifier, VerifierExecutable, FilePath, PropertyProgressBar) m
  )

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  (MonadVerify m) =>
  FilePath ->
  Verifier ->
  VerifierExecutable ->
  m ()
verifySpecification queryFolder verifier verifierExecutable = do
  programOutput "Verifying properties:"
  let verificationPlanFile = specificationCacheIndexFileName queryFolder
  SpecificationCacheIndex {..} <- readSpecificationCacheIndex verificationPlanFile
  maybeIntegrityError <- checkIntegrityOfResources resourcesIntegrityInfo
  case maybeIntegrityError of
    Just err -> programOutput $ "Resource error:" <+> pretty err
    Nothing -> do
      let propertyAddresses = concatMap (multiPropertyAddresses . snd) properties
      forM_ propertyAddresses $
        verifyProperty verifier verifierExecutable queryFolder

verifyProperty ::
  forall m.
  (MonadVerify m) =>
  Verifier ->
  VerifierExecutable ->
  FilePath ->
  PropertyAddress ->
  m ()
verifyProperty verifier verifierExecutable verificationCache address = do
  let propertyPlanFile = propertyPlanFileName verificationCache address
  PropertyVerificationPlan {..} <- readPropertyVerificationPlan propertyPlanFile
  progressBar <- createPropertyProgressBar address (propertySize queryMetaData)
  let readerState = (verifier, verifierExecutable, verificationCache, progressBar)
  result <- runReaderT (verifyPropertyBooleanStructure queryMetaData) readerState
  outputPropertyResult verificationCache address result

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyPropertyBooleanStructure ::
  forall m.
  (MonadVerifyProperty m) =>
  Property QueryMetaData ->
  m PropertyStatus
verifyPropertyBooleanStructure property = do
  (negationStatus, status) <- go property
  return $ PropertyStatus negationStatus status
  where
    go ::
      BooleanExpr (QuerySet QueryMetaData) ->
      m (QuerySetNegationStatus, MaybeTrivial (QueryResult UserVariableAssignment))
    go = \case
      Query qs -> verifyQuerySet qs
      Disjunct x y -> do
        result@(negated, status) <- go x
        if evaluateQuery negated status
          then return result
          else go y
      Conjunct x y -> do
        result@(negated, status) <- go x
        if not (evaluateQuery negated status)
          then return result
          else go y

verifyQuerySet ::
  (MonadVerifyProperty m) =>
  QuerySet QueryMetaData ->
  m (QuerySetNegationStatus, MaybeTrivial (QueryResult UserVariableAssignment))
verifyQuerySet (QuerySet negated queries) = case queries of
  Trivial b -> return (negated, Trivial b)
  NonTrivial disjuncts -> do
    result <- verifyDisjunctAll disjuncts
    return (negated, NonTrivial result)

verifyDisjunctAll ::
  forall m.
  (MonadVerifyProperty m) =>
  DisjunctAll (QueryAddress, QueryMetaData) ->
  m (QueryResult UserVariableAssignment)
verifyDisjunctAll (DisjunctAll ys) = go ys
  where
    go ::
      NonEmpty (QueryAddress, QueryMetaData) ->
      m (QueryResult UserVariableAssignment)
    go (x :| []) = verifyQuery x
    go (x :| y : xs) = do
      r <- verifyQuery x
      if isVerified r
        then return r
        else go (y :| xs)

verifyQuery ::
  (MonadVerifyProperty m) =>
  (QueryAddress, QueryMetaData) ->
  m (QueryResult UserVariableAssignment)
verifyQuery (queryAddress, QueryData metaNetwork userVar) = do
  (verifier, verifierExecutable, folder, progressBar) <- ask
  let queryFile = folder </> calculateQueryFileName queryAddress
  errorOrResult <- invokeVerifier verifier verifierExecutable metaNetwork queryFile
  case errorOrResult of
    Left errMsg -> liftIO $ do
      TIO.hPutStrLn stderr ("\nError: " <> errMsg)
      exitFailure
    Right result -> do
      liftIO $ incProgress progressBar 1
      traverse (reconstructUserVars metaNetwork userVar) result

--------------------------------------------------------------------------------
-- Assignments

outputPropertyResult ::
  (MonadIO m) =>
  FilePath ->
  PropertyAddress ->
  PropertyStatus ->
  m ()
outputPropertyResult verificationCache address result@(PropertyStatus _negated s) = do
  liftIO $ TIO.putStrLn (layoutAsText $ "    result: " <> pretty result)
  writePropertyResult verificationCache address (isVerified result)
  case s of
    NonTrivial (SAT (Just (UserVariableAssignment assignments))) -> do
      -- Output assignments to command line
      let assignmentDocs = vsep (fmap prettyUserVariableAssignment assignments)
      let witnessDoc = indent 6 assignmentDocs
      liftIO $ TIO.hPutStrLn stdout (layoutAsText witnessDoc)

      -- Output assignments to file
      let witnessFolder = verificationCache </> layoutAsString (pretty address) <> "-assignments"
      liftIO $ createDirectoryIfMissing True witnessFolder
      forM_ assignments $ \(UserVariable {..}, value) -> do
        let file = witnessFolder </> unpack userVarName
        let dims = Vector.fromList userVarDimensions
        let idxData = IDXDoubles IDXDouble dims value
        liftIO $ encodeIDXFile idxData file
    _ -> return ()

--------------------------------------------------------------------------------
-- Calculation of file paths

type PropertyProgressBar = ProgressBar ()

createPropertyProgressBar :: (MonadIO m) => PropertyAddress -> Int -> m PropertyProgressBar
createPropertyProgressBar (PropertyAddress name indices) numberOfQueries = do
  let propertyName = LazyText.fromStrict $ intercalate "!" (name : fmap (pack . show) indices)
  let style =
        defStyle
          { stylePrefix = msg ("  " <> propertyName),
            stylePostfix = exact <> msg " queries",
            styleWidth = ConstantWidth 80
          }
  let initialProgress = Progress 0 numberOfQueries ()
  liftIO $ hNewProgressBar stdout style 10 initialProgress

{-
completeProgress :: MonadIO m => PropertyProgressBar -> m ()
completeProgress progressBar = liftIO $ updateProgress progressBar $ \Progress{..} ->
  Progress { progressDone = progressTodo, .. }
-}
