module Vehicle.Verify.Specification.IO
  ( readSpecification,
    readVerificationPlan,
    outputVerificationResult,
    verifySpecification,
    verificationPlanFileName,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy qualified as BIO
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeExtension, (<.>), (</>))
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.VariableReconstruction (reconstructUserVars)
import Vehicle.Expr.Boolean
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier (queryFormats)

--------------------------------------------------------------------------------
-- Specification

readSpecification :: (MonadIO m) => FilePath -> m SpecificationText
readSpecification inputFile
  | takeExtension inputFile /= vehicleSpecificationFileExtension = do
      fatalError $
        "Specification"
          <+> quotePretty inputFile
          <+> "has unsupported"
          <+> "extension"
          <+> quotePretty (takeExtension inputFile) <> "."
          <+> "Only files with a"
          <+> quotePretty vehicleSpecificationFileExtension
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

-- | Outputs the compiled verification plan and queries that make up the specification.
-- If a folder is provided it outputs them to individual files in that folder,
-- otherwise it outputs them to stdout.
outputVerificationResult ::
  (MonadIO m, MonadLogger m) =>
  QueryFormatID ->
  Maybe FilePath ->
  (VerificationPlan, VerificationQueries) ->
  m ()
outputVerificationResult queryFormatID maybeOutputLocation (plan, queries) = do
  case maybeOutputLocation of
    Nothing -> return ()
    Just folder -> liftIO $ createDirectoryIfMissing True folder

  outputVerificationPlan maybeOutputLocation plan
  writeVerificationQueries queryFormatID maybeOutputLocation queries

outputVerificationPlan :: (MonadIO m) => Maybe FilePath -> VerificationPlan -> m ()
outputVerificationPlan maybeFolder plan = do
  let planText = encodePretty' prettyJSONConfig plan
  case maybeFolder of
    Nothing -> programOutput $ pretty $ decodeUtf8 $ BIO.toStrict $ encodePretty' prettyJSONConfig plan
    Just folder -> do
      let planFile = verificationPlanFileName folder

      maybeError <-
        liftIO $
          catch
            (do BIO.writeFile planFile planText; return Nothing)
            (\(e :: IOException) -> return $ Just e)

      case maybeError of
        Nothing -> return ()
        Just err ->
          fatalError $
            "Unable to write the verification plan to file"
              <+> quotePretty planFile
                <> line
                <> indent 2 ("error:" <+> pretty (show err))

readVerificationPlan :: (MonadIO m) => FilePath -> m VerificationPlan
readVerificationPlan planFile = do
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

writeVerificationQueries ::
  (MonadIO m, MonadLogger m) =>
  QueryFormatID ->
  Maybe FilePath ->
  VerificationQueries ->
  m ()
writeVerificationQueries queryFormatID maybeFolder verificationQueries = do
  let queryFormat = queryFormats queryFormatID
  let queryOutputForm = Just $ queryOutputFormat queryFormat
  _ <- flip traverseSpecification verificationQueries $ \(queryAddress, queryText) -> do
    let queryFileName = calculateQueryFileName queryAddress
    case maybeFolder of
      Nothing -> programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
      Just folder -> do
        let queryFilePath = folder </> queryFileName
        writeResultToFile queryOutputForm (Just queryFilePath) (pretty queryText)

  return ()

verificationPlanFileName :: FilePath -> FilePath
verificationPlanFileName folder = folder </> "verification-plan" <.> vehicleVerificationPlanFileExtension

--------------------------------------------------------------------------------
-- Verification

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  (MonadIO m) =>
  FilePath ->
  Verifier ->
  VerifierExecutable ->
  Specification QueryMetaData ->
  m SpecificationStatus
verifySpecification queryFolder verifier verifierExecutable (Specification namedProperties) = do
  results <- forM namedProperties $ \(name, property) -> do
    programOutput $ "Verifying property" <+> quotePretty name
    result <- verifyMultiProperty verifier verifierExecutable queryFolder property
    return (name, result)
  return $ SpecificationStatus (Map.fromList results)

verifyMultiProperty ::
  forall m.
  (MonadIO m) =>
  Verifier ->
  VerifierExecutable ->
  FilePath ->
  MultiProperty QueryMetaData ->
  m MultiPropertyStatus
verifyMultiProperty verifier verifierExecutable queryFolder = go
  where
    go :: MultiProperty QueryMetaData -> m MultiPropertyStatus
    go = \case
      MultiProperty ps -> MultiPropertyStatus <$> traverse go ps
      SingleProperty _address p -> do
        result <- runReaderT (verifyProperty p) (verifier, verifierExecutable, queryFolder)
        return $ SinglePropertyStatus result

type MonadVerify m =
  ( MonadReader (Verifier, VerifierExecutable, FilePath) m,
    MonadIO m
  )

-- | Lazily tries to verify the property, avoiding evaluating parts
-- of the expression that are not needed.
verifyProperty ::
  (MonadVerify m) =>
  Property QueryMetaData ->
  m PropertyStatus
verifyProperty property = do
  (negationStatus, status) <- go property
  return $ PropertyStatus negationStatus status
  where
    go ::
      (MonadVerify m) =>
      BooleanExpr (QuerySet QueryMetaData) ->
      m (QueryNegationStatus, MaybeTrivial (QueryResult UserVariableCounterexample))
    go = \case
      Query qs -> verifyQuerySet qs
      Disjunct x y -> do
        result@(negated, x') <- go x
        if evaluateQuery negated x'
          then return result
          else go y
      Conjunct x y -> do
        result@(negated, x') <- go x
        if not (evaluateQuery negated x')
          then return result
          else go y

verifyQuerySet ::
  (MonadVerify m) =>
  QuerySet QueryMetaData ->
  m (QueryNegationStatus, MaybeTrivial (QueryResult UserVariableCounterexample))
verifyQuerySet (QuerySet negated queries) = case queries of
  Trivial b -> return (negated, Trivial b)
  NonTrivial disjuncts -> do
    result <- verifyDisjunctAll disjuncts
    return (negated, NonTrivial result)

verifyDisjunctAll ::
  forall m.
  (MonadVerify m) =>
  DisjunctAll (QueryAddress, QueryMetaData) ->
  m (QueryResult UserVariableCounterexample)
verifyDisjunctAll (DisjunctAll ys) = go ys
  where
    go ::
      (Monad m) =>
      NonEmpty (QueryAddress, QueryMetaData) ->
      m (QueryResult UserVariableCounterexample)
    go (x :| []) = verifyQuery x
    go (x :| y : xs) = do
      r <- verifyQuery x
      if isVerified r
        then return r
        else go (y :| xs)

verifyQuery ::
  (MonadVerify m) =>
  (QueryAddress, QueryMetaData) ->
  m (QueryResult UserVariableCounterexample)
verifyQuery (queryAddress, QueryData metaNetwork userVar) = do
  (verifier, verifierExecutable, folder) <- ask
  let queryFile = folder </> calculateQueryFileName queryAddress
  result <- invokeVerifier verifier verifierExecutable metaNetwork queryFile
  return $ fmap (reconstructUserVars userVar) result

--------------------------------------------------------------------------------
-- Calculation of file paths

calculateQueryFileName :: QueryAddress -> FilePath
calculateQueryFileName ((propertyName, propertyIndices), queryID) = do
  let propertyStr
        | null propertyIndices = ""
        | otherwise = concatMap (\v -> "!" <> show v) (reverse propertyIndices)

  unpack propertyName
    <> propertyStr
    <> "-query"
    <> show queryID <.> "txt"
