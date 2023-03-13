module Vehicle.Verify.Specification.IO
  ( readSpecification,
    readVerificationPlan,
    outputVerificationResult,
    verifySpecification,
    verificationPlanFileName,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy qualified as BIO
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeExtension, (<.>), (</>))
import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier (queryFormats)

--------------------------------------------------------------------------------
-- Specification

readSpecification :: MonadIO m => FilePath -> m SpecificationText
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
  outputVerificationQueries queryFormatID maybeOutputLocation queries

outputVerificationPlan :: MonadIO m => Maybe FilePath -> VerificationPlan -> m ()
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

readVerificationPlan :: MonadIO m => FilePath -> m VerificationPlan
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

outputVerificationQueries ::
  (MonadLogger m, MonadIO m) =>
  QueryFormatID ->
  Maybe FilePath ->
  VerificationQueries ->
  m ()
outputVerificationQueries queryFormatID maybeFolder (Specification properties) = do
  forM_ properties $ \(ident, property) -> do
    let folderName = fromMaybe "" maybeFolder
    let property' = calculateFilePaths folderName ident property
    let queryFormat = queryFormats queryFormatID
    let queryOutputForm = Just $ queryOutputFormat queryFormat
    _ <- flip traverseProperty property' $ \(queryFilePath, queryText) ->
      case maybeFolder of
        Just {} -> writeResultToFile queryOutputForm (Just queryFilePath) (pretty queryText)
        Nothing -> programOutput ("\n" <> pretty queryFilePath <> "\n\n" <> pretty queryText)

    return ()

verificationPlanFileName :: FilePath -> FilePath
verificationPlanFileName folder = folder </> "verification-plan" <.> vehicleVerificationPlanFileExtension

--------------------------------------------------------------------------------
-- Verification

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  MonadIO m =>
  FilePath ->
  Verifier ->
  VerifierExecutable ->
  Specification QueryMetaData ->
  m SpecificationStatus
verifySpecification queryFolder verifier verifierExecutable (Specification namedProperties) = do
  results <- forM namedProperties $ \(name, property) -> do
    let property' = calculateFilePaths queryFolder name property
    result <- runReaderT (verifyProperty property') (verifier, verifierExecutable)
    return (name, result)
  return $ SpecificationStatus (Map.fromList results)

verifyProperty ::
  (MonadReader (Verifier, VerifierExecutable) m, MonadIO m) =>
  Property (QueryFile, QueryMetaData) ->
  m PropertyStatus
verifyProperty = \case
  SingleProperty p -> SinglePropertyStatus <$> evaluateBoolPropertyM verifyQuery isVerified p
  MultiProperty ps -> MultiPropertyStatus <$> traverse verifyProperty ps

verifyQuery ::
  (MonadReader (Verifier, VerifierExecutable) m, MonadIO m) =>
  (QueryFile, QueryMetaData) ->
  m QueryResult
verifyQuery (queryFile, QueryData metaNetwork userVar) = do
  (verifier, verifierExecutable) <- ask
  invokeVerifier verifier verifierExecutable metaNetwork userVar queryFile

--------------------------------------------------------------------------------
-- Calculation of file paths

-- | Indices into a multi-property.
type MultiPropertyIndex = Int

calculateFilePaths :: FilePath -> Name -> Property a -> Property (FilePath, a)
calculateFilePaths directory propertyName = goProperty []
  where
    goProperty :: [MultiPropertyIndex] -> Property a -> Property (FilePath, a)
    goProperty propertyIndices = \case
      MultiProperty subproperties -> do
        let numberedSubproperties = zip [0 :: QueryID ..] subproperties
        let result = flip fmap numberedSubproperties $ \(i, p) ->
              goProperty (i : propertyIndices) p
        MultiProperty result
      SingleProperty propertyExpr ->
        SingleProperty $ goQuery propertyIndices propertyExpr

    goQuery :: [MultiPropertyIndex] -> BoolProperty a -> BoolProperty (FilePath, a)
    goQuery propertyIndices = fmapNumberedBoolProperty $ first $ \queryID ->
      directory
        </> unpack propertyName
          <> propertyStr
          <> "-query"
          <> show queryID <.> "txt"
      where
        propertyStr =
          if null propertyIndices
            then ""
            else concatMap (\v -> "!" <> show v) (reverse propertyIndices)

fmapNumberedBoolProperty ::
  forall a b.
  ((QueryID, a) -> b) ->
  BoolProperty a ->
  BoolProperty b
fmapNumberedBoolProperty f s =
  runSupply (traverseBoolProperty f' s) [1 ..]
  where
    f' :: a -> Supply Int b
    f' x = do
      queryID <- demand
      lift $ return $ f (queryID, x)
