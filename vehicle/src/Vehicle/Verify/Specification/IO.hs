module Vehicle.Verify.Specification.IO
  ( readSpecification,
    outputVerificationQueries,
    verifySpecification,
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
import Data.Text (unpack)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeExtension, (<.>), (</>))
import Vehicle.Backend.Prelude (Backend (..), writeResultToFile)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier.Interface

--------------------------------------------------------------------------------
-- Specification

readSpecification :: MonadIO m => FilePath -> m SpecificationText
readSpecification inputFile
  | takeExtension inputFile /= vehicleFileExtension = do
      fatalError $
        "Specification"
          <+> quotePretty inputFile
          <+> "has unsupported"
          <+> "extension"
          <+> quotePretty (takeExtension inputFile) <> "."
          <+> "Only files with a"
          <+> quotePretty vehicleFileExtension
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
outputVerificationQueries ::
  MonadIO m =>
  Verifier ->
  Maybe FilePath ->
  (VerificationPlan, VerificationQueries) ->
  m ()
outputVerificationQueries verifier maybeOutputLocation (plan, queries) =
  case maybeOutputLocation of
    Nothing -> do
      programOutput (pretty plan)
      programOutput (pretty queries)
    Just outputLocation -> do
      liftIO $ createDirectoryIfMissing True outputLocation

      writeVerificationPlan outputLocation plan
      writeVerificationQueries verifier outputLocation queries

writeVerificationPlan :: MonadIO m => FilePath -> VerificationPlan -> m ()
writeVerificationPlan folder plan = do
  let planText = encodePretty' prettyJSONConfig plan
  let planFile = mkPlanFileName folder

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
readVerificationPlan folder = do
  let planFile = mkPlanFileName folder
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

mkPlanFileName :: FilePath -> FilePath
mkPlanFileName folder = folder </> "verification-plan.vcle"

writeVerificationQueries :: MonadIO m => Verifier -> FilePath -> VerificationQueries -> m ()
writeVerificationQueries Verifier {..} folder (Specification properties) = do
  let backend = VerifierBackend verifierIdentifier
  forM_ properties $ \(ident, property) -> do
    let property' = calculateFilePaths folder ident property
    _ <- flip traverseProperty property' $ \(queryFilePath, queryText) ->
      liftIO $ writeResultToFile backend (Just queryFilePath) (pretty queryText)

    return ()

--------------------------------------------------------------------------------
-- Verification

-- | Uses the verifier to verify the specification. Failure of one property does
-- not prevent the verification of the other properties.
verifySpecification ::
  MonadIO m =>
  Verifier ->
  VerifierExecutable ->
  FilePath ->
  m SpecificationStatus
verifySpecification verifier verifierExecutable queryFolder = do
  Specification namedProperties <- readVerificationPlan queryFolder
  results <- forM namedProperties $ \(name, property) -> do
    let property' = calculateFilePaths queryFolder name property
    result <- runReaderT (verifyProperty property') (verifier, verifierExecutable)
    return (nameOf name, result)
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
  m SatisfiabilityStatus
verifyQuery (queryFile, QueryData metaNetwork userVar) = do
  (verifier, verifierExecutable) <- ask
  invokeVerifier verifier verifierExecutable metaNetwork userVar queryFile

--------------------------------------------------------------------------------
-- Calculation of file paths

-- | Indices into a multi-property.
type MultiPropertyIndex = Int

calculateFilePaths :: FilePath -> Identifier -> Property a -> Property (FilePath, a)
calculateFilePaths directory propertyIdentifier = goProperty []
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
        </> unpack (nameOf propertyIdentifier)
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
