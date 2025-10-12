{-# LANGUAGE OverloadedStrings #-}

module Vehicle.Verify.Specification.IO
  ( readSpecification,
    writeSpecificationCache,
    readSpecificationCacheIndex,
    writeVerificationQuery,
    writePropertyVerificationPlan,
    readPropertyVerificationPlan,
    writePropertyResult,
    readPropertyResult,
    specificationCacheIndexFileName,
    isValidQueryFolder,
    propertyPlanFileName,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty')
import Data.ByteString.Lazy qualified as BIO
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (<.>), (</>))
import Vehicle.Backend.Agda.Interact (writeResultToFile)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification

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
