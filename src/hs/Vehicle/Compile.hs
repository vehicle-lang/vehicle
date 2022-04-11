module Vehicle.Compile
  ( module CompilePrelude
  , CompileOptions(..)
  , compile
  , compileToMarabou
  , compileToAgda
  , typeCheck
  , typeCheckExpr
  , readInputFile
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (IOException, catch)
import Data.Text as T (Text, pack)
import Data.Text.IO qualified as TIO
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn)
import System.Exit (exitFailure)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.Elaborate.External as External (runElab, runElabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck)
import Vehicle.Compile.Normalise (normalise, defaultNormalisationOptions)
import Vehicle.Compile.RemoveNetworkDecls (removeNetworkDecls)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouProperty)
import Vehicle.Backend.VNNLib qualified as VNNLib
import Vehicle.Backend.VNNLib (VNNLibProperty, writeVNNLibQueryFiles)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.LossFunction ( LExpr, writeLossFunctionFiles)

import Vehicle.Verify.VerificationStatus ( getProofCacheLocation )
import Vehicle.Resource.NeuralNetwork (NetworkMap)
import Vehicle.Compile.RemoveDatasetDecls (removeDatasetDecls)

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = do
  let resources = collateResourceLocations networks datasets
  spec  <- readInputFile loggingOptions inputFile
  case target of
    ITP Agda -> do
      let moduleName = pack $ maybe "" (<> ".") modulePrefix <> maybe "Spec" takeBaseName outputFile
      proofCacheLocation <- getProofCacheLocation loggingOptions proofCache
      let agdaOptions = AgdaOptions proofCacheLocation moduleName mempty
      agdaCode <- compileToAgda loggingOptions agdaOptions spec
      writeAgdaFile outputFile agdaCode

    Verifier Marabou -> do
      marabouProperties <- compileToMarabou loggingOptions spec resources
      Marabou.writeSpecFiles outputFile marabouProperties

    Verifier VNNLib -> do
      vnnlibProperties <- compileToVNNLib loggingOptions spec resources
      writeVNNLibQueryFiles outputFile vnnlibProperties

    LossFunction -> do
      lossFunction <- compileToLossFunction loggingOptions spec resources
      writeLossFunctionFiles outputFile lossFunction

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToMarabou :: LoggingOptions
                 -> Text
                 -> ResourceLocations
                 -> IO [MarabouProperty]
compileToMarabou loggingOptions spec resources =
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckAndExpandResources resources spec
    Marabou.compile networkCtx prog

compileToVNNLib :: LoggingOptions
                -> Text
                -> ResourceLocations
                -> IO [VNNLibProperty]
compileToVNNLib loggingOptions spec resources =
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckAndExpandResources resources spec
    VNNLib.compile networkCtx prog

compileToAgda :: LoggingOptions -> AgdaOptions -> Text -> IO (Doc a)
compileToAgda loggingOptions agdaOptions spec =
  fromLoggedEitherIO loggingOptions $ do
    prog <- typeCheckProg spec
    compileProgToAgda agdaOptions prog

compileToLossFunction :: LoggingOptions -> Text -> ResourceLocations -> IO [LExpr]
compileToLossFunction loggingOptions spec resources = do
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckAndExpandResources resources spec
    LossFunction.compile networkCtx prog

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

readInputFile :: MonadIO m => LoggingOptions -> FilePath -> m Text
readInputFile LoggingOptions{..} inputFile = do
  liftIO $ TIO.readFile inputFile `catch` \ (e :: IOException) -> do
    hPutStrLn errorHandle $
      "Error occured while reading input file: \n  " <> show e
    exitFailure

typeCheckProg :: MonadCompile m
              => Text
              -> m CheckedProg
typeCheckProg txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElab bnfcProg
  scopedProg  <- scopeCheck vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

typeCheckExpr :: MonadCompile m
              => Text
              -> m CheckedExpr
typeCheckExpr txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElabExpr bnfcProg
  scopedProg  <- scopeCheckClosedExpr vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

typeCheckAndExpandResources :: (MonadIO m, MonadCompile m)
                            => ResourceLocations
                            -> Text
                            -> m (NetworkMap, CheckedProg)
typeCheckAndExpandResources resources txt = do
  typedProg <- typeCheckProg txt
  normProg <- normalise defaultNormalisationOptions typedProg
  datasetlessProg <- removeDatasetDecls resources normProg
  (networks, networklessProg) <- removeNetworkDecls datasetlessProg
  normProg2 <- normalise defaultNormalisationOptions networklessProg
  return (networks, normProg2)