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
import Vehicle.Compile.Elaborate.External as External (elabProg, elabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck)
import Vehicle.Compile.Normalise (normalise, defaultNormalisationOptions)
import Vehicle.Compile.Resource.Network (removeNetworkDecls)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouProperty)
import Vehicle.Backend.VNNLib qualified as VNNLib
import Vehicle.Backend.VNNLib (VNNLibProperty, writeVNNLibQueryFiles)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.LossFunction ( LExpr, writeLossFunctionFiles)

import Vehicle.Verify.VerificationStatus ( getProofCacheLocation )
import Vehicle.Resource.NeuralNetwork (NetworkCtx)
import Vehicle.Compile.Resource.Dataset (removeDatasetDecls)
import Vehicle.Compile.Resource.Parameter (expandParameters)

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = do
  let resources = Resources networkLocations datasetLocations parameterValues
  spec <- readInputFile loggingOptions inputFile
  case target of
    ITP Agda -> do
      let moduleName = pack $ maybe "" (<> ".") modulePrefix <> maybe "Spec" takeBaseName outputFile
      proofCacheLocation <- getProofCacheLocation loggingOptions proofCache
      let agdaOptions = AgdaOptions proofCacheLocation moduleName mempty
      agdaCode <- compileToAgda loggingOptions agdaOptions resources spec
      writeAgdaFile outputFile agdaCode

    Verifier Marabou -> do
      marabouProperties <- compileToMarabou loggingOptions resources spec
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
                 -> Resources
                 -> Text
                 -> IO [MarabouProperty]
compileToMarabou loggingOptions resources spec =
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckProgAndLoadResources resources spec
    Marabou.compile networkCtx prog

compileToVNNLib :: LoggingOptions
                -> Text
                -> Resources
                -> IO [VNNLibProperty]
compileToVNNLib loggingOptions spec resources =
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckProgAndLoadResources resources spec
    VNNLib.compile networkCtx prog

compileToAgda :: LoggingOptions
              -> AgdaOptions
              -> Resources
              -> Text
              -> IO (Doc a)
compileToAgda loggingOptions agdaOptions resources spec =
  fromLoggedEitherIO loggingOptions $ do
    prog <- typeCheckProgWithoutLoadingResources resources spec
    compileProgToAgda agdaOptions prog

compileToLossFunction :: LoggingOptions
                      -> Text
                      -> Resources
                      -> IO [LExpr]
compileToLossFunction loggingOptions spec resources = do
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckProgAndLoadResources resources spec
    LossFunction.compile networkCtx prog

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

readInputFile :: MonadIO m => LoggingOptions -> FilePath -> m Text
readInputFile LoggingOptions{..} inputFile = do
  liftIO $ TIO.readFile inputFile `catch` \ (e :: IOException) -> do
    hPutStrLn errorHandle $
      "Error occured while reading input file: \n  " <> show e
    exitFailure

typeCheckExpr :: MonadCompile m
              => Text
              -> m CheckedExpr
typeCheckExpr txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- elabExpr bnfcProg
  scopedProg  <- scopeCheckClosedExpr vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

-- | Parses, expands parameters and datasets, type-checks and then
-- checks the network types from disk. Used during compilation to
-- verification queries.
typeCheckProgAndLoadResources :: (MonadIO m, MonadCompile m)
                              => Resources
                              -> Text
                              -> m (NetworkCtx, CheckedProg)
typeCheckProgAndLoadResources Resources{..} txt = do
  bnfcProg        <- parseVehicle txt
  vehicleProg     <- elabProg bnfcProg
  parameterProg   <- expandParameters parameters vehicleProg
  scopedProg      <- scopeCheck parameterProg
  typedProg       <- typeCheck scopedProg
  normProg        <- normalise defaultNormalisationOptions typedProg
  datasetlessProg <- removeDatasetDecls datasets normProg
  (networkCtx, networklessProg) <- removeNetworkDecls datasetlessProg
  normProg2 <- normalise defaultNormalisationOptions networklessProg
  return (networkCtx, normProg2)

-- | Parses, expands parameters and type-checks the program but does
-- not load networks and datasets from disk. Used during compilation
-- to ITPs.
typeCheckProgWithoutLoadingResources :: MonadCompile m
                                     => Resources
                                     -> Text
                                     -> m CheckedProg
typeCheckProgWithoutLoadingResources Resources{..} txt = do
  bnfcProg      <- parseVehicle txt
  vehicleProg   <- elabProg bnfcProg
  parameterProg <- expandParameters parameters vehicleProg
  scopedProg    <- scopeCheck parameterProg
  typedProg     <- typeCheck scopedProg
  return typedProg
