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
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO
import System.IO (hPutStrLn)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.Elaborate.External as External (elabProg, elabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouSpec)
import Vehicle.Backend.Agda
import Vehicle.Backend.LossFunction qualified as LossFunction
import Vehicle.Backend.LossFunction ( LExpr, writeLossFunctionFiles)
import Vehicle.Compile.Resource
import Vehicle.Compile.ExpandResources

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = do
  let resources = Resources networkLocations datasetLocations parameterValues
  spec <- readInputFile loggingOptions specificationFile
  case target of
    ITP Agda -> do
      proofCacheLocation <- maybe (return Nothing) (fmap Just . makeAbsolute) proofCache
      let agdaOptions = AgdaOptions proofCacheLocation outputFile modulePrefix
      agdaCode <- compileToAgda loggingOptions agdaOptions resources spec
      writeAgdaFile outputFile agdaCode

    Verifier Marabou -> do
      marabouProperties <- compileToMarabou loggingOptions resources spec
      Marabou.writeSpecFiles outputFile marabouProperties

    LossFunction -> do
      lossFunction <- compileToLossFunction loggingOptions spec resources
      writeLossFunctionFiles outputFile lossFunction

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToMarabou :: LoggingOptions
                 -> Resources
                 -> Text
                 -> IO MarabouSpec
compileToMarabou loggingOptions resources spec =
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckProgAndLoadResources resources spec
    Marabou.compile networkCtx prog

compileToLossFunction :: LoggingOptions
                      -> Text
                      -> Resources
                      -> IO [LExpr]
compileToLossFunction loggingOptions spec resources = do
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckProgAndLoadResources resources spec
    LossFunction.compile networkCtx prog

compileToAgda :: LoggingOptions
              -> AgdaOptions
              -> Resources
              -> Text
              -> IO (Doc a)
compileToAgda loggingOptions agdaOptions _resources spec =
  fromLoggedEitherIO loggingOptions $ do
    prog <- typeCheckProg spec
    compileProgToAgda agdaOptions prog

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

-- | Parses and type-checks the program but does
-- not load networks and datasets from disk.
typeCheckProg :: MonadCompile m => Text -> m CheckedProg
typeCheckProg txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- elabProg bnfcProg
  scopedProg  <- scopeCheck vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

-- | Parses, expands parameters and datasets, type-checks and then
-- checks the network types from disk. Used during compilation to
-- verification queries.
typeCheckProgAndLoadResources :: (MonadIO m, MonadCompile m)
                              => Resources
                              -> Text
                              -> m (NetworkContext, CheckedProg)
typeCheckProgAndLoadResources resources txt = do
  typedProg               <- typeCheckProg txt
  (networkCtx, finalProg) <- expandResources resources True typedProg
  return (networkCtx, finalProg)
