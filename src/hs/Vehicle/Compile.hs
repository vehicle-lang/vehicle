module Vehicle.Compile
  ( module CompilePrelude
  , CompileOptions(..)
  , compile
  , compileToMarabou
  , compileToAgda
  , typeCheck
  , typeCheckExpr
  ) where

import Control.Monad.Except (MonadError(..))
import Data.Text as T (Text, pack)
import Data.Text.IO qualified as TIO
import System.FilePath (takeBaseName)

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.Elaborate.External as External (runElab, runElabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (typeCheck)
import Vehicle.Compile.Normalise (normalise, defaultNormalisationOptions)
import Vehicle.Compile.Normalise.NetworkTypes (normaliseNetworkTypes)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouProperty)
import Vehicle.Backend.VNNLib qualified as VNNLib
import Vehicle.Backend.VNNLib (VNNLibProperty, writeVNNLibQueryFiles)
import Vehicle.Backend.Agda
import Vehicle.Verify.VerificationStatus ( getProofCacheLocation )
import Vehicle.Resource.NeuralNetwork (NetworkMap)

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = case target of
  ITP Agda -> do
    let moduleName = pack $ maybe "" (<> ".") modulePrefix <> maybe "Spec" takeBaseName outputFile
    proofCacheLocation <- getProofCacheLocation loggingOptions proofCache
    let agdaOptions = AgdaOptions proofCacheLocation moduleName mempty
    agdaCode <- compileToAgda loggingOptions agdaOptions inputFile
    writeAgdaFile outputFile agdaCode

  Verifier Marabou -> do
    marabouProperties <- compileToMarabou loggingOptions inputFile
    Marabou.writeSpecFiles outputFile marabouProperties

  Verifier VNNLib -> do
    vnnlibProperties <- compileToVNNLib loggingOptions inputFile
    writeVNNLibQueryFiles outputFile vnnlibProperties

--------------------------------------------------------------------------------
-- Backend-specific compilation functions

compileToMarabou :: LoggingOptions -> FilePath -> IO [MarabouProperty]
compileToMarabou loggingOptions inputFile = do
  contents  <- TIO.readFile inputFile
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckAndExtractNetwork contents
    Marabou.compile networkCtx prog

compileToVNNLib :: LoggingOptions -> FilePath -> IO [VNNLibProperty]
compileToVNNLib loggingOptions inputFile = do
  contents  <- TIO.readFile inputFile
  fromLoggedEitherIO loggingOptions $ do
    (networkCtx, prog) <- typeCheckAndExtractNetwork contents
    VNNLib.compile networkCtx prog

compileToAgda :: LoggingOptions -> AgdaOptions -> FilePath -> IO (Doc a)
compileToAgda loggingOptions agdaOptions inputFile = do
  contents  <- TIO.readFile inputFile
  fromLoggedEitherIO loggingOptions $ do
    prog <- typeCheckProg contents
    compileProgToAgda agdaOptions prog

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

typeCheckProg :: (MonadLogger m, MonadError CompileError m)
              => Text -> m CheckedProg
typeCheckProg txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElab bnfcProg
  scopedProg  <- scopeCheck vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

typeCheckExpr :: (MonadLogger m, MonadError CompileError m)
              => Text -> m CheckedExpr
typeCheckExpr txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElabExpr bnfcProg
  scopedProg  <- scopeCheckClosedExpr vehicleProg
  typedProg   <- typeCheck scopedProg
  return typedProg

typeCheckAndExtractNetwork :: (MonadLogger m, MonadError CompileError m)
                           => Text -> m (NetworkMap, CheckedProg)
typeCheckAndExtractNetwork txt = do
  typedProg <- typeCheckProg txt
  normProg <- normalise defaultNormalisationOptions typedProg
  normaliseNetworkTypes normProg
