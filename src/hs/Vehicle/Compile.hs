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

import Vehicle.Backend.Prelude
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.Elaborate.Frontend as Frontend (runElab, runElabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (runTypeCheck)
import Vehicle.Compile.Normalise (normalise, defaultNormalisationOptions)
import Vehicle.Compile.Normalise.NetworkTypes (normaliseNetworkTypes)
import Vehicle.NeuralNetwork (NetworkMap)
import Vehicle.Backend.Marabou qualified as Marabou
import Vehicle.Backend.Marabou (MarabouProperty)
import Vehicle.Backend.VNNLib qualified as VNNLib
import Vehicle.Backend.VNNLib (VNNLibProperty, writeVNNLibQueryFiles)
import Vehicle.Backend.Agda

compile :: LoggingOptions -> CompileOptions -> IO ()
compile loggingOptions CompileOptions{..} = case outputTarget of
  ITP Agda -> do
    let agdaOptions = AgdaOptions "TODO_projectFile" [T.pack moduleName] mempty
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
    prog <- typeCheck contents
    compileProgToAgda agdaOptions prog

--------------------------------------------------------------------------------
-- Useful functions that apply multiple compiler passes

typeCheck :: (MonadLogger m, MonadError CompileError m)
          => Text -> m CheckedProg
typeCheck txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElab bnfcProg
  scopedProg  <- scopeCheck vehicleProg
  typedProg   <- runTypeCheck scopedProg
  return typedProg

typeCheckExpr :: (MonadLogger m, MonadError CompileError m)
              => Text -> m CheckedExpr
typeCheckExpr txt = do
  bnfcProg    <- parseVehicle txt
  vehicleProg <- runElabExpr bnfcProg
  scopedProg  <- scopeCheckClosedExpr vehicleProg
  typedProg   <- runTypeCheck scopedProg
  return typedProg

typeCheckAndExtractNetwork :: (MonadLogger m, MonadError CompileError m)
                           => Text -> m (NetworkMap, CheckedProg)
typeCheckAndExtractNetwork txt = do
  typedProg <- typeCheck txt
  normProg <- normalise defaultNormalisationOptions typedProg
  normaliseNetworkTypes normProg
