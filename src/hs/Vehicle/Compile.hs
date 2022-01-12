module Vehicle.Compile
  ( module CompilePrelude
  , CompileOptions(..)
  , compile
  , typeCheck
  , typeCheckExpr
  ) where

import Control.Monad.Except (MonadError(..))
import Data.Text as T (Text)
import Data.Text.IO qualified as TIO

import Vehicle.Backend
import Vehicle.Compile.Prelude as CompilePrelude
import Vehicle.Compile.Error
import Vehicle.Compile.Error.Message
import Vehicle.Compile.Parse
import Vehicle.Compile.Elaborate.Frontend as Frontend (runElab, runElabExpr)
import Vehicle.Compile.Scope (scopeCheck, scopeCheckClosedExpr)
import Vehicle.Compile.Type (runTypeCheck)
import Vehicle.Compile.Normalise (normalise, defaultNormalisationOptions)
import Vehicle.Compile.Normalise.NetworkTypes (normaliseNetworkTypes)

compile :: OutputFilePaths -> CompileOptions -> IO ()
compile outputFiles opts@CompileOptions{..} = do
  contents  <- TIO.readFile inputFile
  typedProg <- fromLoggedEitherIO outputFiles (typeCheck contents)

  -- Compile to requested backend
  case outputTarget of
    ITP itp ->
      case itp of
        Agda -> toAgda outputFiles opts typedProg

    (Verifier verifier) -> do
      normProg <- fromLoggedEitherIO outputFiles $ normalise defaultNormalisationOptions typedProg
      (networkMap, networkedProg) <- fromLoggedEitherIO outputFiles $ normaliseNetworkTypes normProg
      case verifier of
        VNNLib  -> toVNNLib  outputFiles opts networkMap networkedProg
        Marabou -> toMarabou outputFiles opts networkMap networkedProg

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

