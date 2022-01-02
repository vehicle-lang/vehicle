module Vehicle.Backend
  ( module X
  , toAgda
  , toMarabou
  , toVNNLib
  ) where

import Data.Text (pack)

import Control.Monad (forM_)
import Vehicle.Prelude
import Vehicle.Compile.Prelude
import Vehicle.Backend.Prelude as X

import Vehicle.Backend.VNNLib qualified as VNNLib (compile, writeOutProperty)
import Vehicle.Backend.Marabou qualified as Marabou (compile, writeOutProperty)
import Vehicle.Backend.Agda qualified as Agda (compile, writeOutProperty)
import Vehicle.Backend.Agda (AgdaOptions(..))
import Vehicle.Compile.Error.Message
import Vehicle.NeuralNetwork

toAgda :: LogFilePath -> CompileOptions -> CheckedProg -> IO ()
toAgda logFile CompileOptions{..} prog = do
  let agdaOptions = AgdaOptions "TODO_projectFile" [pack moduleName] mempty
  agdaDoc <- fromLoggedEitherIO logFile $ Agda.compile agdaOptions prog
  Agda.writeOutProperty outputFile agdaDoc

toVNNLib :: LogFilePath -> CompileOptions -> NetworkMap -> CheckedProg -> IO ()
toVNNLib logFile CompileOptions{..} networkMap prog = do
  properties <- fromLoggedEitherIO logFile (VNNLib.compile networkMap prog)
  forM_ properties $ VNNLib.writeOutProperty outputFile

toMarabou :: LogFilePath -> CompileOptions -> NetworkMap -> CheckedProg -> IO ()
toMarabou logFile CompileOptions{..} networkMap prog = do
  properties <- fromLoggedEitherIO logFile (Marabou.compile networkMap prog)
  forM_ properties $ Marabou.writeOutProperty outputFile
