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

toAgda :: OutputFilePaths -> CompileOptions -> CheckedProg -> IO ()
toAgda outputFiles CompileOptions{..} prog = do
  let agdaOptions = AgdaOptions "TODO_projectFile" [pack moduleName] mempty
  agdaDoc <- fromLoggedEitherIO outputFiles $ Agda.compile agdaOptions prog
  Agda.writeOutProperty outputFile agdaDoc

toVNNLib :: OutputFilePaths -> CompileOptions -> NetworkMap -> CheckedProg -> IO ()
toVNNLib outputFiles CompileOptions{..} networkMap prog = do
  properties <- fromLoggedEitherIO outputFiles (VNNLib.compile networkMap prog)
  forM_ properties $ VNNLib.writeOutProperty outputFile

toMarabou :: OutputFilePaths -> CompileOptions -> NetworkMap -> CheckedProg -> IO ()
toMarabou outputFiles CompileOptions{..} networkMap prog = do
  properties <- fromLoggedEitherIO outputFiles (Marabou.compile networkMap prog)
  forM_ properties $ Marabou.writeOutProperty outputFile
