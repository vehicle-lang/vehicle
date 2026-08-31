module Vehicle.Export where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Vehicle.Backend.Prelude
import Vehicle.Compile
import Vehicle.Prelude (DeclarationNames)
import Vehicle.Prelude.IO (MonadStdIO, fatalError)
import Vehicle.Prelude.Logging
import Vehicle.Resource
import Vehicle.Verify.Specification (SpecificationCacheIndex (..))
import Vehicle.Verify.Specification.IO

data ExportOptions = ExportOptions
  { target :: InteractiveTheoremProverID,
    specification :: Maybe FilePath,
    declarationsToCompile :: DeclarationNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    output :: Maybe FilePath,
    moduleName :: Maybe String,
    verificationCache :: Maybe FilePath,
    constructiveReals :: Bool
  }
  deriving (Eq, Show)

export :: (MonadStdIO IO) => LoggingSettings -> OutputAsJSON -> ExportOptions -> IO ()
export loggingSettings outputAsJSON ExportOptions {..} = do
  itpOptions <-
    case (specification, verificationCache) of
      (Just spec, Nothing) ->
        return $
          mkITPOptions
            spec
            declarationsToCompile
            networkLocations
            datasetLocations
            parameterValues
            Nothing
      (Nothing, Just _)
        | not (null declarationsToCompile)
            || not (Map.null networkLocations)
            || not (Map.null datasetLocations)
            || not (Map.null parameterValues) ->
            fatalError
              "`--declaration`, `--network`, `--dataset`, and `--parameter` may only be used with `--specification`, not `--cache`."
      (Nothing, Just cache) -> do
        let cacheIndexFile = specificationCacheIndexFileName cache
        SpecificationCacheIndex {..} <- liftIO $ readSpecificationCacheIndex cacheIndexFile
        let spec = filePath $ specificationSummary resourcesIntegrityInfo
        let resources = reparseResources resourcesIntegrityInfo
        return $
          mkITPOptions
            spec
            (fmap fst properties)
            (networks resources)
            (datasets resources)
            (parameters resources)
            (Just cache)
      (Nothing, Nothing) ->
        fatalError "`vehicle export` requires exactly one of `--cache` or `--specification`."
      (Just _, Just _) ->
        fatalError "`--cache` and `--specification` are mutually exclusive; provide exactly one."

  compile loggingSettings outputAsJSON $ ITPTarget itpOptions
  where
    mkITPOptions spec decls networkLocs datasetLocs parameterVals cache =
      ITPOptions
        { itp = target,
          specification = spec,
          declarationsToCompile = decls,
          networkLocations = networkLocs,
          datasetLocations = datasetLocs,
          parameterValues = parameterVals,
          outputFile = output,
          moduleName = moduleName,
          verificationCache = cache,
          constructiveReals = constructiveReals
        }
