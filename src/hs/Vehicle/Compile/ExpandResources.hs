
module Vehicle.Compile.ExpandResources
  ( expandResources
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Set qualified as Set (singleton)
import Data.Map qualified as Map (singleton, keysSet, insert)
import Data.Maybe ( catMaybes )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Core

import Vehicle.Compile.Normalise
import Vehicle.Compile.Resource

--------------------------------------------------------------------------------
-- Resource monad

addParameter :: MonadExpandResources m => Symbol -> m ()
addParameter ident =
  tell (ResourceContext (Set.singleton ident) mempty mempty)

addImplicitParameter :: MonadExpandResources m => Symbol -> m ()
addImplicitParameter ident =
  modify (Map.insert ident Nothing)

addDataset :: MonadExpandResources m => Symbol -> m ()
addDataset ident =
  tell (ResourceContext mempty (Set.singleton ident) mempty)

addNetworkType :: MonadExpandResources m => Symbol -> NetworkType -> m ()
addNetworkType ident details =
  tell (ResourceContext mempty mempty (Map.singleton ident details))

--------------------------------------------------------------------------------
-- Traversal of program

expandResources :: (MonadIO m, MonadCompile m)
                => Resources
                -> Bool
                -> CheckedProg
                -> m (NetworkContext, CheckedProg)
expandResources resources@Resources{..} expandDatasets prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    ((prog', ResourceContext{..}), _implicitParams) <-
      runStateT (runWriterT (runReaderT (processProg prog)
        (resources, expandDatasets, mempty))) mempty

    warnIfUnusedResources Parameter (Map.keysSet parameters) parameterContext
    warnIfUnusedResources Dataset   (Map.keysSet datasets)   datasetContext
    warnIfUnusedResources Network   (Map.keysSet networks)   (Map.keysSet networkContext)

    return (networkContext, prog')

processProg :: (MonadIO m, MonadExpandResources m) => CheckedProg -> m CheckedProg
processProg (Main ds) = Main . catMaybes <$> processDecls ds

processDecls :: (MonadIO m, MonadExpandResources m) => [CheckedDecl] -> m [Maybe CheckedDecl]
processDecls []       = return []
processDecls (d : ds) = do
  (d', alterCtx)  <- processDecl d
  let updateReader (res, normDatasets, ctx) = (res, normDatasets, alterCtx ctx)
  ds' <- local updateReader $ processDecls ds
  return $ d' : ds'

processDecl :: (MonadIO m, MonadExpandResources m)
            => CheckedDecl
            -> m (Maybe CheckedDecl, DeclCtx CheckedExpr -> DeclCtx CheckedExpr)
processDecl d@(DefFunction _ _ ident _ declExpr) =
  return (Just d, Map.insert ident declExpr)
processDecl d@DefPostulate{} =
  return (Just d, id)
processDecl d@(DefResource p resourceType ident declType) = do
  (resources, expandDatasets, declCtx) <- ask
  let name = nameOf ident
  normType <- normalise declType defaultNormalisationOptions
    { declContext = declCtx
    }

  case resourceType of
    ImplicitParameter -> do
      addImplicitParameter name
      return (Just d, id)

    Parameter -> do
      addParameter name
      parameterExpr <- parseParameterValue (parameters resources) (ident, p) normType
      let result = Just $ DefFunction p Nothing ident normType parameterExpr
      return (result, Map.insert ident parameterExpr)

    Dataset -> do
      addDataset name
      if not expandDatasets
        then return (Just d, id)
        else do
          datasetExpr <- parseDataset (datasets resources) (ident, p) normType
          let result = Just $ DefFunction p Nothing ident normType datasetExpr
          return (result, Map.insert ident datasetExpr)

    Network -> do
      networkType <- getNetworkType (ident, p) normType
      addNetworkType name networkType
      return (Nothing, id)