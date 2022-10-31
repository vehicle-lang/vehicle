
module Vehicle.Compile.ExpandResources
  ( expandResources
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as Map (insert, keysSet, lookup, singleton)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set (singleton)

import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Prelude

import Vehicle.Compile.Normalise
import Vehicle.Compile.Resource

--------------------------------------------------------------------------------
-- Resource monad

addParameter :: MonadExpandResources m => Name -> m ()
addParameter ident =
  tell (ResourceContext (Set.singleton ident) mempty mempty)

addInferableParameter :: MonadExpandResources m => Name -> m ()
addInferableParameter ident =
  modify (Map.insert ident Nothing)

addDataset :: MonadExpandResources m => Name -> m ()
addDataset ident =
  tell (ResourceContext mempty (Set.singleton ident) mempty)

addNetworkType :: MonadExpandResources m => Name -> NetworkType -> m ()
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
    ((prog', ResourceContext{..}), implicitParams) <-
      runStateT (runWriterT (runReaderT (processProg prog)
        (resources, expandDatasets, mempty))) mempty

    finalProg <- insertInferableParameters implicitParams prog'

    warnIfUnusedResources Parameter (Map.keysSet parameters) parameterContext
    warnIfUnusedResources Dataset   (Map.keysSet datasets)   datasetContext
    warnIfUnusedResources Network   (Map.keysSet networks)   (Map.keysSet networkContext)

    return (networkContext, finalProg)

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
processDecl d@(DefFunction _ ident _ declExpr) =
  return (Just d, Map.insert ident declExpr)
processDecl d@DefPostulate{} =
  return (Just d, id)
processDecl d@(DefResource p resourceType ident declType) = do
  (resources, expandDatasets, declCtx) <- ask
  let name = nameOf ident
  normType <- normalise declType fullNormalisationOptions
    { declContext = declCtx
    }

  case resourceType of
    InferableParameter -> do
      addInferableParameter name
      return (Just d, id)

    Parameter -> do
      addParameter name
      parameterExpr <- parseParameterValue (parameters resources) (ident, p) normType
      let result = Just $ DefFunction p ident normType parameterExpr
      return (result, Map.insert ident parameterExpr)

    Dataset -> do
      addDataset name
      if not expandDatasets
        then return (Just d, id)
        else do
          datasetExpr <- parseDataset (datasets resources) (ident, p) normType
          let result = Just $ DefFunction p ident normType datasetExpr
          return (result, Map.insert ident datasetExpr)

    Network -> do
      networkType <- getNetworkType (ident, p) normType
      addNetworkType name networkType
      return (Nothing, id)

insertInferableParameters :: MonadCompile m => InferableParameterContext -> CheckedProg -> m CheckedProg
insertInferableParameters implicitParams = traverseDecls $ \case
  r@DefFunction{}  -> return r
  r@DefPostulate{} -> return r
  DefResource p InferableParameter ident t -> do
    case Map.lookup (nameOf ident) implicitParams of
      Nothing -> compilerDeveloperError "Somehow missed the implicit parameter on the first pass"
      Just Nothing -> throwError $ InferableParameterUninferrable (ident, p)
      Just (Just (_, _, v)) -> return $ DefFunction p ident t (NatLiteral p v)
  r@DefResource{} ->
    compilerDeveloperError $ "Found unexpanded resource: " <+> pretty (identifierOf r)
