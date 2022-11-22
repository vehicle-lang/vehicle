
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

import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Resource
import Vehicle.Expr.Normalised (GluedDecl, GluedExpr (..), GluedProg)

-- | Expands datasets and parameters, and attempts to infer the values of
-- inferable parameters. Also checks the resulting types of networks.
expandResources :: (MonadIO m, MonadCompile m)
                => Resources
                -> Bool
                -> GluedProg
                -> m (NetworkContext, CheckedProg)
expandResources resources@Resources{..} expandDatasets prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    ((prog', ResourceContext{..}), implicitParams) <-
      runStateT (runWriterT (runReaderT (processProg prog)
        (resources, expandDatasets))) mempty

    finalProg <- insertInferableParameters implicitParams prog'

    warnIfUnusedResources Parameter (Map.keysSet parameters) parameterContext
    warnIfUnusedResources Dataset   (Map.keysSet datasets)   datasetContext
    warnIfUnusedResources Network   (Map.keysSet networks)   (Map.keysSet networkContext)

    return (networkContext, finalProg)

--------------------------------------------------------------------------------
-- Traversal of program

processProg :: (MonadIO m, MonadExpandResources m) => GluedProg -> m CheckedProg
processProg (Main ds) = Main . catMaybes <$> processDecls ds

processDecls :: (MonadIO m, MonadExpandResources m) => [GluedDecl] -> m [Maybe CheckedDecl]
processDecls []       = return []
processDecls (d : ds) = do
  d'  <- processDecl d
  ds' <- processDecls ds
  return $ d' : ds'

processDecl :: (MonadIO m, MonadExpandResources m)
            => GluedDecl
            -> m (Maybe CheckedDecl)
processDecl decl = case decl of
  DefFunction{}  -> return (Just $ fmap unnormalised decl)
  DefPostulate{} -> return (Just $ fmap unnormalised decl)

  DefResource p resourceType ident declType -> do
    (resources, expandDatasets) <- ask
    let name = nameOf ident

    case resourceType of
      InferableParameter -> do
        addInferableParameter name
        return (Just $ fmap unnormalised decl)

      Parameter -> do
        addParameter name
        parameterExpr <- parseParameterValue (parameters resources) (ident, p) declType
        let result = Just $ DefFunction p ident (unnormalised declType) (unnormalise parameterExpr)
        return result

      Dataset -> do
        addDataset name
        if not expandDatasets
          then return (Just $ fmap unnormalised decl)
          else do
            datasetExpr <- parseDataset (datasets resources) (ident, p) declType
            let result = Just $ DefFunction p ident (unnormalised declType) (unnormalise datasetExpr)
            return result

      Network -> do
        networkType <- getNetworkType (ident, p) declType
        addNetworkType name networkType
        return Nothing

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
