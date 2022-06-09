
module Vehicle.Compile.Resource
  ( module X
  , ResourceContext
  , checkResourceType
  , expandResources
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Data.Map qualified as Map (singleton, keysSet)
import Data.Maybe ( catMaybes )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Dataset
import Vehicle.Compile.Resource.Parameter
import Vehicle.Compile.Resource.Network

import Vehicle.Compile.Resource.Core as X

--------------------------------------------------------------------------------
-- Resource contexts

data ResourceContext = ResourceContext
  { parameterContext :: Set Symbol
  , datasetContext   :: Set Symbol
  , networkContext   :: NetworkContext
  }

instance Semigroup ResourceContext where
  c1 <> c2 = ResourceContext
    { parameterContext = parameterContext c1 <> parameterContext c2
    , datasetContext   = datasetContext   c1 <> datasetContext   c2
    , networkContext   = networkContext   c1 <> networkContext   c2
    }

instance Monoid ResourceContext where
  mempty = ResourceContext mempty mempty mempty

--------------------------------------------------------------------------------
-- Type-checking

checkResourceType :: MonadCompile m
                  => ResourceType
                  -> Provenance
                  -> Identifier
                  -> CheckedExpr
                  -> m ()
checkResourceType Parameter = checkParameterType
checkResourceType Dataset   = checkDatasetType
checkResourceType Network   = checkNetworkType

--------------------------------------------------------------------------------
-- Resource monad

type MonadResource m =
  ( MonadCompile m
  , MonadIO m
  , MonadReader (Resources, Bool) m
  , MonadWriter ResourceContext m
  )

addParameter :: MonadResource m => Symbol -> m ()
addParameter ident =
  tell (ResourceContext (Set.singleton ident) mempty mempty)

addDataset :: MonadResource m => Symbol -> m ()
addDataset ident =
  tell (ResourceContext mempty (Set.singleton ident) mempty)

addNetworkType :: MonadResource m => Symbol -> NetworkType -> m ()
addNetworkType ident details =
  tell (ResourceContext mempty mempty (Map.singleton ident details))

--------------------------------------------------------------------------------
-- Traversal of program

expandResources :: (MonadCompile m, MonadIO m)
                => Resources
                -> Bool
                -> CheckedProg
                -> m (NetworkContext, CheckedProg)
expandResources resources@Resources{..} expandDatasets prog =
  logCompilerPass "expansion of external resources" $ do
    (prog', ResourceContext{..}) <- runWriterT (runReaderT (processProg prog) (resources, expandDatasets))

    warnIfUnusedResources Parameter (Map.keysSet parameters) parameterContext
    warnIfUnusedResources Dataset   (Map.keysSet datasets)   datasetContext
    warnIfUnusedResources Network   (Map.keysSet networks)   (Map.keysSet networkContext)

    return (networkContext, prog')

processProg :: MonadResource m => CheckedProg -> m CheckedProg
processProg (Main ds) = Main <$> processDecls ds

processDecls :: MonadResource m => [CheckedDecl] -> m [CheckedDecl]
processDecls ds = catMaybes <$> traverse processDecl ds

processDecl :: MonadResource m => CheckedDecl -> m (Maybe CheckedDecl)
processDecl d@DefFunction{} = return $ Just d
processDecl d@(DefResource ann resourceType ident declType) = do
  (resources, expandDatasets) <- ask
  let name = nameOf ident
  case resourceType of
    Parameter -> do
      addParameter name
      parameterExpr <- parseParameterValue (parameters resources) ann ident declType
      return $ Just $ DefFunction ann Nothing ident declType parameterExpr
    Dataset -> do
      addDataset name
      if not expandDatasets
        then return $ Just d
        else do
          datasetExpr <- parseDataset (datasets resources) ann ident declType
          return $ Just $ DefFunction ann Nothing ident declType datasetExpr
    Network -> do
      networkType <- extractNetworkType ann ident declType
      addNetworkType name networkType
      return Nothing