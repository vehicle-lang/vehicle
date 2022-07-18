
module Vehicle.Compile.Resource
  ( module X
  , ResourceContext
  , expandResources
  ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Set (Set)
import Data.Set qualified as Set (singleton)
import Data.Map qualified as Map (singleton, keysSet, insert)
import Data.Maybe ( catMaybes )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Dataset
import Vehicle.Compile.Resource.Parameter
import Vehicle.Compile.Resource.Network

import Vehicle.Compile.Resource.Core as X
import Vehicle.Compile.Normalise
import Vehicle.Compile.Type.VariableContext (DeclCtx)
import Data.Bifunctor

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
-- Resource monad

type MonadResource m =
  ( MonadCompile m
  , MonadIO m
  , MonadReader ((Resources, Bool), DeclCtx) m
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
  logCompilerPass MinDetail "expansion of external resources" $ do
    (prog', ResourceContext{..}) <- runWriterT (runReaderT (processProg prog) ((resources, expandDatasets), mempty))

    warnIfUnusedResources Parameter (Map.keysSet parameters) parameterContext
    warnIfUnusedResources Dataset   (Map.keysSet datasets)   datasetContext
    warnIfUnusedResources Network   (Map.keysSet networks)   (Map.keysSet networkContext)

    return (networkContext, prog')

processProg :: MonadResource m => CheckedProg -> m CheckedProg
processProg (Main ds) = Main . catMaybes <$> processDecls ds

processDecls :: MonadResource m => [CheckedDecl] -> m [Maybe CheckedDecl]
processDecls []       = return []
processDecls (d : ds) = do
  (d', alterCtx)  <- processDecl d
  ds' <- local (second alterCtx) $ processDecls ds
  return $ d' : ds'

processDecl :: MonadResource m => CheckedDecl -> m (Maybe CheckedDecl, DeclCtx -> DeclCtx)
processDecl d = case d of
  DefFunction _ _ ident declType declExpr ->
    return (Just d, Map.insert ident (declType, Just declExpr))

  DefPostulate _ ident declType ->
    return (Just d, Map.insert ident (declType, Nothing))

  DefResource ann resourceType ident declType -> do
    ((resources, expandDatasets), declCtx) <- ask
    let name = nameOf ident
    normType <- normalise declType defaultNormalisationOptions
      { declContext = declCtx
      }

    case resourceType of
      Parameter -> do
        addParameter name
        parameterExpr <- parseParameterValue (parameters resources) ann ident normType
        let result = Just $ DefFunction ann Nothing ident normType parameterExpr
        return (result, Map.insert ident (normType, Just parameterExpr))
      Dataset -> do
        addDataset name
        if not expandDatasets
          then return (Just d, id)
          else do
            datasetExpr <- parseDataset (datasets resources) ann ident normType
            let result = Just $ DefFunction ann Nothing ident normType datasetExpr
            return (result, Map.insert ident (normType, Just datasetExpr))
      Network -> do
        networkType <- getNetworkType ann ident normType
        addNetworkType name networkType
        return (Nothing, id)