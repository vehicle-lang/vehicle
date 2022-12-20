
module Vehicle.Compile.ExpandResources
  ( expandResources
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup)
import Data.Maybe (catMaybes)
import Data.Traversable (for)

import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Normalise.NBE (whnf)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Compile.Type (getGlued)
import Vehicle.Expr.Normalised (GluedExpr (..), NormExpr, pattern VNatLiteral)

-- | Expands datasets and parameters, and attempts to infer the values of
-- inferable parameters. Also checks the resulting types of networks.
expandResources :: (MonadIO m, MonadCompile m)
                => Resources
                -> TypedProg
                -> m (NetworkContext, TypedProg)
expandResources resources@Resources{..} prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    resourcesCtx@ResourceContext{..} <-
      execStateT (runReaderT (readResourcesInProg prog) resources) emptyResourceCtx

    finalProg <-
      evalStateT (runReaderT (insertResourcesInProg prog) resourcesCtx) mempty

    warnIfUnusedResources Parameter parameters parameterContext
    warnIfUnusedResources Dataset   datasets   datasetContext
    warnIfUnusedResources Network   networks   networkContext

    return (networkContext, finalProg)

--------------------------------------------------------------------------------
-- First pass

-- | The first pass of expanding resources goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
type MonadReadResources m =
  ( MonadIO m
  , MonadExpandResources m
  )

readResourcesInProg :: MonadReadResources m => TypedProg -> m ()
readResourcesInProg (Main ds) = traverse_ readResourcesInDecl ds

readResourcesInDecl :: MonadReadResources m => TypedDecl -> m ()
readResourcesInDecl decl = case decl of
  DefFunction{}  -> return ()
  DefPostulate{} -> return ()

  DefResource p ident resourceType declType -> do
    gluedDeclType <- getGlued declType
    case resourceType of
      InferableParameter ->
        modify (noteInferableParameter ident)

      Parameter -> do
        resources <- ask
        normParameterExpr <- parseParameterValue (parameters resources) (ident, p) gluedDeclType
        let parameterExpr = mkTyped normParameterExpr
        modify (addParameter ident parameterExpr)

      Dataset -> do
        resources <- ask
        normDatasetExpr <- parseDataset (datasets resources) (ident, p) gluedDeclType
        let datasetExpr = mkTyped normDatasetExpr
        modify (addDataset ident datasetExpr)

      Network -> do
        networkType <- getNetworkType (ident, p) gluedDeclType
        modify (addNetworkType ident networkType)


mkTyped :: NormExpr -> TypedExpr
mkTyped expr = TypedExpr (Glued (unnormalise expr) expr)

--------------------------------------------------------------------------------
-- Second pass

-- | The second pass of expanding resources goes through the program finding all
-- substituting in the resources and making sure to normalise the program with
-- the new values now inserted.
type MonadInsertResources m =
  ( MonadReader ResourceContext m
  , MonadState (DeclCtx TypedExpr) m
  , MonadCompile m
  )

insertResourcesInProg :: MonadInsertResources m => TypedProg -> m TypedProg
insertResourcesInProg (Main ds) = Main . catMaybes <$> insertDecls ds

insertDecls :: MonadInsertResources m => [TypedDecl] -> m [Maybe TypedDecl]
insertDecls []       = return []
insertDecls (d : ds) = do
  norm <- normDecl d
  d'  <- insertDecl norm
  ds' <- insertDecls ds
  return $ d' : ds'

insertDecl :: MonadInsertResources m
           => TypedDecl
           -> m (Maybe TypedDecl)
insertDecl = \case
  r@DefFunction{}  -> return (Just r)
  r@DefPostulate{} -> return (Just r)
  DefResource p ident resource declType -> case resource of

    InferableParameter -> do
      implicitParams <- asks inferableParameterContext
      paramValue <- lookupValue ident implicitParams
      case paramValue of
        Nothing -> throwError $ InferableParameterUninferrable (ident, p)
        Just (_, _, v) -> do
          let normParameterExpr = VNatLiteral p v
          let parameterExpr = mkTyped normParameterExpr
          modify (Map.insert ident parameterExpr)
          return $ Just $ DefFunction p ident False declType parameterExpr

    Parameter -> do
      parameters <- asks parameterContext
      parameterExpr <- lookupValue ident parameters
      modify (Map.insert ident parameterExpr)
      return $ Just $ DefFunction p ident False declType parameterExpr

    Dataset -> do
      datasets <- asks datasetContext
      datasetExpr <- lookupValue ident datasets
      modify (Map.insert ident datasetExpr)
      return $ Just $ DefFunction p ident False declType datasetExpr

    Network ->
      return Nothing

lookupValue :: MonadCompile m => Identifier -> Map Name a -> m a
lookupValue ident ctx = case Map.lookup (nameOf ident) ctx of
  Nothing    -> compilerDeveloperError $
    "Somehow missed resource" <+> quotePretty ident <+> "on the first pass"
  Just value -> return value

normDecl :: MonadInsertResources m => TypedDecl -> m TypedDecl
normDecl decl = do
  ctx <- gets (fmap glued)
  for decl $ \(TypedExpr (Glued unnorm norm)) -> do
    -- Ugh, horrible. We really need to be able to renormalise.
    norm' <- whnf 0 ctx (unnormalise norm)
    return $ TypedExpr $ Glued unnorm norm'
