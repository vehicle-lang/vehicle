module Vehicle.Compile.ExpandResources
  ( expandResources,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map (insert, lookup)
import Data.Traversable (for)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Normalise.NBE (runNormT, whnf)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

-- | Expands datasets and parameters, and attempts to infer the values of
-- inferable parameters. Also checks the resulting types of networks.
expandResources ::
  (MonadIO m, MonadCompile m) =>
  Resources ->
  StandardGluedProg ->
  m (NetworkContext, StandardGluedProg)
expandResources resources@Resources {..} prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    resourcesCtx@ResourceContext {..} <-
      execStateT (runReaderT (readResourcesInProg prog) resources) emptyResourceCtx

    finalProg <-
      evalStateT (runReaderT (insertResourcesInProg prog) resourcesCtx) mempty

    warnIfUnusedResources Parameter parameters parameterContext
    warnIfUnusedResources Dataset datasets datasetContext
    warnIfUnusedResources Network networks networkContext

    return (networkContext, finalProg)

--------------------------------------------------------------------------------
-- First pass

-- | The first pass of expanding resources goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
type MonadReadResources m =
  ( MonadIO m,
    MonadExpandResources m
  )

readResourcesInProg :: (MonadReadResources m) => StandardGluedProg -> m ()
readResourcesInProg (Main ds) = traverse_ readResourcesInDecl ds

readResourcesInDecl :: (MonadReadResources m) => StandardGluedDecl -> m ()
readResourcesInDecl decl = case decl of
  DefFunction {} -> return ()
  DefAbstract p ident defType declType ->
    case defType of
      InferableParameterDef ->
        modify (noteInferableParameter ident)
      ParameterDef -> do
        parameterValues <- asks parameters
        normParameterExpr <- parseParameterValue parameterValues (ident, p) declType
        let parameterExpr = mkTyped normParameterExpr
        modify (addParameter ident parameterExpr)
      DatasetDef -> do
        datasetLocations <- asks datasets
        normDatasetExpr <- parseDataset datasetLocations (ident, p) declType
        let datasetExpr = mkTyped normDatasetExpr
        modify (addDataset ident datasetExpr)
      NetworkDef -> do
        networkLocations <- asks networks
        networkDetails <- checkNetwork networkLocations (ident, p) declType
        modify (addNetworkType ident networkDetails)
      PostulateDef -> return ()

mkTyped :: StandardNormExpr -> StandardGluedExpr
mkTyped expr = Glued (unnormalise 0 expr) expr

--------------------------------------------------------------------------------
-- Second pass

-- | The second pass of expanding resources goes through the program finding all
-- substituting in the resources and making sure to normalise the program with
-- the new values now inserted.
type MonadInsertResources m =
  ( MonadReader ResourceContext m,
    MonadState (DeclCtx StandardGluedExpr) m,
    MonadCompile m
  )

insertResourcesInProg :: (MonadInsertResources m) => StandardGluedProg -> m StandardGluedProg
insertResourcesInProg (Main ds) = Main <$> insertDecls ds

insertDecls :: (MonadInsertResources m) => [StandardGluedDecl] -> m [StandardGluedDecl]
insertDecls [] = return []
insertDecls (d : ds) = do
  norm <- normDecl d
  maybeDecl <- insertDecl norm
  case maybeDecl of
    Nothing -> insertDecls ds
    Just decl -> do
      case bodyOf decl of
        Nothing -> return ()
        Just body -> modify (Map.insert (identifierOf decl) body)
      ds' <- insertDecls ds
      return $ decl : ds'

insertDecl ::
  (MonadInsertResources m) =>
  StandardGluedDecl ->
  m (Maybe StandardGluedDecl)
insertDecl d = case d of
  DefFunction {} -> return (Just d)
  DefAbstract p ident defType declType -> case defType of
    InferableParameterDef -> do
      implicitParams <- asks inferableParameterContext
      paramValue <- lookupValue ident implicitParams
      case paramValue of
        Nothing -> throwError $ InferableParameterUninferrable (ident, p)
        Just (_, _, v) -> do
          let normParameterExpr = VNatLiteral v
          let parameterExpr = mkTyped normParameterExpr
          modify (Map.insert ident parameterExpr)
          return $ Just $ DefFunction p ident [] declType parameterExpr
    ParameterDef -> do
      parameters <- asks parameterContext
      parameterExpr <- lookupValue ident parameters
      modify (Map.insert ident parameterExpr)
      return $ Just $ DefFunction p ident [] declType parameterExpr
    DatasetDef -> do
      datasets <- asks datasetContext
      datasetExpr <- lookupValue ident datasets
      modify (Map.insert ident datasetExpr)
      return $ Just $ DefFunction p ident [] declType datasetExpr
    NetworkDef ->
      return Nothing
    PostulateDef ->
      return $ Just d

lookupValue :: (MonadCompile m) => Identifier -> Map Name a -> m a
lookupValue ident ctx = case Map.lookup (nameOf ident) ctx of
  Nothing ->
    compilerDeveloperError $
      "Somehow missed resource" <+> quotePretty ident <+> "on the first pass"
  Just value -> return value

normDecl :: (MonadInsertResources m) => StandardGluedDecl -> m StandardGluedDecl
normDecl decl = do
  ctx <- gets (fmap normalised)
  for decl $ \(Glued unnorm norm) -> do
    -- Ugh, horrible. We really need to be able to renormalise.
    norm' <- runNormT ctx mempty (whnf mempty (unnormalise 0 norm))
    return $ Glued unnorm norm'
