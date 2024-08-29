module Vehicle.Compile.ExpandResources
  ( expandResources,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (WriterT (..), tell)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Normalise.NBE (normaliseInEmptyEnv)
import Vehicle.Compile.Normalise.Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Prelude.Warning (CompileWarning (..))

-- | Calculates the context for external resources, reading them from disk and
-- inferring the values of inferable parameters.
expandResources ::
  forall m.
  (MonadIO m, MonadCompile m) =>
  Resources ->
  Prog Builtin ->
  m (Prog Builtin, NetworkContext, FreeCtx Builtin, ResourcesIntegrityInfo)
expandResources resources prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    ((progWithoutResources, (networkCtx, inferableParameterCtx, _explicitParameterCtx)), partialFreeCtx) <-
      runFreeContextT @m @Builtin mempty (runWriterT (runStateT (runReaderT (readResourcesInProg prog) resources) (mempty, mempty, mempty)))

    checkForUnusedResources resources partialFreeCtx

    freeCtx <- fillInInferableParameters partialFreeCtx inferableParameterCtx
    integrityInfo <- generateResourcesIntegrityInfo resources
    return (progWithoutResources, networkCtx, freeCtx, integrityInfo)

mkFunctionDefFromResource :: Provenance -> Identifier -> GluedType Builtin -> WHNFValue Builtin -> FreeCtxEntry Builtin
mkFunctionDefFromResource p ident typ normValue = do
  -- We're doing something wrong here as we only really need the value.
  -- We should really be storing the parameter values in their own environment,
  -- as values rather than as declarations in the free var context.
  let value = unnormalise 0 normValue
  let decl = DefFunction p ident mempty (unnormalised typ) value
  let normDecl = DefFunction p ident mempty (normalised typ) normValue
  (decl, normDecl)

-- | Goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
readResourcesInProg :: (MonadReadResources m) => Prog Builtin -> m (Prog Builtin)
readResourcesInProg (Main ds) = Main <$> readResourcesInDecls ds

readResourcesInDecls :: (MonadReadResources m) => [Decl Builtin] -> m [Decl Builtin]
readResourcesInDecls = \case
  [] -> return []
  decl : decls -> do
    (newDecl, newDeclEntry) <- case decl of
      DefFunction {} -> do
        entry <- mkDeclCtxEntry (Proxy @Builtin) decl
        return (Just decl, entry)
      DefAbstract p ident defType declType -> do
        normDeclType <- normaliseInEmptyEnv declType
        let gluedType = Glued declType normDeclType
        case defType of
          PostulateDef {} -> do
            entry <- mkDeclCtxEntry (Proxy @Builtin) decl
            return (Just decl, entry)
          ParameterDef sort -> case sort of
            Inferable -> do
              entry <- mkDeclCtxEntry (Proxy @Builtin) decl
              noteInferableParameter p ident gluedType
              return (Nothing, entry)
            NonInferable -> do
              parameterValues <- asks parameters
              parameterExpr <- parseParameterValue parameterValues (ident, p) gluedType
              let newDeclEntry = mkFunctionDefFromResource p ident gluedType parameterExpr
              tell (Map.singleton ident newDeclEntry)
              return (Nothing, newDeclEntry)
          DatasetDef -> do
            datasetLocations <- asks datasets
            datasetExpr <- parseDataset datasetLocations (ident, p) gluedType
            let newDeclEntry = mkFunctionDefFromResource p ident gluedType datasetExpr
            tell (Map.singleton ident newDeclEntry)
            return (Nothing, newDeclEntry)
          NetworkDef -> do
            networkLocations <- asks networks
            networkDetails <- checkNetwork networkLocations (ident, p) gluedType
            addNetworkType ident networkDetails
            let newDeclEntry = (DefAbstract p ident defType declType, DefAbstract p ident defType normDeclType)
            tell (Map.singleton ident newDeclEntry)
            entry <- mkDeclCtxEntry (Proxy @Builtin) decl
            return (Nothing, entry)

    decls' <-
      addDeclEntryToContext newDeclEntry $
        readResourcesInDecls decls

    return $ maybeToList newDecl <> decls'

checkForUnusedResources ::
  (MonadLogger m) =>
  Resources ->
  FreeCtx Builtin ->
  m ()
checkForUnusedResources Resources {..} freeCtx = do
  warnIfUnusedResources Parameter parameters freeCtx
  warnIfUnusedResources Dataset datasets freeCtx
  warnIfUnusedResources Network networks freeCtx

fillInInferableParameters ::
  (MonadCompile m) =>
  FreeCtx Builtin ->
  InferableParameterContext ->
  m (FreeCtx Builtin)
fillInInferableParameters freeCtx inferableCtx =
  foldM insertInferableParameter freeCtx (Map.assocs inferableCtx)
  where
    insertInferableParameter ::
      (MonadCompile m) =>
      FreeCtx Builtin ->
      (Identifier, (Provenance, GluedType Builtin, Maybe InferableParameterEntry)) ->
      m (FreeCtx Builtin)
    insertInferableParameter ctx (ident, (p, declType, maybeValue)) = case maybeValue of
      Nothing -> throwError $ InferableParameterUninferrable (ident, p)
      Just ((_, inferProv), _, v) -> do
        logDebug MaxDetail $ "Inferred" <+> quotePretty ident <+> "as" <+> quotePretty v
        let decl = mkFunctionDefFromResource inferProv ident declType (INatLiteral inferProv v)
        return $ Map.insert ident decl ctx

warnIfUnusedResources ::
  (MonadLogger m, HasName ident Name) =>
  ExternalResource ->
  Map Name a ->
  Map ident b ->
  m ()
warnIfUnusedResources resourceType given found = do
  when (null found) $
    logDebug MinDetail $
      "No" <+> pretty resourceType <> "s found in program"

  let givenNames = Map.keysSet given
  let foundNames = Set.map nameOf $ Map.keysSet found
  let unusedParams = givenNames `Set.difference` foundNames
  when (Set.size unusedParams > 0) $
    logWarning $
      UnusedResources resourceType unusedParams
