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
import Data.Set qualified as Set
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Normalise.NBE (normaliseInEmptyEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Prelude.Warning (CompileWarning (..))

-- | Calculates the context for external resources, reading them from disk and
-- inferring the values of inferable parameters.
expandResources ::
  forall m.
  (MonadIO m, MonadCompile m) =>
  Resources ->
  Prog Ix Builtin ->
  m (Prog Ix Builtin, NetworkContext, FreeCtx Builtin, ResourcesIntegrityInfo)
expandResources resources prog =
  logCompilerPass MinDetail "expansion of external resources" $ do
    ((progWithoutResources, (networkCtx, inferableParameterCtx, _explicitParameterCtx)), partialFreeCtx) <-
      runFreeContextT @m @Builtin mempty (runWriterT (runStateT (runReaderT (readResourcesInProg prog) resources) (mempty, mempty, mempty)))

    checkForUnusedResources resources partialFreeCtx

    freeCtx <- fillInInferableParameters partialFreeCtx inferableParameterCtx
    integrityInfo <- generateResourcesIntegrityInfo resources
    return (progWithoutResources, networkCtx, freeCtx, integrityInfo)

mkFunctionDefFromResource :: Provenance -> Identifier -> WHNFValue Builtin -> (WHNFDecl Builtin, Type Ix Builtin)
mkFunctionDefFromResource p ident value = do
  -- We're doing something wrong here as we only really need the value.
  -- We should really be storing the parameter values in their own environment,
  -- as values rather than as declarations in the free var context.
  let unnormType = Builtin mempty (BuiltinType Unit)
  (DefFunction p ident mempty VUnitType value, unnormType)

addFunctionDefFromResource :: (MonadReadResources m) => Provenance -> Identifier -> WHNFValue Builtin -> m ()
addFunctionDefFromResource p ident value = do
  noteExplicitParameter ident value
  let decl = mkFunctionDefFromResource p ident value
  tell (Map.singleton ident decl)

-- | Goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
readResourcesInProg :: (MonadReadResources m) => Prog Ix Builtin -> m (Prog Ix Builtin)
readResourcesInProg (Main ds) = Main <$> readResourcesInDecls ds

readResourcesInDecls :: (MonadReadResources m) => [Decl Ix Builtin] -> m [Decl Ix Builtin]
readResourcesInDecls = \case
  [] -> return []
  decl : decls -> addDeclToContext decl $ do
    maybeDecl <- case decl of
      DefFunction {} -> return $ Just decl
      DefAbstract p ident defType declType -> do
        normalisedType <- normaliseInEmptyEnv declType
        let gluedType = Glued declType normalisedType
        case defType of
          ParameterDef sort -> case sort of
            Inferable -> do
              noteInferableParameter p ident
              return Nothing
            NonInferable -> do
              parameterValues <- asks parameters
              parameterExpr <- parseParameterValue parameterValues (ident, p) gluedType
              addFunctionDefFromResource p ident parameterExpr
              return Nothing
          DatasetDef -> do
            datasetLocations <- asks datasets
            datasetExpr <- parseDataset datasetLocations (ident, p) gluedType
            addFunctionDefFromResource p ident datasetExpr
            return Nothing
          NetworkDef -> do
            networkLocations <- asks networks
            networkDetails <- checkNetwork networkLocations (ident, p) gluedType
            addNetworkType ident networkDetails
            tell (Map.singleton ident (DefAbstract p ident defType normalisedType, declType))
            return Nothing
          PostulateDef ->
            return (Just decl)

    decls' <- readResourcesInDecls decls
    return $ maybeToList maybeDecl <> decls'

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
      (Identifier, Either Provenance InferableParameterEntry) ->
      m (FreeCtx Builtin)
    insertInferableParameter ctx (ident, maybeValue) = case maybeValue of
      Left p -> throwError $ InferableParameterUninferrable (ident, p)
      Right ((_, p), _, v) -> do
        logDebug MaxDetail $ "Inferred" <+> quotePretty ident <+> "as" <+> quotePretty v
        let decl = mkFunctionDefFromResource p ident (VNatLiteral v)
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
