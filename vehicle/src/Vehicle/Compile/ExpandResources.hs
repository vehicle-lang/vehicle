module Vehicle.Compile.ExpandResources
  ( expandResources,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (MonadWriter, WriterT (..), tell)
import Data.Data (Proxy (..))
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
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Compile.Warning (CompileWarning (..))
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.Normalised

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
    ((progWithoutResources, (networkCtx, inferableParameterCtx)), partialDeclCtx) <-
      runFreeContextT @m @Builtin mempty (runWriterT (runStateT (runReaderT (readResourcesInProg prog) resources) (mempty, mempty)))

    checkForUnusedResources resources partialDeclCtx

    declCtx <- fillInInferableParameters partialDeclCtx inferableParameterCtx
    integrityInfo <- generateResourcesIntegrityInfo resources
    return (progWithoutResources, networkCtx, declCtx, integrityInfo)

mkFunctionDefFromResource :: Provenance -> Identifier -> Value Builtin -> GluedDecl Builtin
mkFunctionDefFromResource p ident value = do
  -- This is a hack. The type is only every used for its arity, so this is okay.
  let unitType = BuiltinType Unit
  -- We're doing something wrong here as we only really need the value
  let gluedType = Glued (Builtin mempty unitType) (VBuiltin unitType [])
  let gluedExpr = Glued (Builtin mempty (BuiltinConstructor LUnit)) value
  DefFunction p ident mempty gluedType gluedExpr

addFunctionDefFromResource :: (MonadReadResources m) => Provenance -> Identifier -> Value Builtin -> m ()
addFunctionDefFromResource p ident value = do
  let decl = mkFunctionDefFromResource p ident value
  tell (Map.singleton ident decl)

type MonadReadResources m =
  ( MonadIO m,
    MonadExpandResources m,
    MonadFreeContext Builtin m,
    MonadWriter (FreeCtx Builtin) m
  )

-- | Goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
readResourcesInProg :: (MonadReadResources m) => Prog Ix Builtin -> m (Prog Ix Builtin)
readResourcesInProg (Main ds) = Main <$> readResourcesInDecls ds

readResourcesInDecls :: forall m. (MonadReadResources m) => [Decl Ix Builtin] -> m [Decl Ix Builtin]
readResourcesInDecls = \case
  [] -> return []
  decl : decls -> addDeclToContext decl $ do
    maybeDecl <- case decl of
      DefFunction {} -> return $ Just decl
      DefAbstract p ident defType _declType -> do
        gluedDecl <- getDecl @Builtin @m (Proxy @Builtin) "expandResources" (identifierOf decl)
        let gluedType = typeOf gluedDecl
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
            tell (Map.singleton ident (DefAbstract p ident NetworkDef gluedType))
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
checkForUnusedResources Resources {..} declCtx = do
  warnIfUnusedResources Parameter parameters declCtx
  warnIfUnusedResources Dataset datasets declCtx
  warnIfUnusedResources Network networks declCtx

fillInInferableParameters ::
  (MonadCompile m) =>
  FreeCtx Builtin ->
  InferableParameterContext ->
  m (FreeCtx Builtin)
fillInInferableParameters declCtx inferableCtx =
  foldM insertInferableParameter declCtx (Map.assocs inferableCtx)
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
      pretty $
        UnusedResource resourceType unusedParams
