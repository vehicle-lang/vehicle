module Vehicle.Compile.ExpandResources
  ( expandResources,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.ExpandResources.Dataset
import Vehicle.Compile.ExpandResources.Network
import Vehicle.Compile.ExpandResources.Parameter
import Vehicle.Compile.Normalise.NBE (evalInEmptyEnv)
import Vehicle.Compile.Normalise.Quote
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Free.Context
import Vehicle.Prelude.Warning (CompileWarning (..))

-- | Calculates the context for external resources, reading them from disk and
-- inferring the values of inferable parameters.
expandResources ::
  forall m.
  (MonadIO m, MonadCompile m) =>
  Resources ->
  Prog Builtin ->
  m (Prog Builtin, NetworkContext, ResourcesIntegrityInfo, [MissingResource], [UninferableParameter])
expandResources resources prog =
  logCompilerSection2 MinDetail "expansion of external resources" $ do
    logDebug MidDetail $ "Provided resources:" <> lineIndent (pretty resources)

    (progWithoutResources, ExpandResourcesState {..}) <- runExpandResourcesT resources (readResourcesInProg prog)

    checkForUnusedResources unusedResources

    (finalProg, uninferableParameters) <- fillInInferableParametersInProg inferableParamCtx progWithoutResources
    integrityInfo <- generateResourcesIntegrityInfo resources
    return (finalProg, networkCtx, integrityInfo, missingResources, uninferableParameters)

mkFunctionDefFromResource :: Provenance -> Identifier -> Type Builtin -> Value Builtin -> Decl Builtin
mkFunctionDefFromResource p ident typ normValue = do
  let sort = FunctionDecl 0 Nothing
  let body = unnormalise 0 normValue
  DefFunction p ident sort typ body

--------------------------------------------------------------------------------
-- 1st pass - reading in resources

-- | Goes through the program finding all
-- the resources, comparing the data against the type in the spec, and making
-- note of the values for implicit parameters.
readResourcesInProg :: (MonadIO m, MonadExpandResources m) => Prog Builtin -> m (Prog Builtin)
readResourcesInProg (Main ds) = Main <$> readResourcesInDecls ds

readResourcesInDecls :: (MonadIO m, MonadExpandResources m) => [Decl Builtin] -> m [Decl Builtin]
readResourcesInDecls = \case
  [] -> return []
  decl : decls -> do
    newDecl <- readResourceInDecl decl
    decls' <- addDeclToContext newDecl $ readResourcesInDecls decls
    return $ newDecl : decls'

readResourceInDecl :: (MonadIO m, MonadExpandResources m) => Decl Builtin -> m (Decl Builtin)
readResourceInDecl decl = case decl of
  DefAbstract p ident defType declType -> do
    normDeclType <- evalInEmptyEnv declType
    let gluedType = Glued declType normDeclType
    maybeNewDecl <- case defType of
      BuiltinDef {} -> return Nothing
      ParameterDef sort -> readParameter p ident gluedType sort
      DatasetDef -> readDataset p ident gluedType
      NetworkDef -> readNetwork p ident gluedType
      DynamicsDef -> readNetwork p ident gluedType
    return $ fromMaybe decl maybeNewDecl
  _ -> return decl

readParameter ::
  (MonadIO m, MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  GluedType Builtin ->
  ParameterSort ->
  m (Maybe (Decl Builtin))
readParameter p ident gluedType = \case
  Inferable -> do
    noteInferableParameter p ident gluedType
    return Nothing
  NonInferable -> do
    maybeParameterString <- findNonInferableParameterValue p ident
    forM maybeParameterString $ \parameterString -> do
      parameterValue <- parseParameterValue (ident, p) gluedType parameterString
      noteNonInferableParameter ident parameterValue
      return $ mkFunctionDefFromResource p ident (unnormalised gluedType) parameterValue

readDataset ::
  (MonadIO m, MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  GluedType Builtin ->
  m (Maybe (Decl Builtin))
readDataset p ident gluedType = do
  maybeFile <- findDatasetValue p ident
  forM maybeFile $ \file -> do
    datasetExpr <- parseDataset (ident, p) gluedType file
    return $ mkFunctionDefFromResource p ident (unnormalised gluedType) datasetExpr

readNetwork ::
  (MonadIO m, MonadExpandResources m) =>
  Provenance ->
  Identifier ->
  GluedType Builtin ->
  m (Maybe (Decl Builtin))
readNetwork p ident gluedType = do
  maybeFile <- findNetworkValue p ident
  case maybeFile of
    Nothing -> return Nothing
    Just file -> do
      networkType <- checkNetwork (ident, p) gluedType file
      noteNetwork ident networkType
      return Nothing

--------------------------------------------------------------------------------
-- 2nd pass - reading in resources

fillInInferableParametersInProg ::
  (MonadCompile m) =>
  InferableParameterContext ->
  Prog Builtin ->
  m (Prog Builtin, [UninferableParameter])
fillInInferableParametersInProg ctx prog =
  runWriterT (traverseDecls (fillInInferableParametersInDecl ctx) prog)

fillInInferableParametersInDecl ::
  (MonadCompile m, MonadWriter [UninferableParameter] m) =>
  InferableParameterContext ->
  Decl Builtin ->
  m (Decl Builtin)
fillInInferableParametersInDecl ctx decl = case decl of
  DefAbstract p ident (ParameterDef Inferable) declType -> do
    case Map.lookup ident ctx of
      Just (_, _, Just ((_, inferProv), _, v)) -> do
        logDebug MaxDetail $ "Inferred" <+> quotePretty ident <+> "as" <+> quotePretty v
        return $ mkFunctionDefFromResource inferProv ident declType (INatLiteral v)
      _ -> do
        tell [(ident, p)]
        return decl
  _ -> return decl

--------------------------------------------------------------------------------
-- Warnings

checkForUnusedResources ::
  (MonadLogger m) =>
  Resources ->
  m ()
checkForUnusedResources Resources {..} = do
  warnIfUnusedResources Parameter parameters
  warnIfUnusedResources Dataset datasets
  warnIfUnusedResources Network networks

warnIfUnusedResources ::
  (MonadLogger m) =>
  ExternalResource ->
  Map Name b ->
  m ()
warnIfUnusedResources resourceType notFound = do
  let unusedNames = Map.keysSet notFound
  when (Set.size unusedNames > 0) $
    logWarning $
      UnusedResources resourceType unusedNames
