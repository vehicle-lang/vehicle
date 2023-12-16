module Vehicle.Backend.Queries
  ( compileToQueries,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (isNothing, maybeToList)
import Data.Proxy (Proxy (..))
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Queries.Error
import Vehicle.Backend.Queries.PostProcessing (compileQueryToFormat)
import Vehicle.Backend.Queries.UserVariableElimination (eliminateUserVariables)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat
import Vehicle.Verify.Specification
import Vehicle.Verify.Specification.IO

--------------------------------------------------------------------------------
-- Compilation to individual queries

currentPass :: Doc a
currentPass = "compilation of properties"

-- | Compiles the provided program to individual queries suitable for a
-- verifier and outputs them. We need to output them as they are generated as
-- otherwise storing all the queries can result in an out-of-memory errors.
compileToQueries ::
  (MonadStdIO m, MonadCompile m) =>
  QueryFormat ->
  Prog Ix Builtin ->
  Resources ->
  Maybe FilePath ->
  m ()
compileToQueries queryFormat typedProg resources maybeVerificationFolder =
  logCompilerPass MinDetail currentPass $ do
    -- Create the verification folder if required.
    case maybeVerificationFolder of
      Nothing -> return ()
      Just folder -> liftIO $ createDirectoryIfMissing True folder

    -- Expand out the external resources in the specification (datasets, networks etc.)
    (Main resourceFreeDecls, networkCtx, freeCtx, integrityInfo) <-
      expandResources resources typedProg

    -- Perform the actual compilation to queries
    properties <-
      runFreeContextT freeCtx $
        compileDecls typedProg queryFormat networkCtx 0 resourceFreeDecls maybeVerificationFolder

    -- Check that there were actually properties in the specification.
    when (null properties) $ do
      throwError NoPropertiesFound

    case maybeVerificationFolder of
      Nothing -> return ()
      Just folder -> do
        let verificationPlan = SpecificationCacheIndex integrityInfo properties
        writeSpecificationCache folder verificationPlan

--------------------------------------------------------------------------------
-- Getting properties

compileDecls ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Ix Builtin ->
  QueryFormat ->
  NetworkContext ->
  PropertyID ->
  [Decl Ix Builtin] ->
  Maybe FilePath ->
  m [(Name, MultiProperty ())]
compileDecls _ _ _ _ [] _ = return []
compileDecls prog queryFormat networkCtx propertyID (d : ds) outputLocation = do
  property <- case d of
    DefFunction p ident anns _ body
      | isProperty anns -> do
          unalteredFreeContext <- getFreeCtx (Proxy @Builtin)
          let propertyData = (queryFormat, unalteredFreeContext, networkCtx, (ident, p), propertyID)
          locallyAdjustCtx (Proxy @Builtin) convertVectorOpsToPostulates $ do
            Just <$> compilePropertyDecl prog propertyData body outputLocation
    _ -> return Nothing

  addDeclToContext d $ do
    let newPropertyID = if isNothing property then propertyID else propertyID + 1
    properties <- compileDecls prog queryFormat networkCtx newPropertyID ds outputLocation
    return $ maybeToList property ++ properties

type MultiPropertyMetaData =
  ( QueryFormat,
    FreeCtx Builtin,
    NetworkContext,
    DeclProvenance,
    Int
  )

updateMetaData :: MultiPropertyMetaData -> TensorIndices -> PropertyMetaData
updateMetaData (queryFormat, unalteredFreeCtx, networkCtx, declProvenance, propertyID) indices =
  PropertyMetaData
    { networkCtx = networkCtx,
      queryFormat = queryFormat,
      unalteredFreeContext = unalteredFreeCtx,
      propertyProvenance = declProvenance,
      propertyAddress = PropertyAddress propertyID (nameOf $ fst declProvenance) indices
    }

compilePropertyDecl ::
  (MonadStdIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Ix Builtin ->
  MultiPropertyMetaData ->
  Expr Ix Builtin ->
  Maybe FilePath ->
  m (Name, MultiProperty ())
compilePropertyDecl prog propertyData@(_, _, _, declProv@(ident, _), _) expr outputLocation = do
  logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
    normalisedExpr <- normaliseInEmptyEnv expr
    multiProperty <-
      compileMultiProperty propertyData outputLocation normalisedExpr
        `catchError` handlePropertyCompileError prog propertyData
    return (nameOf (fst declProv), multiProperty)

handlePropertyCompileError :: (MonadCompile m) => Prog Ix Builtin -> MultiPropertyMetaData -> CompileError -> m a
handlePropertyCompileError prog (queryFormat, _, _, declProv, _) e = case e of
  UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity (queryFormatID queryFormat) prog declProv
  UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers (queryFormatID queryFormat) prog declProv
  _ -> throwError e

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadStdIO m, MonadFreeContext QueryBuiltin m) =>
  MultiPropertyMetaData ->
  Maybe FilePath ->
  WHNFValue Builtin ->
  m (MultiProperty ())
compileMultiProperty multiPropertyMetaData outputLocation = go []
  where
    go :: TensorIndices -> WHNFValue Builtin -> m (MultiProperty ())
    go indices expr = case expr of
      VVecLiteral es -> do
        let es' = zip [0 :: QueryID ..] es
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) (argExpr e)) es'
      _ -> do
        let propertyMetaData@PropertyMetaData {..} = updateMetaData multiPropertyMetaData indices
        let logFunction =
              if null indices
                then id
                else logCompilerPass MinDetail ("property" <+> quotePretty propertyAddress)
        flip runReaderT propertyMetaData $ do
          logFunction $ do
            compileSingleProperty outputLocation expr
            return $ SingleProperty propertyAddress ()

-- Compiles an individual property
compileSingleProperty ::
  (MonadPropertyStructure m, MonadStdIO m) =>
  Maybe FilePath ->
  WHNFValue Builtin ->
  m ()
compileSingleProperty outputLocation expr = do
  queries <- eliminateUserVariables expr

  PropertyMetaData {..} <- ask
  -- Warn if trivial.
  case queries of
    Trivial status -> logWarning (TrivialProperty propertyAddress status)
    _ -> return ()

  formattedQueries <- runSupplyT (traverseProperty compileQueryToFormat queries) [1 :: QueryID ..]
  case outputLocation of
    Nothing -> do
      forQueryInProperty formattedQueries $ \(queryMetaData, queryText) ->
        programOutput $ line <> line <> pretty (queryAddress queryMetaData) <> line <> pretty queryText
    Just folder -> do
      let queryMetaTree = fmap (fmap (fmap fst)) formattedQueries
      writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan queryMetaTree)
      forQueryInProperty formattedQueries $ writeVerificationQuery queryFormat folder

convertVectorOpsToPostulates :: FreeCtx Builtin -> FreeCtx Builtin
convertVectorOpsToPostulates = alterKeys vectorOperations (first convertToPostulate)
