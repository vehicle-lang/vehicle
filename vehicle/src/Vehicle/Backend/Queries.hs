module Vehicle.Backend.Queries
  ( compileToQueries,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), evalStateT)
import Control.Monad.Writer (MonadWriter (..), WriterT, runWriterT)
import Data.Data (Proxy (..))
import Data.List.NonEmpty as NonEmpty (unzip)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Traversable (for)
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Queries.Error
import Vehicle.Backend.Queries.IfElimination (unfoldIf)
import Vehicle.Backend.Queries.NetworkElimination
import Vehicle.Backend.Queries.QuerySetStructure
import Vehicle.Backend.Queries.UserVariableElimination (catchableUnsupportedNonLinearConstraint, eliminateUserVariables)
import Vehicle.Backend.Queries.Variable (MixedVariables (MixedVariables), UserVariable (..), pattern VInfiniteQuantifier)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Prelude.MonadContext (MonadContext (addDeclToContext), getNormDeclCtx, runContextT)
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Warning (CompileWarning (..))
import Vehicle.Expr.Boolean
import Vehicle.Expr.BuiltinInterface
import Vehicle.Expr.BuiltinPatterns
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (StdEqualsVector))
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
  (MonadIO m, MonadCompile m) =>
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
    (Main resourceFreeDecls, networkCtx, declCtx, integrityInfo) <- expandResources resources typedProg

    -- Perform the actual compilation to queries
    properties <-
      runContextT
        (Proxy @Builtin)
        (compileDecls typedProg queryFormat networkCtx mempty resourceFreeDecls maybeVerificationFolder)
        (declCtx, mempty)

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
  (MonadIO m, MonadCompile m, MonadContext Builtin m) =>
  Prog Ix Builtin ->
  QueryFormat ->
  NetworkContext ->
  UsedFunctionsCtx ->
  [Decl Ix Builtin] ->
  Maybe FilePath ->
  m [(Name, MultiProperty ())]
compileDecls _ _ _ _ [] _ = return []
compileDecls prog queryFormat networkCtx usedFunctionCtx (d : ds) outputLocation = do
  property <- case d of
    DefFunction p ident anns _ body
      | isProperty anns ->
          Just
            <$> compilePropertyDecl prog queryFormat networkCtx usedFunctionCtx p ident body outputLocation
    _ -> return Nothing

  let maybeUsedFunctionsInfo = getUsedFunctions usedFunctionCtx mempty <$> bodyOf d
  let usedFunctionsInfo = fromMaybe mempty maybeUsedFunctionsInfo
  -- We use `insertWith` to choose the old value here because expanded resources already exist in the map.
  let newUsedFunctionCtx = Map.insertWith (const id) (identifierOf d) usedFunctionsInfo usedFunctionCtx

  addDeclToContext d $ do
    properties <- compileDecls prog queryFormat networkCtx newUsedFunctionCtx ds outputLocation
    return $ maybeToList property ++ properties

compilePropertyDecl ::
  (MonadIO m, MonadCompile m, MonadContext Builtin m) =>
  Prog Ix Builtin ->
  QueryFormat ->
  NetworkContext ->
  UsedFunctionsCtx ->
  Provenance ->
  Identifier ->
  Expr Ix Builtin ->
  Maybe FilePath ->
  m (Name, MultiProperty ())
compilePropertyDecl prog queryFormat networkCtx queryDeclCtx p ident expr outputLocation = do
  logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
    declCtx <- getNormDeclCtx (Proxy @Builtin)
    normalisedExpr <- runNormT defaultEvalOptions declCtx mempty $ eval mempty expr

    let computeProperty = runWriterT $ compileMultiProperty queryFormat networkCtx queryDeclCtx p ident outputLocation normalisedExpr

    (property, unsoundConversion) <-
      computeProperty `catchError` \e -> do
        let formatID = queryFormatID queryFormat
        case e of
          UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog (ident, p)
          UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog (ident, p)
          _ -> throwError e

    when (getAny unsoundConversion) $
      logWarning (pretty (UnsoundStrictOrderConversion (nameOf ident) (queryFormatID queryFormat)))

    return (nameOf ident, property)

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadIO m, MonadCompile m, MonadContext Builtin m, MonadWriter UnsoundStrictOrderConversion m) =>
  QueryFormat ->
  NetworkContext ->
  UsedFunctionsCtx ->
  Provenance ->
  Identifier ->
  Maybe FilePath ->
  Value Builtin ->
  m (MultiProperty ())
compileMultiProperty queryFormat networkCtx declCtx p ident outputLocation = go []
  where
    go :: TensorIndices -> Value Builtin -> m (MultiProperty ())
    go indices expr = case expr of
      VVecLiteral es -> do
        let es' = zip [0 :: QueryID ..] es
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) (argExpr e)) es'
      _ -> do
        let logFunction =
              if null indices
                then id
                else logCompilerPass MinDetail ("property" <+> squotes (pretty ident <> pretty (showTensorIndices indices)))

        logFunction $ do
          let propertyAddress = PropertyAddress (nameOf ident) indices
          let propertyState = PropertyState queryFormat declCtx networkCtx (ident, p) propertyAddress
          evalStateT (runReaderT (compileProperty outputLocation expr) propertyState) 1
          return $ SingleProperty propertyAddress ()

--------------------------------------------------------------------------------
-- Compilation

data PropertyState = PropertyState
  { queryFormat :: QueryFormat,
    queryDeclCtx :: UsedFunctionsCtx,
    networkCtx :: NetworkContext,
    declProvenance :: DeclProvenance,
    propertyAddress :: PropertyAddress
  }

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyState m,
    MonadState QueryID m,
    MonadContext Builtin m,
    MonadWriter UnsoundStrictOrderConversion m
  )

-- Compiles an individual property
compileProperty ::
  (MonadCompileProperty m, MonadIO m) =>
  Maybe FilePath ->
  Value Builtin ->
  m ()
compileProperty outputLocation expr = do
  property <- compilePropertyTopLevelStructure expr

  PropertyState {..} <- ask
  (propertyMetaData, propertyQueries) <- case property of
    NonTrivial b -> do
      let (metaData, queries) = (NonEmpty.unzip . fmap NonEmpty.unzip) b
      return (NonTrivial metaData, NonTrivial queries)
    Trivial status -> do
      logWarning (pretty $ TrivialProperty propertyAddress status)
      return (Trivial status, Trivial status)

  case outputLocation of
    Nothing -> do
      forQueryInProperty propertyQueries $ \(queryAddress, queryText) ->
        programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
    Just folder -> do
      writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan propertyMetaData)
      forQueryInProperty propertyQueries $ writeVerificationQuery queryFormat folder

-- | Compiles the top-level structure of a property of type `Bool` until it
-- hits the first quantifier.
compilePropertyTopLevelStructure ::
  forall m.
  (MonadCompileProperty m) =>
  Value Builtin ->
  m (Property (QueryMetaData, QueryText))
compilePropertyTopLevelStructure = go
  where
    go :: Value Builtin -> m (Property (QueryMetaData, QueryText))
    go expr = case expr of
      VBoolLiteral {} ->
        fmap Query <$> compileQuerySet False expr
      VBuiltinFunction Equals {} _ ->
        fmap Query <$> compileQuerySet False expr
      VBuiltinFunction Order {} _ ->
        fmap Query <$> compileQuerySet False expr
      VFreeVar ident _
        | ident == identifierOf StdEqualsVector ->
            fmap Query <$> compileQuerySet False expr
      VBuiltinFunction And [e1, e2] ->
        andTrivial Conjunct <$> go (argExpr e1) <*> go (argExpr e2)
      VBuiltinFunction Or [e1, e2] ->
        orTrivial Disjunct <$> go (argExpr e1) <*> go (argExpr e2)
      VBuiltinFunction Not [e] ->
        case eliminateNot (argExpr e) of
          Nothing -> compilerDeveloperError $ "Unable to push not through:" <+> prettyVerbose e
          Just r -> go r
      VBuiltinFunction If [_, c, x, y] -> do
        let unfoldedIf = unfoldIf c (argExpr x) (argExpr y)
        logDebug MaxDetail $ "Unfolded `if` to" <+> prettyFriendly (WithContext unfoldedIf emptyDBCtx)
        go unfoldedIf
      VInfiniteQuantifier q args binder env body -> do
        let subsectionDoc = "compilation of set of queries:" <+> prettyFriendly (WithContext expr emptyDBCtx)
        logCompilerPass MaxDetail subsectionDoc $ do
          -- Have to check whether to negate the quantifier here, rather than at the top
          -- of the property, as we may have parallel quantifiers of different polarities
          -- e.g. (forall x . P x) and (exists y . Q y).
          (isPropertyNegated, existsBody) <- case q of
            Exists -> return (False, body)
            Forall -> do
              -- If the property is universally quantified then we negate the expression.
              logDebug MinDetail ("Negating property..." <> line)
              let p = mempty
              return (True, BuiltinFunctionExpr p Not [Arg p Explicit Relevant body])

          let negatedExpr = VInfiniteQuantifier Exists args binder env existsBody
          fmap Query <$> compileQuerySet isPropertyNegated negatedExpr
      _ -> unexpectedExprError "compilation of top-level property structure" (prettyVerbose expr)

compileQuerySet ::
  (MonadCompileProperty m) =>
  Bool ->
  Value Builtin ->
  m (MaybeTrivial (QuerySet (QueryMetaData, QueryText)))
compileQuerySet isPropertyNegated expr = do
  PropertyState {..} <- ask
  -- First we attempt to recursively compile down the remaining boolean structure,
  -- stopping at the level of individual propositions (e.g. equality or ordering assertions)
  queryStructureResult <- compileQueryStructure declProvenance queryDeclCtx networkCtx expr
  case queryStructureResult of
    Left err -> case err of
      AlternatingQuantifiers ->
        throwError catchableUnsupportedAlternatingQuantifiersError
      NonLinearSpecification e -> do
        logDebug MinDetail $ "Found non-linear expression: " <+> prettyVerbose e <> line
        throwError catchableUnsupportedNonLinearConstraint
      UnsupportedQuantifierType binder variableType -> do
        let target = queryFormatID queryFormat
        let p = provenanceOf binder
        let baseName = getBinderName binder
        let baseType = binderType binder
        let declIdent = fst declProvenance
        throwError $ UnsupportedVariableType target declIdent p baseName variableType baseType [BuiltinType Rat]
      UnsupportedInequalityOp -> do
        throwError $ UnsupportedInequality (queryFormatID queryFormat) declProvenance
    Right (quantifiedVariables, maybeTrivialBoolExpr, userVariableReductionInfo) -> do
      case maybeTrivialBoolExpr of
        Trivial b -> return $ Trivial (isPropertyNegated `xor` b)
        NonTrivial boolExpr -> do
          metaNetworkPartitions <- replaceNetworkApplications declProvenance networkCtx quantifiedVariables boolExpr
          let numberedMetaNetworkPartitions = zipDisjuncts [1 ..] metaNetworkPartitions
          let compilePartition = compileMetaNetworkPartition userVariableReductionInfo quantifiedVariables
          queries <- traverse compilePartition numberedMetaNetworkPartitions
          let maybeFlattenedQueries = fmap concatDisjuncts (eliminateTrivialDisjunctions queries)
          case maybeFlattenedQueries of
            Trivial b -> return $ Trivial (isPropertyNegated `xor` b)
            NonTrivial flattenedQueries -> return $ NonTrivial $ QuerySet isPropertyNegated flattenedQueries

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedAlternatingQuantifiersError :: CompileError
catchableUnsupportedAlternatingQuantifiersError =
  UnsupportedAlternatingQuantifiers x x x
  where
    x = developerError "Evaluating temporary quantifier error"

compileMetaNetworkPartition ::
  (MonadCompileProperty m) =>
  VariableNormalisationSteps ->
  BoundCtx UserVariable ->
  (Int, MetaNetworkPartition) ->
  m (MaybeTrivial (DisjunctAll (QueryAddress, (QueryMetaData, QueryText))))
compileMetaNetworkPartition userVariableReductionSteps userVariables (partitionID, MetaNetworkPartition {..}) = do
  PropertyState {..} <- ask
  declCtx <- getNormDeclCtx (Proxy @Builtin)
  logCompilerPass MinDetail ("compilation of meta-network partition" <+> pretty partitionID) $ do
    -- Convert it into linear satisfaction problems in the network variables
    let mixedVariables = MixedVariables userVariables networkVars
    clstQueries <- eliminateUserVariables declCtx declProvenance metaNetwork mixedVariables partitionExpr

    -- Compile the query to the specific verifiers.
    case clstQueries of
      Trivial b -> do
        logDebug MinDetail $ "Meta-network partition found to be trivially" <+> pretty b
        return $ Trivial b
      NonTrivial queries ->
        NonTrivial
          <$> for
            queries
            ( \(conjunctions, userVariableEliminationSteps) -> do
                queryID <- get
                put (queryID + 1)

                queryText <- compileQuery queryFormat conjunctions
                let allVariableSteps = userVariableReductionSteps <> networkNormSteps <> userVariableEliminationSteps
                let queryAddress = (propertyAddress, queryID)
                let queryData = QueryData metaNetwork allVariableSteps

                logDebug MaxDetail $ "Final query:" <> line <> indent 2 (pretty queryText) <> line
                logDebug MaxDetail $ "Variable sequence:" <> line <> indent 2 (pretty allVariableSteps)

                return (queryAddress, (queryData, queryText))
            )
