module Vehicle.Backend.Queries
  ( compileToQueries,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), evalStateT)
import Data.List.NonEmpty as NonEmpty (unzip)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Traversable (for)
import System.Directory (createDirectoryIfMissing)
import Vehicle.Backend.Queries.Error
import Vehicle.Backend.Queries.IfElimination (unfoldIf)
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Backend.Queries.NetworkElimination
import Vehicle.Backend.Queries.QuerySetStructure
import Vehicle.Backend.Queries.UsedFunctions
import Vehicle.Backend.Queries.UserVariableElimination (eliminateUserVariables)
import Vehicle.Backend.Queries.Variable (MixedVariables (MixedVariables), NetworkVariable (..), UserVariableCtx, pattern VInfiniteQuantifier)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx, prettyVerbose)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.BuiltinPatterns
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdEqualsVector))
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
    (Main resourceFreeDecls, networkCtx, freeCtx, integrityInfo) <- expandResources resources typedProg

    -- Perform the actual compilation to queries
    properties <-
      runFreeContextT
        freeCtx
        (compileDecls typedProg queryFormat networkCtx mempty resourceFreeDecls maybeVerificationFolder)

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
  (MonadIO m, MonadCompile m, MonadFreeContext Builtin m) =>
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
  (MonadIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  Prog Ix Builtin ->
  QueryFormat ->
  NetworkContext ->
  UsedFunctionsCtx ->
  Provenance ->
  Identifier ->
  Expr Ix Builtin ->
  Maybe FilePath ->
  m (Name, MultiProperty ())
compilePropertyDecl prog queryFormat networkCtx queryFreeCtx p ident expr outputLocation = do
  logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
    normalisedExpr <- eval (mkNBEOptions vectorStructureOperations) emptyEnv expr

    let computeProperty = compileMultiProperty queryFormat networkCtx queryFreeCtx p ident outputLocation normalisedExpr

    property <-
      computeProperty `catchError` \e -> do
        let formatID = queryFormatID queryFormat
        case e of
          UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog (ident, p)
          UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog (ident, p)
          _ -> throwError e

    return (nameOf ident, property)

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadIO m, MonadCompile m, MonadFreeContext Builtin m) =>
  QueryFormat ->
  NetworkContext ->
  UsedFunctionsCtx ->
  Provenance ->
  Identifier ->
  Maybe FilePath ->
  WHNFValue Builtin ->
  m (MultiProperty ())
compileMultiProperty queryFormat networkCtx freeCtx p ident outputLocation = go []
  where
    go :: TensorIndices -> WHNFValue Builtin -> m (MultiProperty ())
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
          let propertyState = PropertyState queryFormat freeCtx networkCtx (ident, p) propertyAddress
          evalStateT (runReaderT (compileProperty outputLocation expr) propertyState) 1
          return $ SingleProperty propertyAddress ()

--------------------------------------------------------------------------------
-- Compilation

data PropertyState = PropertyState
  { queryFormat :: QueryFormat,
    usedFunctionsCtx :: UsedFunctionsCtx,
    networkCtx :: NetworkContext,
    declProvenance :: DeclProvenance,
    propertyAddress :: PropertyAddress
  }

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyState m,
    MonadState QueryID m,
    MonadFreeContext Builtin m
  )

-- Compiles an individual property
compileProperty ::
  (MonadCompileProperty m, MonadIO m) =>
  Maybe FilePath ->
  WHNFValue Builtin ->
  m ()
compileProperty outputLocation expr = do
  property <- compilePropertyTopLevelStructure expr

  PropertyState {..} <- ask
  (propertyMetaData, propertyQueries) <- case property of
    NonTrivial b -> do
      let (metaData, queries) = (NonEmpty.unzip . fmap NonEmpty.unzip) b
      return (NonTrivial metaData, NonTrivial queries)
    Trivial status -> do
      logWarning (TrivialProperty propertyAddress status)
      return (Trivial status, Trivial status)

  case outputLocation of
    Nothing -> do
      forQueryInProperty propertyQueries $ \(queryAddress, queryText) ->
        programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
    Just folder -> do
      writePropertyVerificationPlan folder propertyAddress (PropertyVerificationPlan propertyMetaData)
      forQueryInProperty propertyQueries $ writeVerificationQuery queryFormat folder

-- | Compiles the top-level structure of a property until it hits the first quantifier.
-- Assumptions - expression is well-typed in the empty context and of type Bool.
compilePropertyTopLevelStructure ::
  forall m.
  (MonadCompileProperty m) =>
  WHNFValue Builtin ->
  m (Property (QueryMetaData, QueryText))
compilePropertyTopLevelStructure = go
  where
    go :: WHNFValue Builtin -> m (Property (QueryMetaData, QueryText))
    go expr = case expr of
      VBoolLiteral {} -> compileQuerySet False expr
      VBuiltinFunction Equals {} _ -> compileQuerySet False expr
      VBuiltinFunction Order {} _ -> compileQuerySet False expr
      VFreeVar ident _
        | ident == identifierOf StdEqualsVector -> compileQuerySet False expr
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
        logDebug MaxDetail $ "Unfolded `if` to" <+> prettyFriendlyEmptyCtx unfoldedIf
        go unfoldedIf
      VInfiniteQuantifier Exists _ _ _ _ -> compileQuerySet False expr
      VInfiniteQuantifier Forall args binder env body -> do
        -- Have to check whether to negate the quantifier here, rather than at the top
        -- of the property, as we may have parallel quantifiers of different polarities
        -- e.g. (forall x . P x) and (exists y . Q y).
        logDebug MinDetail ("Negating property..." <> line)
        let negBody = BuiltinFunctionExpr mempty Not [Arg mempty Explicit Relevant body]
        let negExpr = VInfiniteQuantifier Exists args binder env negBody
        compileQuerySet True negExpr
      -- This case only happens because we can't yet evaluate neural networks
      -- when applied to real inputs. For example
      -- `fold (\x r -> x > 0 and r) True (f [0])` won't evaluate because we
      -- don't evaluate `f [0]`. If we fix that problem this case should disappear
      -- because we can't have any abstract variables in the empty context so we
      -- can't block the evaluation of `fold`.
      VBuiltinFunction (Fold FoldVector) _ -> compileQuerySet False expr
      _ -> unexpectedExprError "compilation of top-level property structure" (prettyVerbose expr)

compileQuerySet ::
  (MonadCompileProperty m) =>
  Bool ->
  WHNFValue Builtin ->
  m (MaybeTrivial (BooleanExpr (QuerySet (QueryMetaData, QueryText))))
compileQuerySet isPropertyNegated expr = do
  let subsectionDoc = "compilation of set of queries:" <+> prettyFriendlyEmptyCtx expr
  logCompilerPass MaxDetail subsectionDoc $ do
    PropertyState {..} <- ask
    let target = queryFormatID queryFormat
    -- First we attempt to recursively compile down the remaining boolean structure,
    -- stopping at the level of individual propositions (e.g. equality or ordering assertions)
    (quantifiedVariables, boolExpr, userVariableReductionInfo) <-
      compileSetQueryStructure target declProvenance usedFunctionsCtx networkCtx expr

    metaNetworkPartitions <- replaceNetworkApplications declProvenance networkCtx quantifiedVariables boolExpr
    let numberedMetaNetworkPartitions = zipDisjuncts [1 ..] metaNetworkPartitions
    let compilePartition = compileMetaNetworkPartition userVariableReductionInfo quantifiedVariables
    queries <- traverse compilePartition numberedMetaNetworkPartitions
    let maybeFlattenedQueries = fmap concatDisjuncts (eliminateTrivialDisjunctions queries)
    case maybeFlattenedQueries of
      Trivial b -> return $ Trivial (isPropertyNegated `xor` b)
      NonTrivial flattenedQueries -> return $ NonTrivial $ Query $ QuerySet isPropertyNegated flattenedQueries

compileMetaNetworkPartition ::
  (MonadCompileProperty m) =>
  VariableNormalisationSteps ->
  UserVariableCtx ->
  (Int, MetaNetworkPartition) ->
  m (MaybeTrivial (DisjunctAll (QueryAddress, (QueryMetaData, QueryText))))
compileMetaNetworkPartition userVariableReductionSteps userVariables (partitionID, MetaNetworkPartition {..}) = do
  PropertyState {..} <- ask
  logCompilerPass MinDetail ("compilation of meta-network partition" <+> pretty partitionID) $ do
    -- Convert it into linear satisfaction problems in the network variables
    let mixedVariables = MixedVariables userVariables networkVars
    clstQueries <- eliminateUserVariables declProvenance metaNetwork mixedVariables partitionExpr

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

                checkIfInputsWellSpecificied conjunctions
                let sortedConjunctions = sortCLSTProblem conjunctions
                queryText <- compileQuery queryFormat declProvenance sortedConjunctions
                let allVariableSteps = userVariableReductionSteps <> networkNormSteps <> userVariableEliminationSteps
                let queryAddress = (propertyAddress, queryID)
                let queryData = QueryData metaNetwork allVariableSteps

                logDebug MaxDetail $ "Final query:" <> line <> indent 2 (pretty queryText) <> line
                logDebug MaxDetail $ "Variable sequence:" <> line <> indent 2 (pretty allVariableSteps)

                return (queryAddress, (queryData, queryText))
            )

-- | Checks for presence of under-constrained input variables.
checkIfInputsWellSpecificied ::
  (MonadCompileProperty m) =>
  CLSTProblem ->
  m ()
checkIfInputsWellSpecificied (CLSTProblem variables assertions) = do
  PropertyState {..} <- ask
  let property = propertyName propertyAddress
  let format = queryFormatID queryFormat
  let inputVariables = filter (\v -> inputOrOutput v == Input) variables
  let initialStatuses = Map.fromList (fmap (,UnderConstrained Unconstrained) inputVariables)
  let finalStatuses = foldr updateStatuses initialStatuses assertions

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  when (format == MarabouQueries && all (== Constant) finalStatuses) $
    logWarning $
      AllConstantInputsMarabouBug property

  -- Check if all inputs are well-specified.
  let underSpecified = mapMaybe (\(v, s) -> (v,) <$> toUnderConstrainedStatus s) $ Map.toList finalStatuses
  unless (null underSpecified) $
    logWarning $
      UnderSpecifiedNetworkInputs property format underSpecified
  where
    updateStatuses ::
      Assertion NetworkVariable ->
      Map NetworkVariable VariableConstraintStatus ->
      Map NetworkVariable VariableConstraintStatus
    updateStatuses assertion statuses = case coefficientsList assertion of
      [(v, c)] | inputOrOutput v == Input -> do
        let status = case assertionRel assertion of
              Equal -> Constant
              _
                | c >= 0 -> UnderConstrained BoundedAbove
                | otherwise -> UnderConstrained BoundedBelow
        Map.insertWith (<>) v status statuses
      _ -> statuses
