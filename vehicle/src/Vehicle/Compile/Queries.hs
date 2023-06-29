module Vehicle.Compile.Queries
  ( compileToQueries,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), evalStateT)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Traversable (for)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources, splitResourceCtx)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.IfElimination (unfoldIf)
import Vehicle.Compile.Queries.LinearityAndPolarityErrors
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Queries.QuerySetStructure
import Vehicle.Compile.Queries.UserVariableElimination (catchableUnsupportedNonLinearConstraint, eliminateUserVariables)
import Vehicle.Compile.Queries.Variable (MixedVariables (MixedVariables), UserVariable (..), pattern VInfiniteQuantifier)
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (StdLibFunction (StdEqualsVector))
import Vehicle.Verify.Core
import Vehicle.Verify.Specification

--------------------------------------------------------------------------------
-- Compilation to individual queries

currentPass :: Doc a
currentPass = "compilation of properties"

-- | Compiles the provided program to invidividual queries suitable for a
-- verifier.
compileToQueries ::
  (MonadIO m, MonadCompile m) =>
  QueryFormat ->
  StandardGluedProg ->
  Resources ->
  m (Specification (Property (QueryMetaData, QueryText)))
compileToQueries queryFormat typedProg resources =
  logCompilerPass MinDetail currentPass $ do
    properties <- compileProgToQueries queryFormat resources typedProg
    if null properties
      then throwError NoPropertiesFound
      else return $ Specification properties

--------------------------------------------------------------------------------
-- Getting properties

compileProgToQueries ::
  (MonadIO m, MonadCompile m) =>
  QueryFormat ->
  Resources ->
  GenericProg StandardGluedExpr ->
  m [(Name, MultiProperty (Property (QueryMetaData, QueryText)))]
compileProgToQueries queryFormat resources prog = do
  resourceCtx <- expandResources resources prog
  let (networkCtx, declCtx) = splitResourceCtx resourceCtx
  let queryDeclCtx = fmap (,mempty) declCtx
  let Main decls = prog
  compileDecls prog queryFormat networkCtx queryDeclCtx decls

compileDecls ::
  (MonadCompile m) =>
  StandardGluedProg ->
  QueryFormat ->
  NetworkContext ->
  QueryDeclCtx ->
  [StandardGluedDecl] ->
  m [(Name, MultiProperty (Property (QueryMetaData, QueryText)))]
compileDecls _ _ _ _ [] = return []
compileDecls prog queryFormat networkCtx queryDeclCtx (d : ds) = case d of
  DefAbstract {} -> compileDecls prog queryFormat networkCtx queryDeclCtx ds
  DefFunction p ident anns typ body -> do
    maybeProperty <-
      if not (isProperty anns)
        then return Nothing
        else Just <$> compilePropertyDecl prog queryFormat networkCtx queryDeclCtx p ident body

    let declCtxEntry = NormDeclCtxEntry (normalised body) anns (arity (normalised typ))
    let usedFunctionsInfo = getUsedFunctions (queryDeclCtxToInfoDeclCtx queryDeclCtx) mempty (unnormalised body)
    let newQueryDeclCtx = Map.insert ident (declCtxEntry, usedFunctionsInfo) queryDeclCtx
    properties <- compileDecls prog queryFormat networkCtx newQueryDeclCtx ds

    return $ maybeToList maybeProperty ++ properties

compilePropertyDecl ::
  (MonadCompile m) =>
  StandardGluedProg ->
  QueryFormat ->
  NetworkContext ->
  QueryDeclCtx ->
  Provenance ->
  Identifier ->
  StandardGluedExpr ->
  m (Name, MultiProperty (Property (QueryMetaData, QueryText)))
compilePropertyDecl prog queryFormat networkCtx queryDeclCtx p ident expr = do
  logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
    -- Normalise the property expression.
    -- Note: we can't use the `normalised` part of the glued expression here because
    -- the external resources have been added since it was normalised during type-checking.
    let declCtx = queryDeclCtxToNormDeclCtx queryDeclCtx
    normalisedExpr <- runNormT defaultEvalOptions declCtx mempty $ eval mempty (unnormalised expr)

    let computeProperty = compileMultiProperty queryFormat networkCtx queryDeclCtx p ident normalisedExpr
    property <-
      computeProperty `catchError` \e -> do
        let formatID = queryFormatID queryFormat
        case e of
          UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog ident
          UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog ident
          _ -> throwError e

    return (nameOf ident, property)

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileMultiProperty ::
  forall m.
  (MonadCompile m) =>
  QueryFormat ->
  NetworkContext ->
  QueryDeclCtx ->
  Provenance ->
  Identifier ->
  StandardNormExpr ->
  m (MultiProperty (Property (QueryMetaData, QueryText)))
compileMultiProperty queryFormat networkCtx declCtx p ident = go []
  where
    go :: TensorIndices -> StandardNormExpr -> m (MultiProperty (Property (QueryMetaData, QueryText)))
    go indices expr = case expr of
      VVecLiteral es -> do
        let es' = zip [0 :: QueryID ..] es
        MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
      _ -> do
        let logFunction =
              if null indices
                then id
                else logCompilerPass MinDetail ("property" <+> squotes (pretty ident <> pretty (showTensorIndices indices)))

        logFunction $ do
          let propertyAddress = PropertyAddress (nameOf ident) indices
          let propertyState = PropertyState queryFormat declCtx networkCtx (ident, p) propertyAddress
          property <- evalStateT (runReaderT (compilePropertyTopLevelStructure expr) propertyState) 1
          return $ SingleProperty propertyAddress property

--------------------------------------------------------------------------------
-- Compilation

data PropertyState = PropertyState
  { queryFormat :: QueryFormat,
    queryDeclCtx :: QueryDeclCtx,
    networkCtx :: NetworkContext,
    declProvenance :: DeclProvenance,
    propertyAddress :: PropertyAddress
  }

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyState m,
    MonadState QueryID m
  )

-- | Compiles the top-level structure of a property of type `Bool` until it
-- hits the first quantifier.
compilePropertyTopLevelStructure ::
  forall m.
  (MonadCompileProperty m) =>
  StandardNormExpr ->
  m (Property (QueryMetaData, QueryText))
compilePropertyTopLevelStructure = go
  where
    go :: StandardNormExpr -> m (Property (QueryMetaData, QueryText))
    go expr = case expr of
      VBoolLiteral {} ->
        Query <$> compileQuerySet False expr
      VBuiltinFunction Equals {} _ ->
        Query <$> compileQuerySet False expr
      VBuiltinFunction Order {} _ ->
        Query <$> compileQuerySet False expr
      VFreeVar ident _
        | ident == identifierOf StdEqualsVector ->
            Query <$> compileQuerySet False expr
      VBuiltinFunction And [e1, e2] ->
        smartConjunct <$> go e1 <*> go e2
      VBuiltinFunction Or [e1, e2] ->
        smartDisjunct <$> go e1 <*> go e2
      VBuiltinFunction Not [e] ->
        case eliminateNot e of
          Nothing -> compilerDeveloperError $ "Unable to push not through:" <+> prettyVerbose e
          Just r -> go r
      VBuiltinFunction If [c, x, y] -> do
        let unfoldedIf = unfoldIf c x y
        logDebug MaxDetail $ "Unfolded `if` to" <+> prettyFriendly (WithContext unfoldedIf emptyDBCtx)
        go $ unfoldIf c x y
      VInfiniteQuantifier q dom args binder env body -> do
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
              return (True, BuiltinFunctionExpr p Not [ExplicitArg p body])

          let negatedExpr = VInfiniteQuantifier Exists dom args binder env existsBody
          Query <$> compileQuerySet isPropertyNegated negatedExpr
      _ -> unexpectedExprError "compilation of top-level property structure" (prettyVerbose expr)

compileQuerySet ::
  (MonadCompileProperty m) =>
  Bool ->
  StandardNormExpr ->
  m (QuerySet (QueryMetaData, QueryText))
compileQuerySet isPropertyNegated expr = do
  PropertyState {..} <- ask
  -- First we attempt to recursively compile down the remaining boolean structure,
  -- stopping at the level of individual propositions (e.g. equality or ordering assertions)
  queryStructureResult <- compileQueryStructure declProvenance queryDeclCtx expr
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
      queries <- case maybeTrivialBoolExpr of
        Trivial b -> return $ Trivial b
        NonTrivial boolExpr -> do
          metaNetworkPartitions <- replaceNetworkApplications declProvenance networkCtx quantifiedVariables boolExpr
          let numberedMetaNetworkPartitions = zipDisjuncts [1 ..] metaNetworkPartitions
          let compilePartition = compileMetaNetworkPartition userVariableReductionInfo quantifiedVariables
          queries <- traverse compilePartition numberedMetaNetworkPartitions
          return $ fmap concatDisjuncts (eliminateTrivialDisjunctions queries)

      return $ QuerySet isPropertyNegated queries

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedAlternatingQuantifiersError :: CompileError
catchableUnsupportedAlternatingQuantifiersError =
  UnsupportedAlternatingQuantifiers x x x x x
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
  let declCtx = queryDeclCtxToNormDeclCtx queryDeclCtx
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

--------------------------------------------------------------------------------
-- Other

smartConjunct :: BooleanExpr (QuerySet a) -> BooleanExpr (QuerySet a) -> BooleanExpr (QuerySet a)
smartConjunct x y = case (x, y) of
  (Query (QuerySet _ (Trivial b)), _) -> if b then y else x
  (_, Query (QuerySet _ (Trivial b))) -> if b then x else y
  (_, _) -> Conjunct x y

smartDisjunct :: BooleanExpr (QuerySet a) -> BooleanExpr (QuerySet a) -> BooleanExpr (QuerySet a)
smartDisjunct x y = case (x, y) of
  (Query (QuerySet _ (Trivial b)), _) -> if b then x else y
  (_, Query (QuerySet _ (Trivial b))) -> if b then y else x
  (_, _) -> Conjunct x y
