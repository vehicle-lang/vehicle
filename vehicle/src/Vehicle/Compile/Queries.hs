module Vehicle.Compile.Queries
  ( compileToQueries,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState, StateT, evalStateT)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (pack)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources, splitResourceCtx)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Compile.Queries.LinearSatisfactionProblem (UserVariableEliminationCache, generateCLSTProblem)
import Vehicle.Compile.Queries.LinearityAndPolarityErrors
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Queries.Variable (UserVariable (..))
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean
import Vehicle.Expr.DeBruijn (Lv (..))
import Vehicle.Expr.Normalised
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
  m (Specification (QueryMetaData, QueryText))
compileToQueries queryFormat typedProg resources =
  logCompilerPass MinDetail currentPass $ do
    properties <- compileProgToQueries queryFormat resources typedProg
    if null properties
      then throwError NoPropertiesFound
      else return $ Specification properties

--------------------------------------------------------------------------------
-- Getting properties

compileProgToQueries ::
  forall m.
  (MonadIO m, MonadCompile m) =>
  QueryFormat ->
  Resources ->
  GenericProg StandardGluedExpr ->
  m [(Name, MultiProperty (QueryMetaData, QueryText))]
compileProgToQueries queryFormat resources prog = do
  resourceCtx <- expandResources resources prog
  let (networkCtx, declCtx) = splitResourceCtx resourceCtx
  let Main decls = prog
  compileDecls networkCtx declCtx decls
  where
    compileDecls ::
      NetworkContext ->
      StandardNormDeclCtx ->
      [StandardGluedDecl] ->
      m [(Name, MultiProperty (QueryMetaData, QueryText))]
    compileDecls _ _ [] = return []
    compileDecls networkCtx declCtx (d : ds) = case d of
      DefAbstract {} -> compileDecls networkCtx declCtx ds
      DefFunction p ident anns typ body -> do
        maybeProperty <-
          if not (isProperty anns)
            then return Nothing
            else Just <$> compilePropertyDecl networkCtx declCtx p ident typ body

        let declCtxEntry = NormDeclCtxEntry (normalised body) anns
        let newDeclCtx = Map.insert ident declCtxEntry declCtx
        properties <- compileDecls networkCtx newDeclCtx ds

        return $ maybeToList maybeProperty ++ properties

    compilePropertyDecl ::
      NetworkContext ->
      StandardNormDeclCtx ->
      Provenance ->
      Identifier ->
      StandardGluedType ->
      StandardGluedExpr ->
      m (Name, MultiProperty (QueryMetaData, QueryText))
    compilePropertyDecl networkCtx declCtx p ident _typ expr = do
      logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
        -- We can't use the `normalised` part of the glued expression here because
        -- the external resources have been added since it was normalised during type-checking.
        normalisedExpr <- runNormT declCtx mempty $ eval mempty (unnormalised expr)
        let computeProperty = compileMultiProperty networkCtx declCtx p ident normalisedExpr
        property <-
          computeProperty `catchError` \e -> do
            let formatID = queryFormatID queryFormat
            case e of
              UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity formatID prog ident
              UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers formatID prog ident
              _ -> throwError e

        return (nameOf ident, property)

    -- \| Compiles a property of type `Tensor Bool dims` for some variable `dims`,
    -- by recursing through the levels of vectors until it reaches something of
    -- type `Bool`.
    compileMultiProperty ::
      NetworkContext ->
      StandardNormDeclCtx ->
      Provenance ->
      Identifier ->
      StandardNormExpr ->
      m (MultiProperty (QueryMetaData, QueryText))
    compileMultiProperty networkCtx declCtx p ident = go []
      where
        go :: TensorIndices -> StandardNormExpr -> m (MultiProperty (QueryMetaData, QueryText))
        go indices = \case
          VVecLiteral es -> do
            let es' = zip [0 :: QueryID ..] es
            MultiProperty <$> traverse (\(i, e) -> go (i : indices) e) es'
          expr ->
            logCompilerSection MaxDetail "Starting single boolean property" $ do
              let propertyAddress = (nameOf ident, indices)
              let state = PropertyState queryFormat declCtx networkCtx p ident indices
              let compileProperty = compilePropertyTopLevelStructure expr
              property <- runMonadCompileProperty state compileProperty
              return $ SingleProperty propertyAddress property

--------------------------------------------------------------------------------
-- Compilation

data PropertyState = PropertyState
  { queryFormat :: QueryFormat,
    declSubst :: StandardNormDeclCtx,
    networkCtx :: NetworkContext,
    declProvenance :: Provenance,
    declIdentifier :: Identifier,
    propertyIndices :: TensorIndices
  }

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyState m,
    MonadSupply QueryID m,
    MonadState UserVariableEliminationCache m
  )

runMonadCompileProperty ::
  (Monad m) =>
  PropertyState ->
  ReaderT PropertyState (SupplyT QueryID (StateT UserVariableEliminationCache m)) a ->
  m a
runMonadCompileProperty state r =
  evalStateT (runSupplyT (runReaderT r state) [1 :: QueryID ..]) mempty

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
      VBuiltinFunction And [e1, e2] ->
        smartConjunct <$> go e1 <*> go e2
      VBuiltinFunction Or [e1, e2] ->
        smartDisjunct <$> go e1 <*> go e2
      VBuiltinFunction Not [x] ->
        go $ lowerNotNorm x
      VBuiltinFunction If [c, x, y] -> do
        let unfoldedIf = unfoldIf c x y
        logDebug MaxDetail $ "Unfolded `if` to" <+> prettyFriendly (WithContext unfoldedIf emptyDBCtx)
        go $ unfoldIf c x y
      VQuantifierExpr q dom args binder env body -> do
        let subsectionDoc = "compilation of set of queries:" <+> prettyFriendly (WithContext expr emptyDBCtx)
        logCompilerPass MaxDetail subsectionDoc $ do
          -- Have to check whether to negate the quantifier here, rather than at the top
          -- of the property, as we may have parallel quantifiers of different polarities
          -- e.g. (forall x . P x) and (exists y . Q y).
          (isPropertyNegated, existsBody) <- case q of
            Exists -> return (False, body)
            Forall -> do
              -- If the property is universally quantified then we negate the expression.
              logDebug MinDetail "Negating property..."
              let p = mempty
              return (True, BuiltinFunctionExpr p Not [ExplicitArg p body])

          let negatedExpr = VQuantifierExpr Exists dom args binder env existsBody
          Query <$> compileQuerySet isPropertyNegated negatedExpr
      _ -> unexpectedExprError "compilation of top-level property structure" (prettyVerbose expr)

compileQuerySet ::
  (MonadCompileProperty m) =>
  Bool ->
  StandardNormExpr ->
  m (QuerySet (QueryMetaData, QueryText))
compileQuerySet isPropertyNegated expr = do
  -- First we recursively compile down the remaining boolean structure, stopping at
  -- the level of individual propositions (e.g. equality or ordering assertions)
  (propositionTree, quantifiedVariables) <-
    compileQueryStructure mempty expr

  -- We then convert this tree into disjunctive normal form (DNF).
  dnfTree <- logCompilerSection MaxDetail "Converting to disjunctive normal form..." $ do
    let dnfTree = convertToDNF propositionTree
    logDebug MaxDetail $ prettyVerbose dnfTree
    return dnfTree

  queries <- case dnfTree of
    Trivial b -> return $ Trivial b
    NonTrivial dnf -> do
      -- Split up into the individual queries needed for Marabou.
      logDebug MinDetail $ "Found" <+> pretty (length dnf) <+> "potential queries" <> line
      queries <- traverse (compileSingleQuery quantifiedVariables) dnf
      return $ eliminateTrivialDisjunctions queries

  return $ QuerySet isPropertyNegated queries

-- | This is a tree structure which stores the reverse environment at each
-- node and propositional expressions at the leaves.
type PropositionTree = BooleanExpr StandardNormExpr

-- | The set of variables that will be cumulatively in scope at the current
-- point in time once all existential quantifiers have been lifted to the
-- top level. Essentially the set of quantifiers that are before the current
-- point in the tree when traversed depth first.
type QuantifiedVariables = [(Name, TensorDimensions, [UserVariable])]

cumulativeVarsToCtx :: QuantifiedVariables -> BoundDBCtx
cumulativeVarsToCtx = concatMap (fmap (Just . userVarName) . (\(_, _, c) -> c))

compileQueryStructure ::
  forall m.
  (MonadCompileProperty m) =>
  QuantifiedVariables ->
  StandardNormExpr ->
  m (PropositionTree, QuantifiedVariables)
compileQueryStructure = go False
  where
    go :: Bool -> QuantifiedVariables -> StandardNormExpr -> m (PropositionTree, QuantifiedVariables)
    go processingLiftedIfs quantifiedVariables expr = case expr of
      VBuiltinFunction And [e1, e2] ->
        goOp2 Conjunct processingLiftedIfs quantifiedVariables e1 e2
      VBuiltinFunction Or [e1, e2] ->
        goOp2 Disjunct processingLiftedIfs quantifiedVariables e1 e2
      VBuiltinFunction Not [x] ->
        go processingLiftedIfs quantifiedVariables $ lowerNotNorm x
      VBuiltinFunction If [c, x, y] ->
        go processingLiftedIfs quantifiedVariables $ unfoldIf c x y
      VQuantifierExpr q dom _ binder env body -> case q of
        Exists -> compileQuantifierBodyToPropositionTree quantifiedVariables dom binder env body
        Forall -> throwError temporaryUnsupportedAlternatingQuantifiersError
      VBuiltinFunction Equals {} _ ->
        goProposition processingLiftedIfs quantifiedVariables expr
      VBuiltinFunction Order {} _ ->
        goProposition processingLiftedIfs quantifiedVariables expr
      VBoolLiteral {} -> do
        goProposition processingLiftedIfs quantifiedVariables expr
      _ -> unexpectedExprError "compiling query structure" (prettyVerbose expr)

    goOp2 ::
      (PropositionTree -> PropositionTree -> PropositionTree) ->
      Bool ->
      QuantifiedVariables ->
      StandardNormExpr ->
      StandardNormExpr ->
      m (PropositionTree, QuantifiedVariables)
    goOp2 op2 processingLiftedIfs quantifiedVariables e1 e2 = do
      let lhsVariables = quantifiedVariables
      (e1', lhsUserVars) <- go processingLiftedIfs lhsVariables e1
      let rhsVariables = lhsUserVars <> quantifiedVariables
      (e2', rhsUserVars) <- go processingLiftedIfs rhsVariables e2
      let userVars = lhsUserVars <> rhsUserVars
      return (op2 e1' e2', userVars)

    goProposition :: Bool -> QuantifiedVariables -> StandardNormExpr -> m (PropositionTree, QuantifiedVariables)
    goProposition processingLiftedIfs quantifiedVariables expr = do
      let ctx = cumulativeVarsToCtx quantifiedVariables
      let subsectionDoc = "Identified proposition:" <+> prettyFriendly (WithContext expr ctx)
      logDebug MaxDetail subsectionDoc
      if processingLiftedIfs
        then do
          return (Query expr, [])
        else do
          incrCallDepth
          exprWithoutIf <- eliminateIfs ctx expr
          result <-
            if not (wasIfLifted exprWithoutIf)
              then return (Query expr, [])
              else go True quantifiedVariables exprWithoutIf
          decrCallDepth
          return result

    wasIfLifted :: StandardNormExpr -> Bool
    wasIfLifted (VBuiltinFunction Or _) = True
    wasIfLifted _ = False

compileQuantifierBodyToPropositionTree ::
  (MonadCompileProperty m) =>
  QuantifiedVariables ->
  QuantifierDomain ->
  StandardNormBinder ->
  StandardEnv ->
  TypeCheckedExpr ->
  m (PropositionTree, QuantifiedVariables)
compileQuantifierBodyToPropositionTree quantifiedVariables _ binder env body = do
  propertyState@PropertyState {..} <- ask

  let variableName = getBinderName binder
  -- TODO avoid calculating this repeatedly?
  let currentLevel = Lv $ sum (map (\(_, _, vars) -> length vars) quantifiedVariables)
  tensorDimensions <- calculateDimensionsOfQuantifiedVariable propertyState binder
  (envEntry, binderUserVars) <- calculateEnvEntry currentLevel variableName tensorDimensions
  let newEnv = extendEnv binder envEntry env

  let newQuantifiedVariable = (variableName, tensorDimensions, binderUserVars)
  let updatedQuantifiedVars = newQuantifiedVariable : quantifiedVariables

  normBody <- runNormT declSubst mempty (eval newEnv body)

  (substructure, allQuantifiedVariables) <- compileQueryStructure updatedQuantifiedVars normBody

  return (substructure, newQuantifiedVariable : allQuantifiedVariables)

compileSingleQuery ::
  (MonadCompileProperty m) =>
  QuantifiedVariables ->
  ConjunctAll StandardNormExpr ->
  m (MaybeTrivial (QueryAddress, (QueryMetaData, QueryText)))
compileSingleQuery quantifiedVariables conjuncts = do
  PropertyState {..} <- ask

  let (variableInfo, nestedNormalisedVars) =
        unzipWith (\(name, dims, vars) -> ((name, dims), vars)) quantifiedVariables
  let normalisedVars = concat nestedNormalisedVars

  logCompilerPass MinDetail "query" $ do
    -- Convert all user variables and applications of networks into magic I/O variables
    let boundCtx = fmap (Just . userVarName) normalisedVars
    (metaNetwork, networkVariables, inputEqualities, networkFreeConjuncts) <-
      replaceNetworkApplications networkCtx boundCtx conjuncts

    -- Convert it into a linear satisfaction problem in the user variables
    let state =
          ( networkCtx,
            (declIdentifier, declProvenance),
            metaNetwork,
            normalisedVars,
            networkVariables
          )
    clstQuery <- generateCLSTProblem state inputEqualities networkFreeConjuncts

    -- Compile the query to the specific verifiers.
    case clstQuery of
      Trivial b -> do
        logDebug MaxDetail $ "Query found to be trivially" <+> pretty b
        return $ Trivial b
      NonTrivial (clstProblem, normalisedVarInfo) -> do
        queryText <- compileQuery queryFormat clstProblem
        logCompilerPassOutput $ pretty queryText
        queryID <- demand
        let queryAddress = ((nameOf declIdentifier, propertyIndices), queryID)
        let queryVarInfo = QueryVariableInfo variableInfo normalisedVarInfo
        let queryData = QueryData metaNetwork queryVarInfo
        return $ NonTrivial (queryAddress, (queryData, queryText))

lowerNotNorm :: StandardNormExpr -> StandardNormExpr
lowerNotNorm arg = case arg of
  -- Base cases
  VBoolLiteral b -> VBoolLiteral (not b)
  VBuiltinFunction (Order dom ord) args -> VBuiltinFunction (Order dom (neg ord)) args
  VBuiltinFunction (Equals dom eq) args -> VBuiltinFunction (Equals dom (neg eq)) args
  VBuiltinFunction Not [e] -> e
  -- Inductive cases
  VBuiltinFunction Or args -> VBuiltinFunction And (lowerNotNorm <$> args)
  VBuiltinFunction And args -> VBuiltinFunction Or (lowerNotNorm <$> args)
  VQuantifierExpr q dom args binder env body ->
    let p = mempty
     in VQuantifierExpr (neg q) dom args binder env (NotExpr p [ExplicitArg p body])
  VBuiltinFunction If [c, e1, e2] -> VBuiltinFunction If [c, lowerNotNorm e1, lowerNotNorm e2]
  -- Errors
  e -> developerError ("Unable to lower 'not' through norm expr:" <+> prettyVerbose e)

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
temporaryUnsupportedAlternatingQuantifiersError :: CompileError
temporaryUnsupportedAlternatingQuantifiersError =
  UnsupportedAlternatingQuantifiers x x x x x
  where
    x = developerError "Evaluating temporary quantifier error"

--------------------------------------------------------------------------------
-- DNF

-- | A tree of expressions in disjunctive normal form.
type DNFTree = MaybeTrivial (DisjunctAll (ConjunctAll StandardNormExpr))

-- | Converts a boolean structure to disjunctive normal form.
convertToDNF :: PropositionTree -> DNFTree
convertToDNF = \case
  Disjunct e1 e2 ->
    orTrivial (convertToDNF e1) (convertToDNF e2) (<>)
  Conjunct e1 e2 -> do
    let xs = convertToDNF e1
    let ys = convertToDNF e2
    andTrivial xs ys $ \(DisjunctAll cs) (DisjunctAll ds) ->
      DisjunctAll $
        NonEmpty.fromList [as <> bs | as <- NonEmpty.toList cs, bs <- NonEmpty.toList ds]
  Query x -> case x of
    VBoolLiteral False -> Trivial False
    VBoolLiteral True -> Trivial True
    _ -> NonTrivial $ DisjunctAll [ConjunctAll [x]]

calculateDimensionsOfQuantifiedVariable ::
  forall m.
  (MonadCompile m) =>
  PropertyState ->
  StandardNormBinder ->
  m TensorDimensions
calculateDimensionsOfQuantifiedVariable propertyState binder = go (typeOf binder)
  where
    go :: StandardNormType -> m TensorDimensions
    go = \case
      VRatType {} -> return []
      VVectorType tElem (VNatLiteral dim) -> do
        dims <- go tElem
        return $ dim : dims
      variableType -> do
        let PropertyState {..} = propertyState
        let target = queryFormatID queryFormat
        let p = provenanceOf binder
        let baseName = getBinderName binder
        let baseType = binderType binder
        throwError $ UnsupportedVariableType target declIdentifier p baseName variableType baseType [BuiltinType Rat]

calculateEnvEntry ::
  (MonadCompile m) =>
  Lv ->
  Name ->
  TensorDimensions ->
  m (StandardNormExpr, [UserVariable])
calculateEnvEntry startingLevel baseName baseDims = do
  runSupplyT (go baseName baseDims) [startingLevel ..]
  where
    go ::
      (MonadSupply Lv m, MonadCompile m) =>
      Name ->
      TensorDimensions ->
      m (StandardNormExpr, [UserVariable])
    go name = \case
      [] -> do
        dbLevel <- demand
        return (VBoundVar dbLevel [], [UserVariable name])

      -- If we're quantifying over a tensor, instead quantify over each individual
      -- element, and then substitute in a LVec construct with those elements in.
      size : dims -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. size - 1]

        -- Generate the corresponding names from the indices
        let mkName i = name <> "_" <> pack (show i)
        (subexprs, userVars) <- unzip <$> traverse (\i -> go (mkName i) dims) allIndices

        let expr = mkVLVec subexprs

        -- Generate a expression prepended with `tensorSize` quantifiers
        return (expr, concat userVars)

pattern VQuantifierExpr :: Quantifier -> QuantifierDomain -> [StandardNormExpr] -> StandardNormBinder -> StandardEnv -> TypeCheckedExpr -> StandardNormExpr
pattern VQuantifierExpr q dom args binder env body <-
  VBuiltinFunction (Quantifier q dom) (reverse -> VLam binder env body : args)
  where
    VQuantifierExpr q dom args binder env body =
      VBuiltinFunction (Quantifier q dom) (reverse (VLam binder env body : args))

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
