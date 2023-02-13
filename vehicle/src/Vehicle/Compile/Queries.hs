module Vehicle.Compile.Queries
  ( compileToQueries,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (mapMaybe)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (pack)
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Compile.Queries.LinearSatisfactionProblem (generateCLSTProblem)
import Vehicle.Compile.Queries.LinearityAndPolarityErrors
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Queries.Variable (UserVariable (..))
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Patterns
import Vehicle.Expr.Boolean
import Vehicle.Expr.DeBruijn (DBLevel (..))
import Vehicle.Expr.Normalised
import Vehicle.Verify.Specification
import Vehicle.Verify.Verifier.Interface (Verifier (..))

--------------------------------------------------------------------------------
-- Compilation to individual queries

currentPass :: Doc a
currentPass = "compilation of properties"

-- | Compiles the provided program to invidividual queries suitable for a
-- verifier.
compileToQueries ::
  (MonadIO m, MonadCompile m) =>
  Verifier ->
  StandardGluedProg ->
  Resources ->
  m (VerificationPlan, VerificationQueries)
compileToQueries verifier typedProg resources =
  logCompilerPass MinDetail currentPass $ do
    properties <- compileProgToQueries verifier resources typedProg
    if null properties
      then throwError NoPropertiesFound
      else return $ NonEmpty.unzip $ Specification properties

--------------------------------------------------------------------------------
-- Getting properties

compileProgToQueries ::
  forall m.
  (MonadIO m, MonadCompile m) =>
  Verifier ->
  Resources ->
  GenericProg StandardGluedExpr ->
  m [(Identifier, Property (QueryMetaData, QueryText))]
compileProgToQueries verifier resources prog = do
  (networkCtx, expandedProg) <- expandResources resources prog
  let Main decls = expandedProg
  go networkCtx mempty decls
  where
    go ::
      NetworkContext ->
      DeclCtx StandardGluedDecl ->
      [StandardGluedDecl] ->
      m [(Identifier, Property (QueryMetaData, QueryText))]
    go _ _ [] = return []
    go networkCtx declCtx (d : ds) = case d of
      DefResource _ r _ _ -> normalisationError currentPass (pretty r <+> "declarations")
      DefPostulate {} -> normalisationError currentPass "postulates"
      DefFunction p ident isProperty typ body -> do
        maybeProperty <-
          if not isProperty
            then return Nothing
            else Just <$> compilePropertyDecl networkCtx declCtx p ident typ body

        properties <- go networkCtx (Map.insert ident d declCtx) ds

        return $ maybeToList maybeProperty ++ properties

    compilePropertyDecl :: NetworkContext -> DeclCtx StandardGluedDecl -> Provenance -> Identifier -> StandardGluedType -> StandardGluedExpr -> m (Identifier, Property (QueryMetaData, QueryText))
    compilePropertyDecl networkCtx declCtx p ident _typ expr = do
      logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
        let declSubst = mapMaybe (fmap normalised . bodyOf) declCtx
        let propertyCtx = (declSubst, verifier, (ident, p), networkCtx)
        let computeProperty = runReaderT (compileProperty (normalised expr)) propertyCtx
        property <-
          computeProperty `catchError` \e -> do
            let verifierIdent = verifierIdentifier verifier
            case e of
              UnsupportedNonLinearConstraint {} -> throwError =<< diagnoseNonLinearity verifierIdent prog ident
              UnsupportedAlternatingQuantifiers {} -> throwError =<< diagnoseAlternatingQuantifiers verifierIdent prog ident
              _ -> throwError e

        return (ident, property)

--------------------------------------------------------------------------------
-- Compilation

type PropertyReaderState = (DeclSubstitution StandardBuiltinType, Verifier, DeclProvenance, NetworkContext)

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyReaderState m
  )

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileProperty :: MonadCompileProperty m => StandardNormExpr -> m (Property (QueryMetaData, QueryText))
compileProperty = \case
  VVecLiteral es -> MultiProperty <$> traverse compileProperty es
  expr ->
    logCompilerSection MaxDetail "Starting single boolean property" $ do
      SingleProperty <$> compilePropertyTopLevelStructure expr

-- | Compiles the top-level structure of a property of type `Bool` until it
-- hits the first quantifier.
compilePropertyTopLevelStructure ::
  forall m.
  MonadCompileProperty m =>
  StandardNormExpr ->
  m (BoolProperty (QueryMetaData, QueryText))
compilePropertyTopLevelStructure = go
  where
    go :: StandardNormExpr -> m (BoolProperty (QueryMetaData, QueryText))
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
      _ -> unexpectedExprError "compiling top-level property structure" (prettyVerbose expr)

compileQuerySet ::
  MonadCompileProperty m =>
  Bool ->
  StandardNormExpr ->
  m (QuerySet (QueryMetaData, QueryText))
compileQuerySet isPropertyNegated expr = do
  -- First we recursively compile down the remaining boolean structure, stopping at
  -- the level of individual propositions (e.g. equality or ordering assertions)
  (propositionTree, userVariables) <-
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
      let flattenedUserVars = concat userVariables
      queries <- traverse (compileSingleQuery flattenedUserVars) dnf
      return $ eliminateTrivialDisjunctions queries

  return $ QuerySet isPropertyNegated queries

-- | This is a tree structure which stores the reverse environment at each
-- node and propositional expressions at the leaves.
type PropositionTree = BooleanExpr StandardNormExpr

-- | The set of variables that will be cumulatively in scope at the current
-- point in time once all existential quantifiers have been lifted to the
-- top level. Essentially the set of quantifiers that are before the current
-- point in the tree when traveresed depth first.
-- Outer list represents quantifier locations in the syntax. Inner lists represent the
-- number of user-variables bound by that quantifier (e.g. for Vectors)
type CumulativeVariables = [[UserVariable]]

cumulativeVarsToCtx :: CumulativeVariables -> BoundDBCtx
cumulativeVarsToCtx = concatMap (fmap (Just . userVarName))

compileQueryStructure ::
  forall m.
  MonadCompileProperty m =>
  CumulativeVariables ->
  StandardNormExpr ->
  m (PropositionTree, CumulativeVariables)
compileQueryStructure = go False
  where
    go :: Bool -> CumulativeVariables -> StandardNormExpr -> m (PropositionTree, CumulativeVariables)
    go processingLiftedIfs cumulativeVariables expr = case expr of
      VBuiltinFunction And [e1, e2] ->
        goOp2 Conjunct processingLiftedIfs cumulativeVariables e1 e2
      VBuiltinFunction Or [e1, e2] ->
        goOp2 Disjunct processingLiftedIfs cumulativeVariables e1 e2
      VBuiltinFunction Not [x] ->
        go processingLiftedIfs cumulativeVariables $ lowerNotNorm x
      VBuiltinFunction If [c, x, y] ->
        go processingLiftedIfs cumulativeVariables $ unfoldIf c x y
      VQuantifierExpr q dom _ binder env body -> case q of
        Exists -> compileQuantifierBodyToPropositionTree cumulativeVariables dom binder env body
        Forall -> throwError temporaryUnsupportedAlternatingQuantifiersError
      VBuiltinFunction Equals {} _ ->
        goProposition processingLiftedIfs cumulativeVariables expr
      VBuiltinFunction Order {} _ ->
        goProposition processingLiftedIfs cumulativeVariables expr
      VBoolLiteral {} -> do
        goProposition processingLiftedIfs cumulativeVariables expr
      _ -> unexpectedExprError "compiling query structure" (prettyVerbose expr)

    goOp2 ::
      (PropositionTree -> PropositionTree -> PropositionTree) ->
      Bool ->
      CumulativeVariables ->
      StandardNormExpr ->
      StandardNormExpr ->
      m (PropositionTree, CumulativeVariables)
    goOp2 op2 processingLiftedIfs cumulativeVariables e1 e2 = do
      let lhsVariables = cumulativeVariables
      (e1', lhsUserVars) <- go processingLiftedIfs lhsVariables e1
      let rhsVariables = lhsUserVars <> cumulativeVariables
      (e2', rhsUserVars) <- go processingLiftedIfs rhsVariables e2
      let userVars = lhsUserVars <> rhsUserVars
      return (op2 e1' e2', userVars)

    goProposition :: Bool -> CumulativeVariables -> StandardNormExpr -> m (PropositionTree, CumulativeVariables)
    goProposition processingLiftedIfs cumulativeVariables expr = do
      let ctx = cumulativeVarsToCtx cumulativeVariables
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
              else go True cumulativeVariables exprWithoutIf
          decrCallDepth
          return result

    wasIfLifted :: StandardNormExpr -> Bool
    wasIfLifted (VBuiltinFunction Or _) = True
    wasIfLifted _ = False

compileQuantifierBodyToPropositionTree ::
  MonadCompileProperty m =>
  CumulativeVariables ->
  QuantifierDomain ->
  StandardNormBinder ->
  StandardEnv ->
  TypeCheckedExpr ->
  m (PropositionTree, CumulativeVariables)
compileQuantifierBodyToPropositionTree cumulativeVariables _ binder env body = do
  -- TODO avoid calculating this repeatedly?
  let currentLevel = DBLevel $ sum (map length cumulativeVariables)
  (envEntry, binderUserVars) <- calculateEnvEntry currentLevel binder

  let updatedCumulativeVars = binderUserVars : cumulativeVariables
  let newEnv = extendEnv binder envEntry env

  -- Normalise the body of the expression
  (declCtx, _, _, _) <- ask
  normBody <- runNormT declCtx mempty (eval newEnv body)

  (substructure, allUserVars) <- compileQueryStructure updatedCumulativeVars normBody

  return (substructure, binderUserVars : allUserVars)

compileSingleQuery ::
  MonadCompileProperty m =>
  [UserVariable] ->
  ConjunctAll StandardNormExpr ->
  m (MaybeTrivial (QueryMetaData, QueryText))
compileSingleQuery userVariables conjuncts = do
  (_, verifier, ident, networkCtx) <- ask

  logCompilerPass MinDetail "query" $ do
    -- Convert all user variables and applications of networks into magic I/O variables
    let boundCtx = fmap (Just . userVarName) userVariables
    (metaNetwork, networkVariables, inputEqualities, networkFreeConjuncts) <-
      replaceNetworkApplications networkCtx boundCtx conjuncts

    -- Convert it into a linear satisfaction problem in the user variables
    let state =
          ( networkCtx,
            ident,
            metaNetwork,
            userVariables,
            networkVariables
          )
    clstQuery <- generateCLSTProblem state inputEqualities networkFreeConjuncts

    -- Compile the query to the specific verifiers.
    case clstQuery of
      Trivial b -> do
        logDebug MaxDetail $ "Query found to be trivially" <+> pretty b
        return $ Trivial b
      NonTrivial (clstProblem, u, v) -> do
        queryText <- compileQuery verifier clstProblem
        logCompilerPassOutput $ pretty queryText
        return $ NonTrivial (QueryData u v, queryText)

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

calculateEnvEntry :: MonadCompileProperty m => DBLevel -> StandardNormBinder -> m (StandardNormExpr, [UserVariable])
calculateEnvEntry startingLevel binder = do
  runSupplyT (go baseName baseType) [startingLevel ..]
  where
    baseName = getBinderName binder
    baseType = binderType binder

    go :: (MonadSupply DBLevel m, MonadCompileProperty m) => Name -> StandardNormType -> m (StandardNormExpr, [UserVariable])
    go name = \case
      VRatType {} -> do
        dbLevel <- demand
        return (VBoundVar dbLevel [], [UserVariable name])

      -- If we're quantifying over a tensor, instead quantify over each individual
      -- element, and then substitute in a LVec construct with those elements in.
      VVectorType tElem (VNatLiteral size) -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. size - 1]

        -- Generate the corresponding names from the indices
        let mkName i = name <> "_" <> pack (show i)
        (subexprs, userVars) <- unzip <$> traverse (\i -> go (mkName i) tElem) allIndices

        let expr = mkVLVec subexprs

        -- Generate a expression prepended with `tensorSize` quantifiers
        return (expr, concat userVars)
      typ -> do
        (_, verifier, (ident, _), _) <- ask
        let target = verifierIdentifier verifier
        let p = provenanceOf binder
        throwError $ UnsupportedVariableType target ident p baseName typ baseType [BuiltinType Rat]

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
