module Vehicle.Compile.Queries
  ( compileToQueries,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text (pack)
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources (expandResources)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Compile.Queries.LinearSatisfactionProblem (generateCLSTProblem)
import Vehicle.Compile.Queries.NetworkElimination
import Vehicle.Compile.Queries.Variable (UserVariable (..))
import Vehicle.Compile.Resource
import Vehicle.Compile.Type (getNormalised, getPropertyInfo)
import Vehicle.Compile.Type.VariableContext (DeclSubstitution)
import Vehicle.Expr.Boolean
import Vehicle.Expr.DeBruijn (DBLevel (..))
import Vehicle.Expr.Normalised
import Vehicle.Verify.Core
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
  TypedProg ->
  Resources ->
  m (VerificationPlan, VerificationQueries)
compileToQueries verifier@Verifier {..} typedProg resources =
  logCompilerPass MinDetail currentPass $ do
    (networkCtx, finalProg) <- expandResources resources typedProg
    properties <- getProperties verifierIdentifier finalProg

    if null properties
      then throwError NoPropertiesFound
      else do
        xs <- forM properties $ \(ctx, decl@(ident, _), expr) -> do
          logCompilerPass MinDetail ("property" <+> quotePretty ident) $ do
            let propertyCtx = (ctx, verifier, decl, networkCtx)
            property <- runReaderT (compileProperty expr) propertyCtx
            return (ident, property)

        return $ NonEmpty.unzip $ Specification xs

--------------------------------------------------------------------------------
-- Getting properties

getProperties ::
  MonadCompile m =>
  VerifierIdentifier ->
  GenericProg TypedExpr ->
  m [(DeclSubstitution, DeclProvenance, BasicNormExpr)]
getProperties verifier (Main decls) = go mempty decls
  where
    go ::
      MonadCompile m =>
      DeclSubstitution ->
      [TypedDecl] ->
      m [(DeclSubstitution, DeclProvenance, BasicNormExpr)]
    go _ [] = return []
    go ctx (d : ds) = case d of
      DefResource _ r _ _ -> normalisationError currentPass (pretty r <+> "declarations")
      DefPostulate {} -> normalisationError currentPass "postulates"
      DefFunction p ident isProperty _ body -> do
        normBody <- getNormalised body
        res <- go (Map.insert ident normBody ctx) ds
        if not isProperty
          then return res
          else do
            propertyInfo <- getPropertyInfo d
            let compatibility = checkCompatibility verifier (ident, p) propertyInfo
            case compatibility of
              Just err -> throwError err
              Nothing -> return $ (ctx, (ident, p), normBody) : res

--------------------------------------------------------------------------------
-- Compilation

type PropertyReaderState = (DeclSubstitution, Verifier, DeclProvenance, NetworkContext)

type MonadCompileProperty m =
  ( MonadCompile m,
    MonadReader PropertyReaderState m
  )

-- | Compiles a property of type `Tensor Bool dims` for some variable `dims`,
-- by recursing through the levels of vectors until it reaches something of
-- type `Bool`.
compileProperty :: MonadCompileProperty m => BasicNormExpr -> m (Property (QueryMetaData, QueryText))
compileProperty = \case
  VLVec es _ -> MultiProperty <$> traverse compileProperty es
  expr ->
    logCompilerSection MaxDetail "Starting single boolean property" $ do
      SingleProperty <$> compilePropertyTopLevelStructure expr

-- | Compiles the top-level structure of a property of type `Bool` until it
-- hits the first quantifier.
compilePropertyTopLevelStructure ::
  forall m.
  MonadCompileProperty m =>
  BasicNormExpr ->
  m (BoolProperty (QueryMetaData, QueryText))
compilePropertyTopLevelStructure = go
  where
    go :: BasicNormExpr -> m (BoolProperty (QueryMetaData, QueryText))
    go expr = case expr of
      VLiteral LBool {} ->
        Query <$> compileQuerySet False expr
      VBuiltinFunction Equals {} _ ->
        Query <$> compileQuerySet False expr
      VBuiltinFunction Order {} _ ->
        Query <$> compileQuerySet False expr
      VBuiltinFunction And [ExplicitArg _ e1, ExplicitArg _ e2] ->
        Conjunct <$> go e1 <*> go e2
      VBuiltinFunction Or [ExplicitArg _ e1, ExplicitArg _ e2] ->
        Disjunct <$> go e1 <*> go e2
      VBuiltinFunction Not [ExplicitArg _ x] ->
        go $ lowerNotNorm x
      VBuiltinFunction If [_tRes, c, x, y] ->
        go $ unfoldIf c x y
      VQuantifierExpr q dom args binder env body -> do
        let subsectionDoc = "Identified start of query set:" <+> prettyFriendly (WithContext expr emptyDBCtx)
        logCompilerSection MaxDetail subsectionDoc $ do
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
  BasicNormExpr ->
  m (QuerySet (QueryMetaData, QueryText))
compileQuerySet isPropertyNegated expr = do
  -- First we recursively compile down the remaining boolean structure, stopping at
  -- the level of individual propositions (e.g. equality or ordering assertions)
  (propositionTree, userVariables) <-
    compileQueryStructure mempty expr

  -- We then convert this tree into disjunctive normal form (DNF).
  logDebug MaxDetail "Converting to disjunctive normal form..."
  let dnfTree = convertToDNF propositionTree

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
type PropositionTree = BooleanExpr BasicNormExpr

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
  BasicNormExpr ->
  m (PropositionTree, CumulativeVariables)
compileQueryStructure = go False
  where
    go :: Bool -> CumulativeVariables -> BasicNormExpr -> m (PropositionTree, CumulativeVariables)
    go processingLiftedIfs cumulativeVariables expr = case expr of
      VBuiltinFunction And [ExplicitArg _ e1, ExplicitArg _ e2] ->
        goOp2 Conjunct processingLiftedIfs cumulativeVariables e1 e2
      VBuiltinFunction Or [ExplicitArg _ e1, ExplicitArg _ e2] ->
        goOp2 Disjunct processingLiftedIfs cumulativeVariables e1 e2
      VBuiltinFunction Not [ExplicitArg _ x] ->
        go processingLiftedIfs cumulativeVariables $ lowerNotNorm x
      VBuiltinFunction If [_tRes, c, x, y] ->
        go processingLiftedIfs cumulativeVariables $ unfoldIf c x y
      VQuantifierExpr Exists dom _ binder env body ->
        compileQuantifierBodyToPropositionTree cumulativeVariables dom binder env body
      VBuiltinFunction Equals {} _ ->
        goProposition processingLiftedIfs cumulativeVariables expr
      VBuiltinFunction Order {} _ ->
        goProposition processingLiftedIfs cumulativeVariables expr
      VLiteral LBool {} -> do
        goProposition processingLiftedIfs cumulativeVariables expr
      _ -> unexpectedExprError "compiling query structure" (prettyVerbose expr)

    goOp2 ::
      (PropositionTree -> PropositionTree -> PropositionTree) ->
      Bool ->
      CumulativeVariables ->
      BasicNormExpr ->
      BasicNormExpr ->
      m (PropositionTree, CumulativeVariables)
    goOp2 op2 processingLiftedIfs cumulativeVariables e1 e2 = do
      let lhsVariables = cumulativeVariables
      (e1', lhsUserVars) <- go processingLiftedIfs lhsVariables e1
      let rhsVariables = lhsUserVars <> cumulativeVariables
      (e2', rhsUserVars) <- go processingLiftedIfs rhsVariables e2
      let userVars = lhsUserVars <> rhsUserVars
      return (op2 e1' e2', userVars)

    goProposition :: Bool -> CumulativeVariables -> BasicNormExpr -> m (PropositionTree, CumulativeVariables)
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

    wasIfLifted :: BasicNormExpr -> Bool
    wasIfLifted (VBuiltinFunction Or _) = True
    wasIfLifted _ = False

compileQuantifierBodyToPropositionTree ::
  MonadCompileProperty m =>
  CumulativeVariables ->
  QuantifierDomain ->
  BasicNormBinder ->
  BasicEnv ->
  CheckedExpr ->
  m (PropositionTree, CumulativeVariables)
compileQuantifierBodyToPropositionTree cumulativeVariables _ binder env body = do
  -- TODO avoid calculating this repeatedly?
  let currentLevel = DBLevel $ sum (map length cumulativeVariables)
  (envEntry, binderUserVars) <- calculateEnvEntry currentLevel binder

  let updatedCumulativeVars = binderUserVars : cumulativeVariables
  let newEnv = envEntry : env

  -- Normalise the body of the expression
  (declCtx, _, _, _) <- ask
  normBody <- runReaderT (eval newEnv body) (declCtx, mempty)

  (substructure, allUserVars) <- compileQueryStructure updatedCumulativeVars normBody

  return (substructure, binderUserVars : allUserVars)

compileSingleQuery ::
  MonadCompileProperty m =>
  [UserVariable] ->
  ConjunctAll BasicNormExpr ->
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

lowerNotNorm :: BasicNormExpr -> BasicNormExpr
lowerNotNorm arg = case arg of
  -- Base cases
  VLiteral (LBool b) -> VLiteral (LBool (not b))
  VBuiltinFunction (Order dom ord) args -> VBuiltinFunction (Order dom (neg ord)) args
  VBuiltinFunction (Equals dom eq) args -> VBuiltinFunction (Equals dom (neg eq)) args
  VBuiltinFunction Not [e] -> argExpr e
  -- Inductive cases
  VBuiltinFunction Or args -> VBuiltinFunction And (lowerNotNormArg <$> args)
  VBuiltinFunction And args -> VBuiltinFunction Or (lowerNotNormArg <$> args)
  VQuantifierExpr q dom args binder env body ->
    let p = mempty
     in VQuantifierExpr (neg q) dom args binder env (NotExpr p [ExplicitArg p body])
  VBuiltinFunction If [tRes, c, e1, e2] -> VBuiltinFunction If [tRes, c, lowerNotNormArg e1, lowerNotNormArg e2]
  -- Errors
  e -> developerError ("Unable to lower 'not' through norm expr:" <+> prettyVerbose e)

lowerNotNormArg :: BasicNormArg -> BasicNormArg
lowerNotNormArg = fmap lowerNotNorm

--------------------------------------------------------------------------------
-- DNF

-- | A tree of expressions in disjunctive normal form.
type DNFTree = MaybeTrivial (DisjunctAll (ConjunctAll BasicNormExpr))

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

checkCompatibility :: VerifierIdentifier -> DeclProvenance -> PropertyInfo -> Maybe CompileError
checkCompatibility verifier prov (PropertyInfo linearity polarity) =
  case (linearity, polarity) of
    (NonLinear p pp1 pp2, _) ->
      Just $ UnsupportedNonLinearConstraint (VerifierBackend verifier) prov p pp1 pp2
    (_, MixedSequential q p pp2) ->
      Just $ UnsupportedAlternatingQuantifiers (VerifierBackend verifier) prov q p pp2
    _ -> Nothing

calculateEnvEntry :: MonadCompileProperty m => DBLevel -> BasicNormBinder -> m (BasicNormExpr, [UserVariable])
calculateEnvEntry startingLevel binder = do
  runSupplyT (go baseName baseType) [startingLevel ..]
  where
    baseName = getBinderName binder
    baseType = binderType binder

    go :: (MonadSupply DBLevel m, MonadCompileProperty m) => Name -> BasicNormType -> m (BasicNormExpr, [UserVariable])
    go name = \case
      VRatType {} -> do
        dbLevel <- demand
        return (VBoundVar dbLevel [], [UserVariable name])

      -- If we're quantifying over a tensor, instead quantify over each individual
      -- element, and then substitute in a LVec construct with those elements in.
      VVectorType tElem (VLiteral (LNat size)) -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. size - 1]

        -- Generate the corresponding names from the indices
        let mkName i = name <> "_" <> pack (show i)
        (subexprs, userVars) <- unzip <$> traverse (\i -> go (mkName i) tElem) allIndices

        let expr = VLVec subexprs [ImplicitArg mempty tElem]

        -- Generate a expression prepended with `tensorSize` quantifiers
        return (expr, concat userVars)
      typ -> do
        (_, verifier, (ident, _), _) <- ask
        let target = verifierIdentifier verifier
        let p = provenanceOf binder
        throwError $ UnsupportedVariableType target ident p baseName typ baseType [Constructor Rat]

pattern VQuantifierExpr :: Quantifier -> QuantifierDomain -> [BasicNormArg] -> BasicNormBinder -> BasicEnv -> CheckedExpr -> BasicNormExpr
pattern VQuantifierExpr q dom args binder env body <-
  VBuiltinFunction (Quantifier q dom) (reverse -> ExplicitArg _ (VLam binder env body) : args)
  where
    VQuantifierExpr q dom args binder env body =
      VBuiltinFunction (Quantifier q dom) (reverse (ExplicitArg mempty (VLam binder env body) : args))
