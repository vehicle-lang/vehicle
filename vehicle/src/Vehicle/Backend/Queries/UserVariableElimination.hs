{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.UserVariableElimination
  ( eliminateUserVariables,
    catchableUnsupportedNonLinearConstraint,
  )
where

-- Need to import this qualified as in GHC 9.6 and above liftA2 is part of Prelude
-- and therefore importing it normally gives us an "Unused import warning" on
-- 9.6 and above, but not earlier versions.
import Control.Applicative qualified as Applicative (liftA2)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Foldable (foldrM)
import Data.IntSet qualified as IntSet
import Data.List (elemIndex, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as Vector
import Vehicle.Backend.Queries.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Backend.Queries.GaussianElimination
  ( gaussianElimination,
  )
import Vehicle.Backend.Queries.IfElimination (eliminateIfs, unfoldIf)
import Vehicle.Backend.Queries.LinearExpr
import Vehicle.Backend.Queries.QuerySetStructure (eliminateNot)
import Vehicle.Backend.Queries.Variable
import Vehicle.Compile.Context.Free (MonadFreeContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalMul)
import Vehicle.Compile.Normalise.NBE (defaultNBEOptions, reeval)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BooleanExpr
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.DeBruijn
import Vehicle.Data.NormalisedExpr
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (..), pattern TensorIdent)
import Vehicle.Prelude.Warning (CompileWarning (ResortingtoFMElimination))
import Vehicle.Verify.Core

-- | Takes in a set of unreduced user and network variables and a boolean expression
-- over those variables, and returns a list of assertions only over the fully reduced
-- network variables.
eliminateUserVariables ::
  (MonadCompile m, MonadFreeContext Builtin m) =>
  DeclProvenance ->
  MetaNetwork ->
  MixedVariables ->
  BooleanExpr UnreducedAssertion ->
  m (MaybeTrivial (DisjunctAll (CLSTProblem, VariableNormalisationSteps)))
eliminateUserVariables declProvenance metaNetwork variables expr =
  flip runReaderT (declProvenance, metaNetwork) $ do
    -- The first step is to try to solve for as many user variables as possible
    -- without performing any reduction of any vector variables.
    unreducedSolverResult <- tryToSolveForUnreducedUserVariables variables expr
    case unreducedSolverResult of
      Trivial b -> return $ Trivial b
      NonTrivial (vectorVariableSolutions, unsolvableAssertions, unreducedSolutionSteps) -> do
        -- We then fully reduce the remaining variables to variables over rationals.
        reductionResult <- reduceRemainingVariables variables vectorVariableSolutions unsolvableAssertions
        case reductionResult of
          Trivial b -> return $ Trivial b
          NonTrivial (reducedVariables, reducedUncompiledAssertions, normalisationSteps) -> do
            logDebug MaxDetail "Converting to DNF"
            let dnfExpr = exprToDNF reducedUncompiledAssertions
            case dnfExpr of
              Trivial b -> return $ Trivial b
              NonTrivial disjuncts -> do
                logDebug MaxDetail $ "Found" <+> pretty (length disjuncts) <+> "queries" <> line
                results <- for disjuncts $ \conjuncts -> do
                  -- We then solve for the remaining reduced user variables
                  (networkVariableAssertions, reducedSolutionSteps) <-
                    solveForReducedUserVariables reducedVariables conjuncts

                  -- Finally we check that the problem is still non-trivial
                  let maybeAssertions = filterTrivialAssertions networkVariableAssertions
                  return $ case maybeAssertions of
                    Nothing -> Trivial False
                    Just [] -> Trivial True
                    Just (a : as) -> do
                      let reconstructionSteps = unreducedSolutionSteps <> normalisationSteps <> reducedSolutionSteps
                      NonTrivial (CLSTProblem (networkVariableCtx reducedVariables) (a : as), reconstructionSteps)

                return $ eliminateTrivialDisjunctions results

type MonadSMT m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadReader LCSState m
  )

type LCSState =
  ( DeclProvenance,
    MetaNetwork
  )

--------------------------------------------------------------------------------
-- Solving of unreduced assertions

-- | Solutions for vector-level user variables. They are ordered so that
-- earlier solutions may depend on solved variables later in the list.
type UnreducedVariableSolutions = [(UserVariable, WHNFValue Builtin)]

type SolvableAssertion = Assertion MixedVariable

-- | Tries to optimistically solve for the user variables in an unreduced state.
-- This is not guaranteed to succeed, as higher order functions are not
-- guaranteed to be fully reduced and there may still be lookups present.
tryToSolveForUnreducedUserVariables ::
  (MonadSMT m) =>
  MixedVariables ->
  BooleanExpr UnreducedAssertion ->
  m (MaybeTrivial (UnreducedVariableSolutions, BooleanExpr UnreducedAssertion, VariableNormalisationSteps))
tryToSolveForUnreducedUserVariables variables expr =
  logCompilerPass MidDetail "attempt to solve for unreduced user variables" $ do
    logDebug MidDetail $ "There are" <+> pretty (length expr) <+> "assertions..."

    -- Split out the equalities from the inequalities.
    (solvableEqualities, remainingExpr) <- extractSolvableVectorEqualities (mixedVariableCtx variables) expr

    logDebug MidDetail $
      "... of which"
        <+> pretty (length solvableEqualities)
        <+> "are solvable vector equalities."
        <> line

    if null solvableEqualities
      then return $ NonTrivial (mempty, expr, mempty)
      else do
        -- Try to solve the solvable equalities using Gaussian elimination.
        let userVars = UserVar <$> userVariableCtx variables
        (gaussianSolutions, _, usedEqualityIDs) <-
          gaussianElimination userVars (fmap (assertionExpr . snd) solvableEqualities)
        let gaussianReconstructionSteps = fmap mkGaussianReconstructionStep gaussianSolutions
        logDebug MidDetail $ "Eliminated user variables:" <+> pretty (fmap fst gaussianSolutions)

        -- Calculate the updated set of assertions.
        let unusedEqualities = filterByIndex (`IntSet.notMember` usedEqualityIDs) (fmap fst solvableEqualities)
        let remainingEqualityExpr = case unusedEqualities of
              [] -> Nothing
              a : as -> Just $ foldr (Conjunct . Query) (Query a) as
        let maybeFinalExpr = unionMaybeWith Conjunct remainingEqualityExpr remainingExpr

        case maybeFinalExpr of
          Nothing -> return $ Trivial True
          Just finalExpr ->
            NonTrivial <$> do
              -- Substitute the solutions through the remaining set of solutions.
              let varsSolved = mapMaybe (\(v, s) -> (,s) <$> getUserVariable v) gaussianSolutions
              solutions <- traverse (solutionToExpr (mixedVariableCtx variables)) varsSolved
              return (solutions, finalExpr, gaussianReconstructionSteps)

-- | Tries to extract any vector level equalities that are suitable for plugging into
-- Gaussian elimination.
extractSolvableVectorEqualities ::
  forall m.
  (MonadSMT m) =>
  MixedVariableCtx ->
  BooleanExpr UnreducedAssertion ->
  m ([(UnreducedAssertion, SolvableAssertion)], Maybe (BooleanExpr UnreducedAssertion))
extractSolvableVectorEqualities mixedVariables = go
  where
    go ::
      BooleanExpr UnreducedAssertion ->
      m ([(UnreducedAssertion, SolvableAssertion)], Maybe (BooleanExpr UnreducedAssertion))
    go expr = case expr of
      -- When we hit a query we need to check if it's actually a solvable vector equality
      Query assertion -> case assertion of
        NonVectorEqualityAssertion {} -> return ([], Just (Query assertion))
        VectorEqualityAssertion vecEquality -> do
          maybeSolvableAssertion <- compileVectorEquality mixedVariables vecEquality
          case maybeSolvableAssertion of
            Nothing -> return ([], Just (Query assertion))
            Just solvableVectorEquality -> return ([(assertion, solvableVectorEquality)], Nothing)
      -- When we hit a conjunction we just recurse into it's branches
      Conjunct e1 e2 -> do
        (cs1, e1') <- go e1
        (cs2, e2') <- go e2
        return (cs1 <> cs2, unionMaybeWith Conjunct e1' e2')
      -- Note when we hit a disjunct we don't proceed any further as the equalities
      -- are not universally applicable. One potential future optimisation
      -- could be to further convert to DNF here.
      Disjunct {} ->
        return ([], Just expr)

-- | Converts a Gaussian elimination solution for a user variable back to a
-- Vehicle expression.
solutionToExpr ::
  (MonadCompile m) =>
  MixedVariableCtx ->
  (UserVariable, SparseLinearExpr MixedVariable) ->
  m (UserVariable, WHNFValue Builtin)
solutionToExpr variables (var, Sparse {..}) = do
  let findVarIx v = Ix $ fromMaybe (developerError ("Variable" <+> pretty var <+> "not found")) (v `elemIndex` variables)
  let toExprVar v = VBoundVar (dbIndexToLevel (Lv $ length variables) (findVarIx v)) []
  let addFn e1 e2 = mkRatVectorAdd (VNatLiteral <$> dimensions) [e1, e2]
  let subFn e1 e2 = mkRatVectorSub (VNatLiteral <$> dimensions) [e1, e2]
  -- Need to negate the coefficients as we're rearranging the equation.
  let combFn (v, c) e
        | v == UserVar var = e
        | c == 1.0 = subFn e (toExprVar v)
        | c == -1.0 = addFn e (toExprVar v)
        | otherwise = developerError "Vector equality coefficients should currently all be magnitude 1.0"
  let constant = constantExpr dimensions (Vector.map (* (-1)) constantValue)
  let varCoeffList = Map.toList coefficients
  return (var, foldr combFn constant varCoeffList)
  where
    mkTensorType :: WHNFType Builtin -> WHNFType Builtin -> WHNFType Builtin
    mkTensorType tElem dims = VFreeVar TensorIdent [Arg mempty Explicit Relevant tElem, Arg mempty Explicit Irrelevant dims]

    mkRatVectorAdd :: [WHNFValue Builtin] -> [WHNFValue Builtin] -> WHNFValue Builtin
    mkRatVectorAdd = mkVectorOp (Add AddRat) StdAddVector

    mkRatVectorSub :: [WHNFValue Builtin] -> [WHNFValue Builtin] -> WHNFValue Builtin
    mkRatVectorSub = mkVectorOp (Sub SubRat) StdSubVector

    mkVectorOp ::
      BuiltinFunction ->
      StdLibFunction ->
      [WHNFValue Builtin] ->
      [WHNFValue Builtin] ->
      WHNFValue Builtin
    mkVectorOp baseOp libOp dims spine = case dims of
      [] -> VBuiltinFunction baseOp (Arg mempty Explicit Relevant <$> spine)
      (d : ds) ->
        VFreeVar
          (identifierOf libOp)
          ( [ Arg p (Implicit True) Relevant vecType,
              Arg p (Implicit True) Relevant vecType,
              Arg p (Implicit True) Relevant vecType,
              Arg p (Implicit True) Irrelevant d,
              Arg p (Instance True) Relevant (mkVectorOp baseOp libOp ds [])
            ]
              <> fmap (Arg p Explicit Relevant) spine
          )
        where
          p = mempty; vecType = mkTensorType VRatType (mkVList ds)

compileVectorEquality ::
  (MonadSMT m) =>
  MixedVariableCtx ->
  VectorEquality ->
  m (Maybe SolvableAssertion)
compileVectorEquality variables VectorEquality {..} = do
  lhsLinExpr <- compilerVectorLinearExpr variables assertionDims assertionLHS
  rhsLinExpr <- compilerVectorLinearExpr variables assertionDims assertionRHS
  case (lhsLinExpr, rhsLinExpr) of
    (Just lhs, Just rhs) -> do
      let reducedAssertion = constructReducedAssertion (lhs, Equal, rhs)
      return $ Just reducedAssertion
    _ -> return Nothing

compilerVectorLinearExpr ::
  forall m.
  (MonadSMT m) =>
  MixedVariableCtx ->
  TensorDimensions ->
  WHNFValue Builtin ->
  m (Maybe (SparseLinearExpr MixedVariable))
compilerVectorLinearExpr variables dimensions = go
  where
    go :: WHNFValue Builtin -> m (Maybe (SparseLinearExpr MixedVariable))
    go e = case e of
      VBoundVar v [] ->
        Just <$> singletonVar v 1
      (isConstant -> Just constant) -> do
        return $ Just $ Sparse [] mempty constant
      (isAddVector -> Just (e1, e2)) -> do
        l1 <- go e1
        l2 <- go e2
        return $ Applicative.liftA2 (\x y -> addExprs 1 x 1 y) l1 l2
      (isSubVector -> Just (e1, e2)) -> do
        l1 <- go e1
        l2 <- go e2
        return $ Applicative.liftA2 (\x y -> addExprs 1 x (-1) y) l1 l2
      _ ->
        return Nothing

    singletonVar :: Lv -> Coefficient -> m (SparseLinearExpr MixedVariable)
    singletonVar level coef = do
      var <- lookupLvInBoundCtx currentPass level variables
      let constant = Vector.replicate (product dimensions) 0
      return $ Sparse dimensions (Map.singleton var coef) constant

    isAddVector :: WHNFValue Builtin -> Maybe (WHNFValue Builtin, WHNFValue Builtin)
    isAddVector = \case
      VFreeVar ident [_, _, _, _, _, e1, e2]
        | ident == identifierOf StdAddVector -> Just (argExpr e1, argExpr e2)
      _ -> Nothing

    isSubVector :: WHNFValue Builtin -> Maybe (WHNFValue Builtin, WHNFValue Builtin)
    isSubVector = \case
      VFreeVar ident [_, _, _, _, _, e1, e2]
        | ident == identifierOf StdSubVector -> Just (argExpr e1, argExpr e2)
      _ -> Nothing

    isConstant :: WHNFValue Builtin -> Maybe Constant
    isConstant = \case
      VVecLiteral xs -> mconcat <$> traverse (isConstant . argExpr) xs
      VRatLiteral r -> Just $ Vector.singleton (fromRational r)
      _ -> Nothing

--------------------------------------------------------------------------------
-- Variable reduction

reduceRemainingVariables ::
  (MonadSMT m) =>
  MixedVariables ->
  UnreducedVariableSolutions ->
  BooleanExpr UnreducedAssertion ->
  m (MaybeTrivial (MixedVariables, BooleanExpr SolvableAssertion, VariableNormalisationSteps))
reduceRemainingVariables variables userVariableSolutions assertions =
  logCompilerPass MidDetail "reduction of remaining unreduced user variables and assertions" $ do
    -- So this is a little complex. We want to substitute all the reduced variables
    -- through the assertions in one fell-swoop. However, to do so efficiently we need
    -- to first substitute them through the vector variable solutions, otherwise we end up
    -- normalising the solutions over and over again during substitution.
    let solvedUserVariables = Set.fromList (fmap fst userVariableSolutions)

    -- First reduce the variables
    (reducedVariables, reconstructionSteps, partialReducedVarEnv, solvedUserVariableIndices) <-
      reduceVariables variables solvedUserVariables

    -- Subsitute through the solutions
    reducedVarEnv <- substituteReducedVariablesThroughSolutions partialReducedVarEnv userVariableSolutions solvedUserVariableIndices

    -- Then reduce the assertions
    logDebug MidDetail "Substituting reduced variables through assertions..."
    let reduceAssertion = compileReducedAssertion reducedVariables reducedVarEnv
    compiledAssertions <- traverse reduceAssertion assertions

    case eliminateTrivialAtoms compiledAssertions of
      Trivial b -> return $ Trivial b
      NonTrivial finalAssertions -> do
        let finalExpr = concatBooleanExpr finalAssertions
        logDebug MaxDetail $ line <> "Reduced assertions:"
        logDebug MaxDetail $ indent 2 $ pretty finalExpr
        return $ NonTrivial (reducedVariables, finalExpr, reverse reconstructionSteps)

reduceVariables ::
  (MonadCompile m) =>
  MixedVariables ->
  Set UserVariable ->
  m (MixedVariables, VariableNormalisationSteps, WHNFEnv Builtin, Map UserVariable Ix)
reduceVariables variables solvedUserVariables = do
  logDebug MidDetail $ "Reducing unsolved variables..." <> line
  (userVarLv, reducedUserVariables, userVarReconstructionSteps, reducedUserVarEnv, solvedUserVariableIndices) <-
    foldrM (possiblyReduceVariable solvedUserVariables) (0, mempty, mempty, mempty, mempty) (userVariableCtx variables)

  (_, reducedNetworkVariables, allReconstructionSteps, reducedVarEnv, _) <-
    foldrM (possiblyReduceVariable mempty) (userVarLv, [], userVarReconstructionSteps, reducedUserVarEnv, mempty) (networkVariableCtx variables)

  let finalSolvedUserVariableIndices = fmap (dbLevelToIndex (Lv $ length reducedVarEnv)) solvedUserVariableIndices
  let reducedVariables = MixedVariables reducedUserVariables reducedNetworkVariables
  return (reducedVariables, allReconstructionSteps, reducedVarEnv, finalSolvedUserVariableIndices)
  where
    possiblyReduceVariable ::
      (MonadCompile m, Variable variable) =>
      Set variable ->
      variable ->
      (Lv, GenericBoundCtx variable, VariableNormalisationSteps, WHNFEnv Builtin, Map variable Lv) ->
      m (Lv, GenericBoundCtx variable, VariableNormalisationSteps, WHNFEnv Builtin, Map variable Lv)
    possiblyReduceVariable solvedVariables var (currentLv, reducedVariables, steps, subst, solvedIndices)
      | var `Set.member` solvedVariables = do
          logDebug MaxDetail $
            "Variable"
              <+> quotePretty var
              <+> "not reduced as previously solved"
              <> line
          let newExpr = VFreeVar (Identifier User (layoutAsText ("Should not appear - SOLVED" <+> pretty var))) []
          let newEnv = mkDefaultBinder varName newExpr : subst
          let newSolvedIndices = Map.insert var (Lv $ length subst) solvedIndices
          return (currentLv, reducedVariables, steps, newEnv, newSolvedIndices)
      | isRationalVariable var = do
          logDebug MaxDetail $ "Variable" <+> quotePretty var <+> "already fully reduced" <> line
          let newNormVariables = var : reducedVariables
          let newExpr = VBoundVar (Lv $ length subst) []
          let newEnv = mkDefaultBinder varName newExpr : subst
          return (currentLv + 1, newNormVariables, steps, newEnv, solvedIndices)
      | otherwise = do
          let (newVars, newExpr) = reduceVariable currentLv var
          logDebug MaxDetail $ "Variable" <+> quotePretty var <+> "reduced to" <> line <> indent 2 (pretty newVars) <> line
          let newNormVariables = newVars <> reducedVariables
          let newEnv = mkDefaultBinder varName newExpr : subst
          return (currentLv + Lv (length newVars), newNormVariables, Reduce (toMixedVariable var) : steps, newEnv, solvedIndices)
      where
        varName = layoutAsText $ pretty var

substituteReducedVariablesThroughSolutions ::
  forall m.
  (MonadSMT m) =>
  WHNFEnv Builtin ->
  UnreducedVariableSolutions ->
  Map UserVariable Ix ->
  m (WHNFEnv Builtin)
substituteReducedVariablesThroughSolutions partialEnv solutions solvedVariablePositions = do
  logDebug MidDetail "Substituting reduced variables through variable solutions..."
  foldrM f partialEnv solutions
  where
    f :: (UserVariable, WHNFValue Builtin) -> WHNFEnv Builtin -> m (WHNFEnv Builtin)
    f (var, solution) env = do
      normalisedSolution <- reeval defaultNBEOptions env solution
      let errorMsg = developerError $ "Environment index missing for solved variable" <+> quotePretty var
      let index = unIx $ fromMaybe errorMsg $ Map.lookup var solvedVariablePositions
      let newEnv = take index env <> [mkDefaultBinder (layoutAsText $ pretty var) normalisedSolution] <> drop (index + 1) env
      return newEnv

--------------------------------------------------------------------------------
-- Solving of unreduced assertions

solveForReducedUserVariables ::
  (MonadSMT m) =>
  MixedVariables ->
  ConjunctAll SolvableAssertion ->
  m ([Assertion NetworkVariable], VariableNormalisationSteps)
solveForReducedUserVariables variables assertions =
  logCompilerPass MidDetail "elimination of user variables" $ do
    ((ident, _), _) <- ask

    let userVars = fmap UserVar (userVariableCtx variables)
    let userVariablesSet = Set.fromList userVars

    -- First remove those assertions that don't have any user variables in them.
    let (withUserVars, withoutUserVars) =
          partition (`referencesVariables` userVariablesSet) (conjunctsToList assertions)

    -- Then split out the equalities from the inequalities.
    let (equalitiesWithUserVars, inequalitiesWithUserVars) =
          partition isEquality withUserVars

    -- Try to solve for user variables using Gaussian elimination.
    (gaussianSolutions, unusedEqualityExprs, _usedEqualityIDs) <-
      gaussianElimination userVars (map assertionExpr equalitiesWithUserVars)
    let gaussianReconstructionSteps = fmap mkGaussianReconstructionStep gaussianSolutions
    let equalitiesNewlyWithoutUserVars = fmap (Assertion Equal) unusedEqualityExprs

    -- Eliminate the solved user variables in the inequalities
    let reducedInequalities =
          flip fmap inequalitiesWithUserVars $ \assertion ->
            foldr (flip $ uncurry . substitute) assertion gaussianSolutions

    -- Calculate the set of unsolved user variables
    let varsSolvedByGaussianElim = Set.fromList (fmap fst gaussianSolutions)
    let varsUnsolvedByGaussianElim = Set.difference userVariablesSet varsSolvedByGaussianElim

    (inequalitiesNewlyWithoutUserVars, fourierMotzkinSteps) <-
      if null varsUnsolvedByGaussianElim
        then return (reducedInequalities, mempty)
        else do
          logWarning $ ResortingtoFMElimination (nameOf ident) varsUnsolvedByGaussianElim

          -- Eliminate the remaining unsolved user vars using Fourier-Motzkin elimination
          (fourierMotzkinSolutions, fmElimOutputInequalities) <-
            fourierMotzkinElimination varsUnsolvedByGaussianElim reducedInequalities
          let eliminationSteps = fmap (uncurry EliminateViaFourierMotzkin) fourierMotzkinSolutions
          return (fmElimOutputInequalities, eliminationSteps)

    -- Calculate the final set of (user-variable free) assertions
    let allAssertions = withoutUserVars <> equalitiesNewlyWithoutUserVars <> inequalitiesNewlyWithoutUserVars
    let reconstructionSteps = gaussianReconstructionSteps <> fourierMotzkinSteps

    let uneliminatedVarError v =
          developerError $ "User variable" <+> quotePretty v <+> "not successfully eliminated in property" <+> quotePretty ident
    let toNetworkVar v = fromMaybe (uneliminatedVarError v) (getNetworkVariable v)
    let networkVarAssertions = fmap (mapAssertionVariables toNetworkVar) allAssertions

    -- Eliminate network variables
    return (networkVarAssertions, reconstructionSteps)

mkGaussianReconstructionStep :: (MixedVariable, SparseLinearExpr MixedVariable) -> VariableNormalisationStep
mkGaussianReconstructionStep (v, e) =
  EliminateViaGaussian v (GaussianVariableSolution $ rearrangeExprToSolveFor v e)

--------------------------------------------------------------------------------
-- Compilation of fully reduced assertions

compileReducedAssertion ::
  (MonadSMT m) =>
  MixedVariables ->
  WHNFEnv Builtin ->
  UnreducedAssertion ->
  m (MaybeTrivial (BooleanExpr SolvableAssertion))
compileReducedAssertion variables variableSubstEnv assertion = do
  let assertionExpr = case assertion of
        VectorEqualityAssertion vectorEquality -> originalVectorEqualityExpr vectorEquality
        NonVectorEqualityAssertion expr -> expr

  -- First normalise the expression under the new environment of reduced variables
  normExpr <- reeval defaultNBEOptions variableSubstEnv assertionExpr

  -- Then extract the relation and arguments
  splitAssertions <- splitUpAssertions False normExpr
  -- Then reduce the assertions
  traverse (traverse reduceAssertion) splitAssertions
  where
    variableCtx :: MixedVariableCtx
    variableCtx = mixedVariableCtx variables

    splitUpAssertions ::
      (MonadSMT m) =>
      Bool ->
      WHNFValue Builtin ->
      m (MaybeTrivial (BooleanExpr (WHNFValue Builtin, Relation, WHNFValue Builtin)))
    splitUpAssertions alreadyLiftedIfs expr = case expr of
      VBoolLiteral b -> return $ Trivial b
      VBuiltinFunction And [e1, e2] -> do
        ass1 <- splitUpAssertions alreadyLiftedIfs (argExpr e1)
        ass2 <- splitUpAssertions alreadyLiftedIfs (argExpr e2)
        return $ andTrivial Conjunct ass1 ass2
      VBuiltinFunction Or [e1, e2] -> do
        ass1 <- splitUpAssertions alreadyLiftedIfs (argExpr e1)
        ass2 <- splitUpAssertions alreadyLiftedIfs (argExpr e2)
        return $ orTrivial Disjunct ass1 ass2
      VBuiltinFunction Not [e] -> case eliminateNot (argExpr e) of
        -- This should always work at this stage.
        Nothing -> compilerDeveloperError $ "Cannot eliminate 'not' over" <+> prettyVerbose e
        Just result -> splitUpAssertions alreadyLiftedIfs result
      VBuiltinFunction If [_, c, x, y] -> do
        -- As the expression is of type `Bool` we can immediately unfold the `if`.
        let unfoldedExpr = unfoldIf c (argExpr x) (argExpr y)
        splitUpAssertions alreadyLiftedIfs unfoldedExpr
      VBuiltinFunction (Equals _ Eq) [e1, e2]
        | alreadyLiftedIfs -> return $ NonTrivial $ Query (argExpr e1, Equal, argExpr e2)
        | otherwise -> liftIfs expr
      VBuiltinFunction (Order _ op) [e1, e2]
        | alreadyLiftedIfs -> return $ NonTrivial $ Query $ ordToRelation (argExpr e1) op (argExpr e2)
        | otherwise -> liftIfs expr
      _ ->
        unexpectedExprError "compiling reduced assertion" (prettyVerbose expr)

    liftIfs ::
      (MonadSMT m) =>
      WHNFValue Builtin ->
      m (MaybeTrivial (BooleanExpr (WHNFValue Builtin, Relation, WHNFValue Builtin)))
    liftIfs expr = do
      maybeExprWithoutIf <- eliminateIfs expr
      case maybeExprWithoutIf of
        Nothing -> splitUpAssertions True expr
        Just Nothing -> compilerDeveloperError $ "Cannot lift 'if' over" <+> prettyVerbose expr
        Just (Just exprWithoutIf) -> do
          let env = variableCtxToEnv variableCtx
          normExprWithoutIf <- reeval defaultNBEOptions env exprWithoutIf
          splitUpAssertions True normExprWithoutIf

    reduceAssertion ::
      (MonadSMT m) =>
      (WHNFValue Builtin, Relation, WHNFValue Builtin) ->
      m SolvableAssertion
    reduceAssertion (lhs, rel, rhs) = do
      lhsLinExpr <- compileReducedLinearExpr variableCtx lhs
      rhsLinExpr <- compileReducedLinearExpr variableCtx rhs
      -- And construct the reduced assertion
      return $ constructReducedAssertion (lhsLinExpr, rel, rhsLinExpr)

compileReducedLinearExpr ::
  forall m.
  (MonadSMT m) =>
  MixedVariableCtx ->
  WHNFValue Builtin ->
  m (SparseLinearExpr MixedVariable)
compileReducedLinearExpr variables expr = do
  lnfExpr <- convertToLNF expr
  go lnfExpr
  where
    go :: WHNFValue Builtin -> m (SparseLinearExpr MixedVariable)
    go e = case e of
      VBoundVar v [] ->
        singletonVar v 1
      VBuiltinFunction (Neg NegRat) [RelevantExplicitArg _ (VBoundVar v [])] ->
        singletonVar v (-1)
      VRatLiteral l -> do
        return $ Sparse mempty mempty (Vector.singleton (fromRational l))
      VBuiltinFunction (Add AddRat) [e1, e2] -> do
        l1 <- go (argExpr e1)
        l2 <- go (argExpr e2)
        return $ addExprs 1 l1 1 l2
      VBuiltinFunction (Mul MulRat) [e1, e2] ->
        case (argExpr e1, argExpr e2) of
          (VRatLiteral l, VBoundVar v []) -> singletonVar v (fromRational l)
          (VBoundVar v [], VRatLiteral l) -> singletonVar v (fromRational l)
          _ -> throwError catchableUnsupportedNonLinearConstraint
      VBuiltinFunction (Div DivRat) [e1, e2] ->
        case (argExpr e1, argExpr e2) of
          (VBoundVar v [], VRatLiteral l) ->
            singletonVar v (fromRational (1 / l))
          _ -> throwError catchableUnsupportedNonLinearConstraint
      ex -> unexpectedExprError currentPass $ prettyVerbose ex

    singletonVar :: Lv -> Coefficient -> m (SparseLinearExpr MixedVariable)
    singletonVar level coef = do
      var <- lookupLvInBoundCtx currentPass level variables
      return (Sparse [] (Map.singleton var coef) (Vector.singleton 0))

-- | Converts the provided expression to linear normal form,
-- i.e. consisting of only additions and multiplications by constants.
-- e.g. x + 3 * (x + y) ====> x + 3 * x + 3 * y
convertToLNF :: (MonadCompile m) => WHNFValue Builtin -> m (WHNFValue Builtin)
convertToLNF = lnf
  where
    lnf :: (MonadCompile m) => WHNFValue Builtin -> m (WHNFValue Builtin)
    lnf expr = case expr of
      VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
      VPi {} -> unexpectedTypeInExprError currentPass "Pi"
      VMeta {} -> resolutionError currentPass "Meta"
      VLam {} -> caseError currentPass "Lam" ["QuantifierExpr"]
      VFreeVar i _ -> normalisationError currentPass ("FreeVar" <+> pretty i)
      VBuiltinFunction fun args -> do
        args' <- traverse (lnf . argExpr) args
        case (fun, args') of
          (Neg dom, [e1]) -> return $ argExpr $ lowerNeg dom e1
          (Sub dom, [e1, e2]) -> return $ normSub dom e1 e2
          (Mul dom, [e1, e2]) -> return $ normMul dom e1 e2
          (Div dom, [e1, e2]) -> return $ normDiv dom e1 e2
          _ -> return $ VBuiltinFunction fun (Arg mempty Explicit Relevant <$> args')
      VBuiltin {} -> return expr
      VBoundVar {} -> return expr

    normMul :: MulDomain -> WHNFValue Builtin -> WHNFValue Builtin -> WHNFValue Builtin
    normMul dom e1 e2 = case (e1, e2) of
      (_, VBuiltinFunction (Add addDom) [v1, v2]) -> do
        let r1 = normMul dom e1 (argExpr v1)
        let r2 = normMul dom e1 (argExpr v2)
        VBuiltinFunction (Add addDom) (Arg mempty Explicit Relevant <$> [r1, r2])
      (VBuiltinFunction (Add addDom) [v1, v2], _) -> do
        let r1 = normMul dom (argExpr v1) e2
        let r2 = normMul dom (argExpr v2) e2
        VBuiltinFunction (Add addDom) (Arg mempty Explicit Relevant <$> [r1, r2])
      (x1, x2) -> case evalMul dom [x1, x2] of
        Nothing -> VBuiltinFunction (Mul dom) (Arg mempty Explicit Relevant <$> [e1, e2])
        Just r -> r

    normSub :: SubDomain -> WHNFValue Builtin -> WHNFValue Builtin -> WHNFValue Builtin
    normSub dom e1 e2 = do
      let negDom = subToNegDomain dom
      let addDom = subToAddDomain dom
      VBuiltinFunction (Add addDom) [Arg mempty Explicit Relevant e1, lowerNeg negDom e2]

    normDiv :: DivDomain -> WHNFValue Builtin -> WHNFValue Builtin -> WHNFValue Builtin
    normDiv dom e1 e2 = case (e1, e2) of
      (_, VRatLiteral l) -> do
        let mulDom = divToMulDomain dom
        normMul mulDom e1 (VRatLiteral (1 / l))
      _ -> do
        VBuiltinFunction (Div dom) (Arg mempty Explicit Relevant <$> [e1, e2])

    lowerNeg :: NegDomain -> WHNFValue Builtin -> WHNFArg Builtin
    lowerNeg dom expr = Arg mempty Explicit Relevant $ case expr of
      -- Base cases
      VBuiltinFunction (Neg _) [e] -> argExpr e
      VIntLiteral x -> VIntLiteral (-x)
      VRatLiteral x -> VRatLiteral (-x)
      VBoundVar _ [] -> do
        let mulDom = negToMulDomain dom
        let minus1 = Arg mempty Explicit Relevant $ case dom of
              NegInt -> VIntLiteral (-1)
              NegRat -> VRatLiteral (-1)
        VBuiltinFunction (Mul mulDom) [minus1, Arg mempty Explicit Relevant expr]

      -- Inductive cases
      VBuiltinFunction (Add addDom) [e1, e2] ->
        VBuiltinFunction (Add addDom) [lowerNeg dom (argExpr e1), lowerNeg dom (argExpr e2)]
      VBuiltinFunction (Mul mulDom) [e1, e2] ->
        VBuiltinFunction (Mul mulDom) [lowerNeg dom (argExpr e1), e2]
      -- Errors
      e -> developerError ("Unable to lower 'neg' through" <+> pretty (show e))

currentPass :: Doc a
currentPass = "linear satisfaction problem"

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedNonLinearConstraint :: CompileError
catchableUnsupportedNonLinearConstraint =
  UnsupportedNonLinearConstraint x x x
  where
    x = developerError "Evaluating temporary quantifier error"
