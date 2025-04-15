module Vehicle.Backend.Queries.UserVariableElimination.EliminateExists
  ( solveExists,
  )
where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Vehicle.Backend.Queries.ConstraintSearch
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Name (getNameContext)
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (LinearExpr, VariableLike (..), rearrangeExprToSolveFor, referencesVariable)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor, tensorToList)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Specification

--------------------------------------------------------------------------------
-- Main function

type MonadSolveExists m = MonadQueryStructure m

-- | Eliminates the provided user variable from the assertion tree. This may
-- require partially converting the expression to disjunctive normal form so it
-- returns a set of disjuncted updated assertion trees and variable solutions.
solveExists ::
  (MonadSolveExists m) =>
  MaybeTrivial (Partitions UserOrNetworkTensorVariable) ->
  UserTensorVariable ->
  m (MaybeTrivial (Partitions UserOrNetworkTensorVariable))
solveExists maybePartitions userVar = case maybePartitions of
  Trivial b -> return $ Trivial b
  NonTrivial partitions -> do
    newPartitions <- traverse (solvePartition userVar) (partitionsToDisjuncts partitions)
    return $ foldr1 (orTrivial orPartitions) (disjunctDisjuncts newPartitions)

--------------------------------------------------------------------------------
-- Tensor equalities

solvePartition ::
  UserTensorVariable ->
  Partition UserOrNetworkTensorVariable ->
  m (DisjunctAll (MaybeTrivial (Partitions UserOrNetworkTensorVariable)))
solvePartition userVar partition@(_, tree) = do
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let userVarName = lookupLvInBoundCtx (toLv userVar) ctx
    let treeDoc = prettyFriendly (WithContext tree ctx)
    return $
      "Solving for" <+> quotePretty userVarName <+> "in:" <> line <> indent 2 treeDoc <> line

  constraints <- findVariableConstraints checkAssertion userVar tree
  traverse (solveVariableViaConstraints partition userVar) constraints

checkAssertion :: ConstraintSearchCriteria
checkAssertion var assertion@NormalisedRelation {..}
  | linearExpr `referencesVariable` toTensorVar var = case splitRelation assertion of
      Right equality -> SingleEquality equality (Trivial True)
      Left inequality -> Inequalities [inequality] (Trivial True)
  | otherwise = Inequalities [] $ NonTrivial $ Query assertion

solveVariableViaConstraints ::
  (MonadSolveExists m) =>
  Partition UserOrNetworkTensorVariable ->
  UserTensorVariable ->
  ConstrainedAssertionTree ->
  m (MaybeTrivial (Partitions UserOrNetworkTensorVariable))
solveVariableViaConstraints (compilationTrace, originalTree) userVar = \case
  SingleEquality equality remainingTree ->
    solveVariableViaEquality compilationTrace userVar equality remainingTree
  Inequalities ineqs remainingTree -> do
    globalCtx <- get
    let maybeTensorVarInfo = Map.lookup userVar (userTensorVariableInfo globalCtx)
    case maybeTensorVarInfo of
      Just info -> solveVariableByReducing userVar info originalTree
      Nothing -> solveVariableViaInequalities compilationTrace userVar ineqs remainingTree

solveVariableViaEquality ::
  [UserVariableCompilationStep] ->
  UserTensorVariable ->
  Equality UserOrNetworkTensorVariable RatTensor ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable) ->
  m (MaybeTrivial (Partitions UserOrNetworkTensorVariable))
solveVariableViaEquality compilationTrace userVar equality remainingTree = do
  let (_, rearrangedExpr) = rearrangeExprToSolveFor (toTensorVar userVar) (linearExpr equality)
  let elementEqs = case maybeTensorVarInfo of
        Nothing -> []
        Just info -> zip (tensorToList (elementVariables info)) $ reduceTensorExpr globalCtx rearrangedExpr
  let solutionMap = Map.fromList $ (userVar, rearrangedExpr) : elementEqs
  let updatedTree = solutionMap `substituteThrough` remainingTree
  let newCompilationTrace = SolveEquality userVar rearrangedExpr : compilationTrace
  -- Update tree
  logEqualitySolved userVar rearrangedExpr remainingTree updatedTree
  return $ mkSingletonPartitions (newCompilationTrace, updatedTree)

solveVariableByReducing ::
  UserTensorVariable ->
  TensorVariableInfo ->
  AssertionTree UserOrNetworkTensorVariable ->
  m (MaybeTrivial (Partitions UserOrNetworkTensorVariable))
solveVariableByReducing userVar userVarInfo originalTree = do
  let userRationalVars = elementVariables userVarInfo
  logDebug MaxDetail "No equality constraints on original tensor variable found"
  let step = ReconstructUserTensor userVar userRationalVars
  let initial = mkSinglePartition (step : solutions, NonTrivial originalTree)
  foldlM solveExists initial (tensorToList userRationalVars)

solveVariableViaInequalities ::
  [UserVariableCompilationStep] ->
  UserTensorVariable ->
  [Inequality UserOrNetworkTensorVariable RatTensor] ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable) ->
  m (MaybeTrivial (Partitions UserOrNetworkTensorVariable))
solveVariableViaInequalities compilationTrace userVar ineqs remainingTree = do
  (bounds, newInequalities) <- fourierMotzkinElimination (toTensorVar userVar) ineqs
  let addIneq ineq = andTrivial andBoolExpr (NonTrivial $ Query $ inequalityToNormRelation ineq)
  let updatedTree = foldr addIneq remainingTree newInequalities
  let traceStep = SolveInequalities userVar bounds
  let newCompilationTrace = traceStep : compilationTrace
  logInequalitiesSolved userVar traceStep remainingTree
  return $ mkSingletonPartitions (newCompilationTrace, updatedTree)

substituteThrough ::
  Map UserTensorVariable (LinearExpr NetworkTensorVariable RatTensor) ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable) ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable)
substituteThrough f = filterTrivialAtoms . fmap (fmap (eliminateVarsInAssertion f))

--------------------------------------------------------------------------------
-- Logging

logEqualitySolved ::
  (MonadSolveExists m) =>
  UserTensorVariable ->
  LinearExpr NetworkTensorVariable RatTensor ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable) ->
  MaybeTrivial (AssertionTree NetworkTensorVariable) ->
  m ()
logEqualitySolved var rearrangedEq remainingTree updatedTree =
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let varName = lookupLvInBoundCtx (toLv var) ctx
    return $
      "Solving"
        <> line
        <> indent 2 (pretty varName <+> "=" <+> prettyFriendly (WithContext rearrangedEq ctx))
        <> line
        <> "in context:"
        <> line
        <> indent 2 (prettyFriendly (WithContext remainingTree ctx))
        <> line
        <> "to get:"
        <> line
        <> indent 2 (prettyFriendly (WithContext updatedTree ctx))

logInequalitiesSolved ::
  (MonadSolveExists m) =>
  UserTensorVariable ->
  UserVariableCompilationStep ->
  MaybeTrivial (AssertionTree UserOrNetworkTensorVariable) ->
  m ()
logInequalitiesSolved var step remainingTree = do
  PropertyMetaData {..} <- ask
  ctx <- getNameContext
  let varName = fromMaybe "<unknown-var>" $ lookupLvInBoundCtx (toLv var) ctx

  logWarning $ UnderSpecifiedProblemSpaceVar propertyAddress varName
  logDebugM MaxDetail $ do
    return $
      "Solving"
        <> line
        <> indent 2 (pretty step)
        <> line
        <> "in context:"
        <> line
        <> indent 2 (prettyFriendly (WithContext remainingTree ctx))
