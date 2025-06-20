module Vehicle.Backend.Queries.UserVariableElimination.EliminateExists
  ( solveExists,
  )
where

import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..), gets)
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
import Vehicle.Backend.Queries.ConstraintSearch
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Context.Name (getNameContext)
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (HasVariables (containsVariable), VariableLike (..))
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor as Tensor (RatTensor, toList)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Syntax.Tensor (Tensor)
import Vehicle.Verify.Specification

--------------------------------------------------------------------------------
-- Main function

type MonadSolveExists m = MonadQueryStructure m

-- | Eliminates the provided user variable from the assertion tree. This may
-- require partially converting the expression to disjunctive normal form so it
-- returns a set of disjuncted updated assertion trees and variable solutions.
solveExists ::
  (MonadSolveExists m) =>
  MaybeTrivial (Partitions TensorVariable) ->
  UserVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
solveExists maybePartitions userVar = case maybePartitions of
  Trivial b -> return $ Trivial b
  NonTrivial partitions -> do
    let disjunctedPartitions = partitionsToDisjuncts partitions
    newPartitions <- traverse (solvePartition userVar) disjunctedPartitions
    return $ foldr1 (orTrivial orPartitions) (disjunctDisjuncts newPartitions)

--------------------------------------------------------------------------------
-- Tensor equalities

solvePartition ::
  (MonadSolveExists m) =>
  UserVariable ->
  Partition TensorVariable ->
  m (DisjunctAll (MaybeTrivial (Partitions TensorVariable)))
solvePartition userVar partition@(_, tree) = do
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let userVarName = lookupLvInBoundCtx (toLv userVar) ctx
    let treeDoc = prettyFriendly (WithContext tree ctx)
    return $
      "Solving for" <+> quotePretty userVarName <+> "in:" <> lineIndent treeDoc <> line

  constraints <- findConstraints (constraintReferencesVariable userVar) tree
  traverse (solveVariableViaConstraints partition userVar) constraints

constraintReferencesVariable :: UserVariable -> ConstraintSearchCriteria
constraintReferencesVariable var assertion@NormalisedRelation {..}
  | linearExpr `containsVariable` toTensorVar var = case splitRelation assertion of
      Right equality -> (SingleEquality equality, Trivial True)
      Left inequality -> (Inequalities [inequality], Trivial True)
  | otherwise = (Inequalities [], NonTrivial $ Query assertion)

solveVariableViaConstraints ::
  (MonadSolveExists m) =>
  Partition TensorVariable ->
  UserVariable ->
  ConstrainedAssertionTree ->
  m (MaybeTrivial (Partitions TensorVariable))
solveVariableViaConstraints (compilationTrace, originalTree) userVar (varConstraints, remainingTree) = do
  case varConstraints of
    SingleEquality equality ->
      solveVariableViaEquality compilationTrace userVar equality remainingTree
    Inequalities ineqs -> do
      maybeChildVariables <- gets (`lookupChildVariables` userVar)
      case maybeChildVariables of
        Just childVariables -> solveVariableByReducing compilationTrace userVar (coerce childVariables) originalTree
        Nothing -> solveVariableViaInequalities compilationTrace userVar ineqs remainingTree

solveVariableViaEquality ::
  (MonadSolveExists m) =>
  [CompilationStep] ->
  UserVariable ->
  Equality TensorVariable RatTensor ->
  MaybeTrivial (AssertionTree TensorVariable) ->
  m (MaybeTrivial (Partitions TensorVariable))
solveVariableViaEquality compilationTrace userVar equality remainingTree = do
  globalCtx <- get
  (solutionMap, compilationStep) <- createSubstitutionForVariable globalCtx (coerce userVar) equality
  let updatedTree = solutionMap `substituteThrough` remainingTree
  let newCompilationTrace = compilationStep : compilationTrace
  -- Update tree
  logEqualitySolved userVar compilationStep updatedTree
  return $ mkSingletonPartitions (newCompilationTrace, updatedTree)

solveVariableByReducing ::
  (MonadSolveExists m) =>
  [CompilationStep] ->
  UserVariable ->
  Tensor UserVariable ->
  AssertionTree TensorVariable ->
  m (MaybeTrivial (Partitions TensorVariable))
solveVariableByReducing compilationTrace userVar userElementVars originalTree = do
  ctx <- getNameContext
  let userVarName = lookupLvInBoundCtx (toLv userVar) ctx
  logDebug MaxDetail $ "No equality constraints on original tensor variable" <+> quotePretty userVarName <+> "found"
  logCompilerSection MaxDetail ("Solving for elements of" <+> quotePretty userVarName) $ do
    let step = ReconstructTensorVariable (coerce userVar) (coerce userElementVars)
    let initial = mkSingletonPartitions (step : compilationTrace, NonTrivial originalTree)
    foldlM solveExists initial (Tensor.toList userElementVars)

solveVariableViaInequalities ::
  (MonadSolveExists m) =>
  [CompilationStep] ->
  UserVariable ->
  [Inequality TensorVariable RatTensor] ->
  MaybeTrivial (AssertionTree TensorVariable) ->
  m (MaybeTrivial (Partitions TensorVariable))
solveVariableViaInequalities compilationTrace userVar ineqs remainingTree = do
  (bounds, newInequalities) <- fourierMotzkinElimination (toTensorVar userVar) ineqs
  let addIneq ineq = andTrivial andBoolExpr (NonTrivial $ Query $ inequalityToNormRelation ineq)
  let updatedTree = foldr addIneq remainingTree newInequalities
  let traceStep = SolveInequalities userVar bounds
  let newCompilationTrace = traceStep : compilationTrace
  logInequalitiesSolved userVar traceStep remainingTree
  return $ mkSingletonPartitions (newCompilationTrace, updatedTree)

substituteThrough ::
  LinearSubstitution TensorVariable ->
  MaybeTrivial (AssertionTree TensorVariable) ->
  MaybeTrivial (AssertionTree TensorVariable)
substituteThrough f =
  filterTrivialAtoms . fmap (fmap (eliminateVarsInAssertion f))

--------------------------------------------------------------------------------
-- Logging

logEqualitySolved ::
  (MonadSolveExists m) =>
  UserVariable ->
  CompilationStep ->
  MaybeTrivial (AssertionTree TensorVariable) ->
  m ()
logEqualitySolved var compilationStep updatedTree =
  logDebugM MaxDetail $ do
    ctx <- getNameContext
    let varName = lookupLvInBoundCtx (toLv var) ctx
    return $
      "Eliminating" <+> quotePretty varName <+> "using"
        <> line
        <> indent 2 (prettyFriendly (WithContext compilationStep ctx))
        <> line
        <> "to get:"
        <> line
        <> indent 2 (prettyFriendly (WithContext updatedTree ctx))

logInequalitiesSolved ::
  (MonadSolveExists m) =>
  UserVariable ->
  CompilationStep ->
  MaybeTrivial (AssertionTree TensorVariable) ->
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
        <> indent 2 (prettyFriendly (WithContext step ctx))
        <> line
        <> "in context:"
        <> line
        <> indent 2 (prettyFriendly (WithContext remainingTree ctx))
