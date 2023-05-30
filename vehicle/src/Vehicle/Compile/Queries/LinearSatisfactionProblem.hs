{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Vehicle.Compile.Queries.LinearSatisfactionProblem
  ( UserVariableEliminationCache,
    generateCLSTProblem,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Control.Monad.State (MonadState (get), modify)
import Data.Bifunctor (Bifunctor (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (partition)
import Data.Map.Strict qualified as Map
  ( lookup,
  )
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Queries.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Queries.GaussianElimination
  ( gaussianElimination,
  )
import Vehicle.Compile.Queries.LNF (convertToLNF)
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.NetworkElimination (InputEqualities)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Boolean (ConjunctAll, MaybeTrivial (..), conjunctsToList, eliminateTrivialConjunctions)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Vehicle.Verify.Core

type MonadCLST m =
  ( MonadCompile m,
    MonadState UserVariableEliminationCache m
  )

-- | Generates a constraint satisfication problem in the magic network variables only.
generateCLSTProblem ::
  (MonadCLST m) =>
  LCSState ->
  InputEqualities ->
  ConjunctAll StandardNormExpr ->
  m (MaybeTrivial (CLSTProblem NetworkVariable, QueryNormalisedVariableInfo))
generateCLSTProblem state inputEqualities conjuncts = flip runReaderT state $ do
  (_, _, _, userVariables, _) <- ask
  variables <- getVariables

  inputEqualityAssertions <- forM inputEqualities $ \(i, expr) -> do
    -- Create linear expression equating the magic variable `x_i`
    -- with the expression `e` in the relevant point = xs_i`
    exprSize <- getExprSize
    let lhs = fromSparse $ Sparse exprSize (HashMap.singleton (unLv i) 1) 0
    rhs <- compileLinearExpr expr
    return $ constructAssertion (lhs, Equal, rhs)

  maybeTrivialUserAssertions <- eliminateTrivialConjunctions <$> traverse compileAssertions conjuncts
  case maybeTrivialUserAssertions of
    Trivial b -> return $ Trivial b
    NonTrivial userAssertions -> do
      let assertions = inputEqualityAssertions <> conjunctsToList userAssertions
      let clst = CLSTProblem variables assertions

      networkVarQuery <-
        solveForUserVariables (length userVariables) clst

      let finalQuery = flip fmap networkVarQuery $
            \(solvedCLST, varReconst) -> (solvedCLST, varReconst)

      return finalQuery

solveForUserVariables ::
  (MonadCLST m) =>
  Int ->
  CLSTProblem Variable ->
  m (MaybeTrivial (CLSTProblem NetworkVariable, QueryNormalisedVariableInfo))
solveForUserVariables numberOfUserVars (CLSTProblem variables assertions) =
  logCompilerPass MinDetail "elimination of user variables" $ do
    let allUserVars = Set.fromList [0 .. numberOfUserVars - 1]

    -- First remove those assertions that don't have any user variables in them.
    let (withUserVars, withoutUserVars) =
          partition (hasUserVariables numberOfUserVars) assertions
    let originallyWithoutUserVars = fmap (removeUserVariables numberOfUserVars) withoutUserVars

    cache <- get
    (newlyWithoutUserVars, varSolutions) <- case HashMap.lookup withUserVars cache of
      Just result -> do
        logDebug MinDetail "Cache hit for query user variable solutions"
        return result
      Nothing -> do
        -- Then split out the equalities from the inequalities.
        let (equalitiesWithUserVars, inequalitiesWithUserVars) =
              partition isEquality withUserVars

        -- Try to solve for user variables using Gaussian elimination.
        (gaussianSolutions, unusedEqualityExprs) <-
          gaussianElimination variables (map assertionExpr equalitiesWithUserVars) numberOfUserVars
        let unusedEqualities = fmap (Assertion Equal) unusedEqualityExprs

        -- Eliminate the solved user variables in the inequalities
        let gaussianSolutionEqualities = fmap (second solutionEquality) gaussianSolutions
        let reducedInequalities =
              flip fmap inequalitiesWithUserVars $ \assertion ->
                foldl (uncurry . substitute) assertion gaussianSolutionEqualities

        -- Calculate the set of unsolved user variables
        let varsSolvedByGaussianElim = Set.fromList (fmap fst gaussianSolutions)
        let varsUnsolvedByGaussianElim = Set.difference allUserVars varsSolvedByGaussianElim

        -- Eliminate the remaining unsolved user vars using Fourier-Motzkin elimination
        (fourierMotzkinSolutions, fmElimOutputInequalities) <-
          fourierMotzkinElimination variables varsUnsolvedByGaussianElim reducedInequalities

        -- Calculate the way to reconstruct the user variables
        let varSolutions =
              fmap (second FourierMotzkinSolution) fourierMotzkinSolutions
                <> fmap (second GaussianSolution) gaussianSolutions

        let finalAssertions = unusedEqualities <> fmElimOutputInequalities
        let finalAssertionsWithoutUserVars = fmap (removeUserVariables numberOfUserVars) finalAssertions

        let result = (finalAssertionsWithoutUserVars, varSolutions)
        modify (HashMap.insert withUserVars result)
        return result

    -- Calculate the final set of (user-variable free) assertions
    let allAssertions = originallyWithoutUserVars <> newlyWithoutUserVars
    let networkVariables = mapMaybe getNetworkVariable variables

    -- Check for trivial assertions
    let maybeAssertions = filterTrivialAssertions allAssertions

    return $ case maybeAssertions of
      Nothing -> Trivial False
      Just [] -> Trivial True
      Just finalAssertions -> do
        let clstProblem = CLSTProblem networkVariables finalAssertions
        NonTrivial (clstProblem, varSolutions)

type UserVariableEliminationCache =
  HashMap
    [Assertion SolvingLinearExpr]
    ([Assertion SolvingLinearExpr], [(LinearVar, VariableSolution)])

--------------------------------------------------------------------------------
-- Monad

type LCSState =
  ( NetworkContext,
    DeclProvenance,
    MetaNetwork,
    [UserVariable],
    [NetworkVariable]
  )

type MonadSMT m =
  ( MonadCompile m,
    MonadReader LCSState m
  )

getNetworkDetailsFromCtx :: (MonadCompile m) => NetworkContext -> Name -> m NetworkType
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just (_file, typ) -> return typ
    Nothing ->
      compilerDeveloperError $
        "Either" <+> squotes (pretty name) <+> "is not a network or it is not in scope"

getNumberOfUserVariables :: (MonadSMT m) => m Int
getNumberOfUserVariables = do
  (_, _, _, userVariables, _) <- ask
  return $ length userVariables

getMetaNetworkType :: (MonadSMT m) => m [NetworkType]
getMetaNetworkType = do
  (networkCtx, _, metaNetwork, _, _) <- ask
  traverse (getNetworkDetailsFromCtx networkCtx . fst) metaNetwork

getNumberOfMagicVariables :: (MonadSMT m) => m Int
getNumberOfMagicVariables = sum . fmap networkSize <$> getMetaNetworkType

getTotalNumberOfVariables :: (MonadSMT m) => m Int
getTotalNumberOfVariables = do
  numberOfUserVariables <- getNumberOfUserVariables
  numberOfMagicVariables <- getNumberOfMagicVariables
  return $ numberOfUserVariables + numberOfMagicVariables

getExprSize :: (MonadSMT m) => m Int
getExprSize =
  -- Add one more for the constant term.
  (1 +) <$> getTotalNumberOfVariables

getVariables :: (MonadSMT m) => m [Variable]
getVariables = do
  (_, _, _, userVariables, networkVariables) <- ask
  return $ fmap UserVar userVariables <> fmap NetworkVar networkVariables

--------------------------------------------------------------------------------
-- Compilation of assertions

compileAssertions :: (MonadSMT m) => StandardNormExpr -> m (MaybeTrivial (Assertion SolvingLinearExpr))
compileAssertions = go
  where
    go :: (MonadSMT m) => StandardNormExpr -> m (MaybeTrivial (Assertion SolvingLinearExpr))
    go expr = case expr of
      VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
      VPi {} -> unexpectedTypeInExprError currentPass "Pi"
      VMeta {} -> resolutionError currentPass "Meta"
      VLam {} -> normalisationError currentPass "Lam"
      VBoundVar {} -> caseError currentPass "Var" ["OrderOp", "Eq"]
      VFreeVar {} -> normalisationError currentPass "VFreeVar"
      VBuiltinFunction (Order OrderRat ord) [e1, e2] -> do
        let (rel, lhs, rhs) = case ord of
              Lt -> (LessThan, e1, e2)
              Le -> (LessThanOrEqualTo, e1, e2)
              Gt -> (LessThan, e2, e1)
              Ge -> (LessThanOrEqualTo, e2, e1)
        assertion <- compileAssertion rel lhs rhs
        return $ NonTrivial assertion
      VBuiltinFunction (Equals EqRat eq) [e1, e2] -> case eq of
        Neq -> do
          (_, ident, _, _, _) <- ask
          throwError $ UnsupportedInequality MarabouQueryFormat ident
        Eq -> do
          assertion <- compileAssertion Equal e1 e2
          return $ NonTrivial assertion
      VBoolLiteral b -> return $ Trivial b
      _ -> unexpectedExprError currentPass (prettyVerbose expr)

compileAssertion ::
  (MonadSMT m) =>
  Relation ->
  StandardNormExpr ->
  StandardNormExpr ->
  m (Assertion SolvingLinearExpr)
compileAssertion rel lhs rhs = do
  lhsLinExpr <- compileLinearExpr lhs
  rhsLinExpr <- compileLinearExpr rhs
  return $ constructAssertion (lhsLinExpr, rel, rhsLinExpr)

compileLinearExpr :: (MonadSMT m) => StandardNormExpr -> m SolvingLinearExpr
compileLinearExpr expr = do
  lnfExpr <- convertToLNF expr
  (linearExpr, constant) <- go lnfExpr
  exprSize <- getExprSize
  return $ fromSparse $ Sparse exprSize linearExpr constant
  where
    singletonVar :: Lv -> Coefficient -> HashMap Int Coefficient
    singletonVar v = HashMap.singleton (unLv v)

    go :: (MonadSMT m) => StandardNormExpr -> m (HashMap Int Coefficient, Coefficient)
    go e = case e of
      VBoundVar v [] ->
        return (singletonVar v 1, 0)
      VBuiltinFunction (Neg NegRat) [VBoundVar v []] ->
        return (singletonVar v (-1), 0)
      VRatLiteral l -> do
        return (mempty, fromRational l)
      VBuiltinFunction (Add AddRat) [e1, e2] -> do
        (coeff1, const1) <- go e1
        (coeff2, const2) <- go e2
        return (HashMap.unionWith (+) coeff1 coeff2, const1 + const2)
      VBuiltinFunction (Mul MulRat) [e1, e2] ->
        case (e1, e2) of
          (VRatLiteral l, VBoundVar v []) -> return (singletonVar v (fromRational l), 0)
          (VBoundVar v [], VRatLiteral l) -> return (singletonVar v (fromRational l), 0)
          _ -> do
            logDebug MinDetail $ "Found non-linear expression: " <+> prettyVerbose e <> line
            throwError temporaryUnsupportedNonLinearConstraint
      VBuiltinFunction (Div DivRat) [e1, e2] ->
        case (e1, e2) of
          (VBoundVar v [], VRatLiteral l) -> return (singletonVar v (fromRational (1 / l)), 0)
          _ -> do
            logDebug MinDetail $ "Found non-linear expression: " <+> prettyVerbose e <> line
            throwError temporaryUnsupportedNonLinearConstraint
      ex -> unexpectedExprError currentPass $ prettyVerbose ex

currentPass :: Doc a
currentPass = "linear satisfaction problem"

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
temporaryUnsupportedNonLinearConstraint :: CompileError
temporaryUnsupportedNonLinearConstraint =
  UnsupportedNonLinearConstraint x x x x x
  where
    x = developerError "Evaluating temporary quantifier error"
