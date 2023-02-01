{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Vehicle.Compile.Queries.LinearSatisfactionProblem
  ( generateCLSTProblem,
  )
where

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Bifunctor (Bifunctor (..))
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
  ( lookup,
    singleton,
    unionWith,
  )
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Vehicle.Backend.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Queries.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Queries.GaussianElimination
  ( gaussianElimination,
    solutionEquality,
  )
import Vehicle.Compile.Queries.LNF (convertToLNF)
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Compile.Queries.NetworkElimination (InputEqualities)
import Vehicle.Compile.Queries.Variable
import Vehicle.Compile.Queries.VariableReconstruction
import Vehicle.Compile.Resource
import Vehicle.Expr.Boolean (ConjunctAll, MaybeTrivial (..), unConjunctAll)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised (NormExpr (..), pattern VBuiltinFunction)
import Vehicle.Verify.Specification

-- | Generates a constraint satisfication problem in the magic network variables only.
generateCLSTProblem ::
  MonadCompile m =>
  LCSState ->
  InputEqualities ->
  ConjunctAll NormExpr ->
  m (MaybeTrivial (CLSTProblem NetworkVariable, MetaNetwork, UserVarReconstructionInfo))
generateCLSTProblem state inputEqualities conjuncts = flip runReaderT state $ do
  (_, _, metaNetwork, userVariables, _) <- ask
  variables <- getVariables

  inputEqualityAssertions <- forM inputEqualities $ \(i, expr) -> do
    -- Create linear expression equating the magic variable `x_i`
    -- with the expression `e` in the relevant point = xs_i`
    exprSize <- getExprSize
    let lhs = linearExprFromMap exprSize (Map.singleton (unLevel i) 1)
    rhs <- compileLinearExpr expr
    return $ constructAssertion (lhs, Equal, rhs)

  userAssertions <- NonEmpty.toList . unConjunctAll <$> traverse compileAssertions conjuncts

  {-
  case result of
    Trivial p -> return $ Trivial p
    NonTrivial userAssertions -> do
      -}
  let assertions = inputEqualityAssertions <> userAssertions
  let clst = CLSTProblem variables assertions

  networkVarQuery <-
    solveForUserVariables (length userVariables) clst

  let finalQuery = flip fmap networkVarQuery $
        \(solvedCLST, varReconst) -> (solvedCLST, metaNetwork, varReconst)

  return finalQuery

solveForUserVariables ::
  MonadCompile m =>
  Int ->
  CLSTProblem Variable ->
  m (MaybeTrivial (CLSTProblem NetworkVariable, UserVarReconstructionInfo))
solveForUserVariables numberOfUserVars (CLSTProblem variables assertions) =
  logCompilerPass MinDetail "elimination of user variables" $ do
    let allUserVars = Set.fromList [0 .. numberOfUserVars - 1]

    -- First remove those assertions that don't have any user variables in them.
    let (withUserVars, withoutUserVars) =
          partition (hasUserVariables numberOfUserVars) assertions

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

    -- Calculate the final set of (user-variable free) assertions
    let resultingAssertions = withoutUserVars <> unusedEqualities <> fmElimOutputInequalities

    -- Remove all user variables
    let networkVariables = mapMaybe getNetworkVariable variables
    let networkAssertions = fmap (removeUserVariables numberOfUserVars) resultingAssertions

    -- Check for trivial assertions
    let maybeAssertions = filterTrivialAssertions networkAssertions

    return $ case maybeAssertions of
      Nothing -> Trivial False
      Just [] -> Trivial True
      Just finalAssertions -> do
        let clstProblem = CLSTProblem networkVariables finalAssertions
        NonTrivial (clstProblem, varSolutions)

-- Return the problem

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

getNetworkDetailsFromCtx :: MonadCompile m => NetworkContext -> Name -> m NetworkType
getNetworkDetailsFromCtx networkCtx name = do
  case Map.lookup name networkCtx of
    Just (_file, typ) -> return typ
    Nothing ->
      compilerDeveloperError $
        "Either" <+> squotes (pretty name) <+> "is not a network or it is not in scope"

getBoundContext :: MonadSMT m => m BoundDBCtx
getBoundContext = do
  (_, _, _, userVariables, networkVariables) <- ask
  let userNames = reverse $ fmap (layoutAsText . pretty) userVariables
  let networkNames = reverse $ fmap (layoutAsText . pretty) networkVariables
  return $ fmap Just (userNames <> networkNames)

getNumberOfUserVariables :: MonadSMT m => m Int
getNumberOfUserVariables = do
  (_, _, _, userVariables, _) <- ask
  return $ length userVariables

getMetaNetworkType :: MonadSMT m => m [NetworkType]
getMetaNetworkType = do
  (networkCtx, _, metaNetwork, _, _) <- ask
  traverse (getNetworkDetailsFromCtx networkCtx . fst) metaNetwork

getNumberOfMagicVariables :: MonadSMT m => m Int
getNumberOfMagicVariables = sum . fmap networkSize <$> getMetaNetworkType

getTotalNumberOfVariables :: MonadSMT m => m Int
getTotalNumberOfVariables = do
  numberOfUserVariables <- getNumberOfUserVariables
  numberOfMagicVariables <- getNumberOfMagicVariables
  return $ numberOfUserVariables + numberOfMagicVariables

getExprSize :: MonadSMT m => m Int
getExprSize =
  -- Add one more for the constant term.
  (1 +) <$> getTotalNumberOfVariables

getVariables :: MonadSMT m => m [Variable]
getVariables = do
  (_, _, _, userVariables, networkVariables) <- ask
  return $ fmap UserVar userVariables <> fmap NetworkVar networkVariables

getExprConstantIndex :: MonadSMT m => m DBLevel
getExprConstantIndex =
  -- The constant in the linear expression is stored in the last index.
  DBLevel <$> getTotalNumberOfVariables

--------------------------------------------------------------------------------
-- Compilation of assertions

compileAssertions :: MonadSMT m => NormExpr -> m Assertion
compileAssertions = go
  where
    go :: MonadSMT m => NormExpr -> m Assertion
    go expr = case expr of
      VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
      VPi {} -> unexpectedTypeInExprError currentPass "Pi"
      VMeta {} -> resolutionError currentPass "Meta"
      VLam {} -> normalisationError currentPass "Lam"
      VLVec {} -> normalisationError currentPass "LVec"
      VBoundVar {} -> caseError currentPass "Var" ["OrderOp", "Eq"]
      VFreeVar {} -> normalisationError currentPass "VFreeVar"
      VLiteral l -> case l of
        LBool _ -> normalisationError currentPass "LBool"
        _ -> caseError currentPass "Literal" ["AndExpr"]
      VBuiltinFunction (Order OrderRat ord) [ExplicitArg _ e1, ExplicitArg _ e2] -> do
        let (rel, lhs, rhs) = case ord of
              Lt -> (LessThan, e1, e2)
              Le -> (LessThanOrEqualTo, e1, e2)
              Gt -> (LessThan, e2, e1)
              Ge -> (LessThanOrEqualTo, e2, e1)
        assertion <- compileAssertion rel lhs rhs
        return assertion
      VBuiltinFunction (Equals EqRat eq) [ExplicitArg _ e1, ExplicitArg _ e2] -> case eq of
        Neq -> do
          (_, ident, _, _, _) <- ask
          throwError $ UnsupportedInequality MarabouBackend ident
        Eq -> do
          assertion <- compileAssertion Equal e1 e2
          return assertion
      _ -> unexpectedExprError currentPass (prettyVerbose expr)

compileAssertion ::
  MonadSMT m =>
  Relation ->
  NormExpr ->
  NormExpr ->
  m Assertion
compileAssertion rel lhs rhs = do
  lhsLinExpr <- compileLinearExpr lhs
  rhsLinExpr <- compileLinearExpr rhs
  return $ constructAssertion (lhsLinExpr, rel, rhsLinExpr)

compileLinearExpr :: MonadSMT m => NormExpr -> m LinearExpr
compileLinearExpr expr = do
  lnfExpr <- convertToLNF expr
  linearExpr <- go lnfExpr
  exprSize <- getExprSize
  return $ linearExprFromMap exprSize linearExpr
  where
    singletonVar :: DBLevel -> Coefficient -> Map Int Coefficient
    singletonVar v = Map.singleton (unLevel v)

    go :: MonadSMT m => NormExpr -> m (Map Int Coefficient)
    go e = case e of
      VBoundVar v [] ->
        return $ singletonVar v 1
      VBuiltinFunction (Neg NegRat) [ExplicitArg _ (VBoundVar v [])] ->
        return $ singletonVar v (-1)
      VLiteral (LRat l) -> do
        constLevel <- getExprConstantIndex
        return $ singletonVar constLevel (fromRational l)
      VBuiltinFunction (Add AddRat) [ExplicitArg _ e1, ExplicitArg _ e2] -> do
        Map.unionWith (+) <$> go e1 <*> go e2
      VBuiltinFunction (Mul MulRat) [ExplicitArg _ e1, ExplicitArg _ e2] ->
        case (e1, e2) of
          (VLiteral (LRat l), VBoundVar v []) -> return $ singletonVar v (fromRational l)
          (VBoundVar v [], VLiteral (LRat l)) -> return $ singletonVar v (fromRational l)
          _ -> do
            (_, _ident, _, _, _) <- ask
            _ctx <- getBoundContext
            compilerDeveloperError $
              "Unexpected non-linear constraint that should have been caught by the"
                <+> "linearity analysis during type-checking."
      ex -> unexpectedExprError currentPass $ prettyVerbose ex

filterTrivialAssertions :: [Assertion] -> Maybe [Assertion]
filterTrivialAssertions = go
  where
    go :: [Assertion] -> Maybe [Assertion]
    go [] = Just []
    go (a : as) = case go as of
      Nothing -> Nothing
      Just as' -> case checkTriviality a of
        Nothing -> Just $ a : as'
        Just True -> Just as'
        Just False -> Nothing

currentPass :: Doc a
currentPass = "linear satisfaction problem compilation"
