{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Vehicle.Backend.LossFunction.Domain
  ( extractSearchDomain,
    Domain (..),
  )
where

import Control.Monad (foldM, unless, void)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Vehicle.Backend.LossFunction.Core
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Normalise.NBE (normaliseInEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Assertion (Bound, Bounds, Inequality (..), UnderConstrainedVariableStatus, checkBoundsExist, mkInequality, pattern Bound)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr (LinearExpr, addExprs, constantExpr, isConstant, linearExprToExpr, scaleExpr, singletonVarExpr)
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (mapTensor)

type MonadDomain m =
  ( MonadCompile m,
    MonadNameContext m
  )

data Domain = Domain
  { lowerBound :: WHNFValue TensorBuiltin,
    upperBound :: WHNFValue TensorBuiltin
  }

extractSearchDomain ::
  (MonadDomain m) =>
  DeclProvenance ->
  WHNFBinder TensorBuiltin ->
  Lv ->
  WHNFValue TensorBuiltin ->
  m (Domain, WHNFValue TensorBuiltin)
extractSearchDomain _propertyProv _binder _lv value = do
  _variableInfo <- case typeOf binder of
    ITensorType _ dims -> do
      maybeReducedVars <- case getDimensions dims of
        Just tensorShape -> do
          logDebug MaxDetail $ "Found concrete tensor shape" <+> pretty tensorShape
          let userVar = OriginalUserVariable (getBinderName binder) tensorShape
          let (reducedUseVars, _vectorExpr) = reduceVariable userTensorVarDimensions (lv + 1) userVar
          return $ Just reducedUseVars
        Nothing -> do
          logDebug MaxDetail $ "Found non-concrete tensor shape" <+> prettyVerbose dims
          return Nothing

      return $
        VariableInfo
          { tensorVarLv = lv,
            tensorVarName = getBinderName binder,
            elementVars = maybeReducedVars
          }
    _ -> compilerDeveloperError "Unexpected quantifier type"

  -- Search for constraints
  (constraints, remainder) <- flip runReaderT variableInfo $ findConstraints value
  maybeDomain <- extractDomainFromConstraints variableInfo constraints
  case maybeDomain of
    Left missingCostraints -> throwError $ NoQuantifierDomainFound propertyProv (void binder) (Just missingCostraints)
    Right domain -> return (domain, remainder)

--------------------------------------------------------------------------------
-- Constraints

type TensorElementInequality = Inequality UserElementVariable (WHNFValue TensorBuiltin)

type TensorInequality = Inequality Name (WHNFValue TensorBuiltin)

type VariableConstraint = Either TensorElementInequality TensorInequality

type VariableConstraints = [VariableConstraint]

splitConstraints :: VariableConstraints -> ([TensorElementInequality], [TensorInequality])
splitConstraints = partitionEithers

pattern NoConstraints :: VariableConstraints
pattern NoConstraints = []

type ConstrainedValue = (VariableConstraints, WHNFValue TensorBuiltin)

unconstrained :: WHNFValue TensorBuiltin -> ConstrainedValue
unconstrained = (NoConstraints,)

updateConstrainedValue ::
  WHNFValue TensorBuiltin ->
  ConstrainedValue ->
  ConstrainedValue
updateConstrainedValue originalExpr = \case
  constr@(_ : _, _) -> constr
  ([], _) -> ([], originalExpr)

instance IsConstant (WHNFValue TensorBuiltin) where
  isZero = \case
    -- This is only semi-decidable, probably need to think harder about what
    -- to do here.
    IRatLiteral _ 0 -> True
    _ -> False
  scaleConstant c = IMul MulRat (IRatLiteral mempty c)
  addConstants c1 c2 e1 e2 =
    IAdd AddRat (scaleConstant c1 e1) (scaleConstant c2 e2)

--------------------------------------------------------------------------------
-- Constraints

-- | Information for the variable whose domain we are trying to find.
data VariableInfo = VariableInfo
  { tensorVarLv :: Lv,
    tensorVarName :: Name,
    elementVars :: Maybe [(Lv, UserElementVariable)]
  }

type MonadSearch m =
  ( MonadDomain m,
    MonadReader VariableInfo m
  )

--------------------------------------------------------------------------------
-- Domain

extractDomainFromConstraints ::
  (MonadCompile m) =>
  VariableInfo ->
  VariableConstraints ->
  m (Either [(UserElementVariable, UnderConstrainedVariableStatus)] Domain)
extractDomainFromConstraints VariableInfo {..} constraints = do
  let (tensorElementInequalities, tensorInequalities) = splitConstraints constraints

  (tensorBounds, remainingInequalities) <- fourierMotzkinElimination tensorVarName tensorInequalities

  unless (null remainingInequalities) $
    compilerDeveloperError "Found unused tensor inequalities when solving for bounds. Not currently implemented."

  case checkBoundsExist (tensorVarName, tensorBounds) of
    Right (lowerBounds, upperBounds) -> do
      unless (null tensorElementInequalities) $
        compilerDeveloperError "Found mixed tensor and element inequalities when solving for bounds. Not currently implemented."
      return $ Right $ Domain _ _
    Left _ -> do
      (remainingElementInequalities, results) <- foldM extractVarBounds (tensorElementInequalities, mempty) (fmap snd elementVars)

      unless (null remainingElementInequalities) $
        compilerDeveloperError "Found unused element inequalities when solving for bounds. Not currently implemented."

      let (unsolvedVars, solvedVars) = partitionEithers (fmap checkBoundsExist results)
      if not $ null unsolvedVars
        then return $ Left unsolvedVars
        else do
          let (lowerBounds, upperBounds) = Data.Functor.unzip solvedVars
          let varMap = Map.fromList (fmap swap tensorElementVars)
          let lowerBoundElements = fmap (convertBoundToExpr varMap IMax) lowerBounds
          let upperBoundElements = fmap (convertBoundToExpr varMap IMin) upperBounds
          let tensorShape = userTensorVarDimensions tensorVar
          let lowerBoundExpr = tensorLikeToExpr id tensorShape lowerBoundElements
          let upperBoundExpr = tensorLikeToExpr id tensorShape upperBoundElements
          return $ Right $ Domain lowerBoundExpr upperBoundExpr

extractVarBounds ::
  (MonadCompile m) =>
  ([TensorElementInequality], [(UserElementVariable, Bounds UserElementVariable (WHNFValue TensorBuiltin))]) ->
  UserElementVariable ->
  m ([TensorElementInequality], [(UserElementVariable, Bounds UserElementVariable (WHNFValue TensorBuiltin))])
extractVarBounds (currentConstraints, solutions) var = do
  (bounds, newInequalities) <- fourierMotzkinElimination var currentConstraints
  return (newInequalities, (var, bounds) : solutions)

convertBoundToExpr ::
  Map UserElementVariable Lv ->
  (WHNFValue TensorBuiltin -> WHNFValue TensorBuiltin -> WHNFValue TensorBuiltin) ->
  NonEmpty (Bound UserElementVariable (WHNFValue TensorBuiltin)) ->
  WHNFValue TensorBuiltin
convertBoundToExpr varMap op bounds = foldr1 _ (fmap convertBound bounds)
  where
    -- Ignore strictness for the moment.

    convertConstant _first c = c
    convertVariable _first (v, c) = case Map.lookup v varMap of
      Nothing -> developerError $ "Missing variable Lv for" <+> pretty v
      Just lv -> IMul MulRat (IRatLiteral mempty c) (VBoundVar lv [])
    convertBound (Bound _strictness value) = linearExprToExpr convertConstant convertVariable (IAdd AddRat) value

--------------------------------------------------------------------------------
-- Constraint search

findConstraints :: (MonadSearch m) => WHNFValue TensorBuiltin -> m ConstrainedValue
findConstraints expr = case toBoolTensorView expr of
  -------------------------
  -- Unuseful base cases --
  -------------------------
  VBoolTensor {} -> return $ unconstrained expr
  VConstBoolTensor {} -> return $ unconstrained expr
  VQuantifyRatTensor {} -> return $ unconstrained expr
  VEqualsRatTensor Neq _ _ _ -> return $ unconstrained expr
  -- These two cases need to be altered if we are to handle disjoint domains?
  VOrTensor {} -> return $ unconstrained expr
  VReduceOrTensor {} -> return $ unconstrained expr
  -- Maybe we can do something with these?
  VReduceAndTensor {} -> return $ unconstrained expr
  VStackBoolTensor {} -> return $ unconstrained expr
  ----------------
  -- Useful base cases --
  ----------------
  VOrderRatTensor op _ x y -> handleRatInequality op x y
  ---------------------
  -- Recursive cases --
  ---------------------
  VEqualsRatTensor Eq _ x y ->
    updateConstrainedValue expr <$> findConstraints (unfoldEquality x y)
  VAndTensor _ x y -> do
    (cx, x') <- findConstraints x
    (cy, y') <- findConstraints y
    return (cx <> cy, IAnd x' y')
  VNotTensor _ x -> handleNot x

handleNot ::
  forall m.
  (MonadSearch m) =>
  WHNFValue TensorBuiltin ->
  m ConstrainedValue
handleNot expr = do
  loweredExpr <- lowerBoolTensor expr
  case loweredExpr of
    INot {} -> return $ unconstrained expr
    newExpr -> updateConstrainedValue expr <$> findConstraints newExpr
  where
    lowerBoolTensor :: WHNFValue TensorBuiltin -> m (WHNFValue TensorBuiltin)
    lowerBoolTensor e =
      fromBoolTensorView <$> case toBoolTensorView e of
        ----------------
        -- Base cases --
        ----------------
        VBoolTensor t -> return $ VBoolTensor $ mapTensor not t
        VOrderRatTensor op dims x y -> return $ VOrderRatTensor (neg op) dims x y
        VEqualsRatTensor op dims x y -> return $ VEqualsRatTensor (neg op) dims x y
        VQuantifyRatTensor op dims fn -> return $ VQuantifyRatTensor (neg op) dims fn
        VNotTensor _dims x -> return $ toBoolTensorView x
        ---------------------
        -- Inductive cases --
        ---------------------
        VConstBoolTensor tElem v dims -> VConstBoolTensor tElem <$> lowerBool v <*> pure dims
        VOrTensor dims x y -> VAndTensor dims <$> lowerBoolTensor x <*> lowerBoolTensor y
        VAndTensor dims x y -> VOrTensor dims <$> lowerBoolTensor x <*> lowerBoolTensor y
        VStackBoolTensor tElem elemDims n xs -> VStackBoolTensor tElem elemDims n <$> traverse lowerBoolTensor xs
        ---------------------
        -- Unhandled cases --
        ---------------------
        -- We can handle these cases if we know the dimension of the vector concretely?
        VReduceAndTensor dims _ -> return $ VNotTensor dims e
        VReduceOrTensor dims _ -> return $ VNotTensor dims e

    lowerBool :: WHNFValue TensorBuiltin -> m (WHNFValue TensorBuiltin)
    lowerBool = \case
      INullaryBoolTensorOp (BoolLiteral b) -> return $ INullaryBoolTensorOp (BoolLiteral b)
      e -> developerError $ "Unexpected expression of type Bool:" <+> prettyVerbose e

unfoldEquality ::
  WHNFValue TensorBuiltin ->
  WHNFValue TensorBuiltin ->
  WHNFValue TensorBuiltin
unfoldEquality x y = IAnd (IOrderRat Le x y) (IOrderRat Ge x y)

--------------------------------------------------------------------------------
-- Compilation of inequalities

-- | At the moment we only handle linear constraints, and consider all variables
-- equally. In theory we should be able to handle much more complex domains but
-- issues such as the postivity or negativity of arbitrary expressions come into
-- play when solving the inequalities so leaving at this for now.
handleRatInequality ::
  (MonadSearch m) =>
  OrderOp ->
  WHNFValue TensorBuiltin ->
  WHNFValue TensorBuiltin ->
  m ConstrainedValue
handleRatInequality op e1 e2 = do
  result <- compileRatLinearRelation (mkInequality op) e1 e2
  let noResult = (NoConstraints, IOrderRat op e1 e2)
  let exprDoc = IBoolTensorOp (OrderRatTensor op) [explicit e1, explicit e2]
  case result of
    Left blockingExpr -> do
      logDebug MaxDetail $
        "Couldn't compile"
          <+> prettyVerbose exprDoc
          <+> "to a bound"
          <+> "as encountered not currently handled expr"
          <+> prettyVerbose blockingExpr
      return noResult
    Right inequality -> return ([Left inequality], ITrueExpr mempty)

compileRatLinearRelation ::
  (MonadLogger m, MonadReader VariableInfo m) =>
  (LinearExp -> LinearExp -> relation) ->
  WHNFValue TensorBuiltin ->
  WHNFValue TensorBuiltin ->
  m (Either (WHNFValue TensorBuiltin) relation)
compileRatLinearRelation mkRelation x y = runExceptT $ do
  x' <- compileRatLinearExpr x
  y' <- compileRatLinearExpr y
  return $ mkRelation x' y'

type LinearExp = LinearExpr UserElementVariable (WHNFValue TensorBuiltin)

compileRatLinearExpr ::
  forall m.
  (MonadLogger m, MonadReader VariableInfo m, MonadError (WHNFValue TensorBuiltin) m) =>
  WHNFValue TensorBuiltin ->
  m LinearExp
compileRatLinearExpr = go
  where
    go :: WHNFValue TensorBuiltin -> m LinearExp
    go expr = case toRatTensorView expr of
      ----------------
      -- Base cases --
      ----------------
      VRatTensor {} -> return $ constantExpr expr
      VConstRatTensor {} -> return $ constantExpr expr
      VStackRatTensor _ _ _ _ -> _
      VRatTensorVar lv -> do
        VariableInfo {..} <- ask
        let result = lookup lv elementVars
        case result of
          Just var -> return $ singletonVarExpr (IRatLiteral mempty 0) var
          Nothing -> return $ constantExpr (VBoundVar lv [])
      ---------------------
      -- Inductive cases --
      ---------------------
      VNegRatTensor _dims x -> scaleExpr (-1) <$> go x
      VAddRatTensor _dims x y -> addExprs 1 1 <$> go x <*> go y
      VSubRatTensor _dims x y -> addExprs 1 (-1) <$> go x <*> go y
      VMulRatTensor _dims x y -> do
        e1' <- go x
        e2' <- go y
        case (isConstTensor e1', isConstTensor e2') of
          (Just c1, _) -> return $ scaleExpr c1 e2'
          (_, Just c2) -> return $ scaleExpr c2 e1'
          _ -> throwError expr
      VDivRatTensor _dims x y -> do
        x' <- go x
        y' <- go y
        case isConstTensor y' of
          (Just c2) -> return $ scaleExpr (1 / c2) x'
          _ -> throwError expr
      -----------------
      -- Error cases --
      -----------------
      VReduceAddRatTensor {} -> throwError expr
      VReduceMulRatTensor {} -> throwError expr
      VReduceMinRatTensor {} -> throwError expr
      VReduceMaxRatTensor {} -> throwError expr
      VSearchRatTensor {} -> throwError expr
      -- These could be handled by splitting into two constraints?
      VMinRatTensor {} -> throwError expr
      VMaxRatTensor {} -> throwError expr

isConstTensor :: LinearExp -> Maybe Rational
isConstTensor expr = case isConstant expr of
  Just (toRatTensorView -> VConstRatTensor _ (INullaryRatTensorOp (RatLiteral c)) _) -> Just c
  _ -> Nothing

currentPass :: CompilerPass
currentPass = "DomainSearch"
