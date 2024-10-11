{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid NonEmpty.unzip" #-}

module Vehicle.Backend.LossFunction.Domain
  ( extractSearchDomain,
    Domain (..),
  )
where

import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Value

{-
import Control.Monad (foldM, unless, void)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Tuple (swap)
import Vehicle.Backend.LossFunction.Core
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.QuantifiedVariable (reduceTensorVariable, Variable)
import Vehicle.Data.Tensor (mapTensor, Tensor, TensorShape)
import Vehicle.Data.Assertion (UnderConstrainedVariableStatus, checkBoundsExist, Bounds(..), Bound, pattern Bound, mkInequality, Inequality)
import Data.Set qualified as Set (unions, insert, disjoint, fromList, notMember)
import Data.Set (Set)
import Vehicle.Data.Code.LinearExpr
-}

type MonadDomain m =
  ( MonadCompile m,
    MonadNameContext m
  )

data Domain = Domain
  { lowerBound :: Value TensorBuiltin,
    upperBound :: Value TensorBuiltin
  }

extractSearchDomain ::
  (MonadDomain m) =>
  DeclProvenance ->
  VBinder TensorBuiltin ->
  Lv ->
  Value TensorBuiltin ->
  m (Domain, Value TensorBuiltin)
extractSearchDomain _propertyProv _binder _lv value = do
  -- Normalise the body
  let fakeBound = VBuiltin (TensorRat $ RatLiteral 0) []
  return (Domain fakeBound fakeBound, value)

{-
  variableInfo <- case typeOf binder of
    ITensorType _ dims -> do
      maybeReducedVars <- case getDimensions dims of
        Just tensorShape -> do
          logDebug MaxDetail $ "Found concrete tensor shape" <+> pretty tensorShape
          let userVar = getBinderName binder
          let (reducedUseVars, _vectorExpr) = reduceTensorVariable (lv + 1) userVar tensorShape
          return $ Just reducedUseVars
        Nothing -> do
          logDebug MaxDetail $ "Found non-concrete tensor shape" <+> prettyVerbose dims
          return Nothing

      return $
        VariableInfo
          { tensorVarLv = lv,
            tensorVarName = getBinderName binder,
            variablesByLv = _,
            maybeElementVariables = _
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

type VariableShape = Value TensorBuiltin

type TensorInequality = Inequality (Value TensorBuiltin)

type VariableConstraint = TensorInequality

type VariableConstraints = [VariableConstraint]

pattern NoConstraints :: VariableConstraints
pattern NoConstraints = []

type ConstrainedValue = (VariableConstraints, Value TensorBuiltin)

unconstrained :: Value TensorBuiltin -> ConstrainedValue
unconstrained = (NoConstraints,)

updateConstrainedValue ::
  Value TensorBuiltin ->
  ConstrainedValue ->
  ConstrainedValue
updateConstrainedValue originalExpr = \case
  constr@(_ : _, _) -> constr
  ([], _) -> ([], originalExpr)

{-
instance IsConstant (Value TensorBuiltin) where
  isZero = \case
    -- This is only semi-decidable, probably need to think harder about what
    -- to do here.
    IRatLiteral _ 0 -> True
    _ -> False
  scaleConstant c = IMul MulRat (IRatLiteral mempty c)
  addConstants c1 c2 e1 e2 =
    IAdd AddRat (scaleConstant c1 e1) (scaleConstant c2 e2)
-}

getConstant :: LinearExpr (Value TensorBuiltin) -> Maybe Rational
getConstant = _

zeroExpr :: VariableShape -> Value TensorBuiltin
zeroExpr tensorShape = IConstTensor _ _ _

instance Constant (Value TensorBuiltin) where
  addConstants = _
  scaleConstant = _
  isZero = _

--------------------------------------------------------------------------------
-- Constraints

-- | Information for the variable whose domain we are trying to find.
data VariableInfo = VariableInfo
  { tensorVarLv :: Lv,
    tensorVarName :: Name,
    variablesByLv :: Map Lv (VariableShape, Variable),
    maybeElementVariables :: Maybe (TensorShape, [Variable])
  }

allVariables :: VariableInfo -> Set Lv
allVariables VariableInfo{..} = Set.insert tensorVarLv _

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
  m (Either [(Variable, UnderConstrainedVariableStatus)] Domain)
extractDomainFromConstraints VariableInfo {..} constraints = do
  let (tensorElementInequalities, tensorInequalities) = splitConstraints constraints

  (tensorBounds, remainingInequalities) <- fourierMotzkinElimination tensorVarName tensorInequalities

  unless (null remainingInequalities) $
    compilerDeveloperError "Found unused tensor inequalities when solving for bounds. Not currently implemented."

  case checkBoundsExist (tensorVarName, tensorBounds) of
    Right (lowerBounds, upperBounds) -> do
      unless (null tensorElementInequalities) $
        compilerDeveloperError "Found mixed tensor and element inequalities when solving for bounds. Not currently implemented."
      return $ Right $ Domain (convertBoundsToExpr _ _ lowerBounds) (convertBoundsToExpr _ _ upperBounds)
    Left failedTensorVarResult -> case maybeElementVariables of
      Nothing -> return $ Left [failedTensorVarResult]
      Just (tensorShape, elementVariables) -> do
        (remainingElementInequalities, results) <- foldM extractVarBounds (tensorElementInequalities, mempty) elementVariables

        unless (null remainingElementInequalities) $
          compilerDeveloperError "Found unused element inequalities when solving for bounds. Not currently implemented."

        let (unsolvedVars, solvedVars) = partitionEithers (fmap checkBoundsExist results)
        if not $ null unsolvedVars
          then return $ Left unsolvedVars
          else do
            let (lowerBounds, upperBounds) = unzip solvedVars
            let varMap = _
            let lowerBoundElements = fmap (convertBoundsToExpr varMap _) lowerBounds
            let upperBoundElements = fmap (convertBoundsToExpr varMap _) upperBounds
            let lowerBoundExpr = tensorLikeToExpr id tensorShape lowerBoundElements
            let upperBoundExpr = tensorLikeToExpr id tensorShape upperBoundElements
            return $ Right $ Domain lowerBoundExpr upperBoundExpr

extractVarBounds ::
  (MonadCompile m) =>
  ([TensorInequality], [(Variable, Bounds (Value TensorBuiltin))]) ->
  Variable ->
  m ([TensorInequality], [(Variable, Bounds (Value TensorBuiltin))])
extractVarBounds (currentConstraints, solutions) var = do
  (bounds, newInequalities) <- fourierMotzkinElimination var currentConstraints
  return (newInequalities, (var, bounds) : solutions)

convertBoundsToExpr ::
  Map Variable Lv ->
  (Value TensorBuiltin -> Value TensorBuiltin -> Value TensorBuiltin) ->
  NonEmpty (Bound (Value TensorBuiltin)) ->
  Value TensorBuiltin
convertBoundsToExpr varMap op bounds = foldr1 _ (fmap convertBound bounds)
  where
    -- Ignore strictness for the moment.

    convertConstant _first c = c
    convertVariable _first (v, c) = case Map.lookup v varMap of
      Nothing -> developerError $ "Missing variable Lv for" <+> pretty v
      Just lv -> IMul MulRat (IRatLiteral mempty c) (VBoundVar lv [])
    convertBound (Bound _strictness value) = linearExprToExpr convertConstant convertVariable (IAdd AddRat) value

splitConstraints :: VariableConstraints -> ([TensorInequality], [TensorInequality])
splitConstraints = _

--------------------------------------------------------------------------------
-- Constraint search

findConstraints :: (MonadSearch m) => Value TensorBuiltin -> m ConstrainedValue
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
  -----------------------
  -- Useful base cases --
  -----------------------
  VOrderRatTensor op dims x y -> handleInequality op dims x y
  ---------------------
  -- Recursive cases --
  ---------------------
  VEqualsRatTensor Eq dims x y ->
    updateConstrainedValue expr <$> findConstraints (unfoldEquality dims x y)
  VAndTensor dims x y -> do
    (cx, x') <- findConstraints x
    (cy, y') <- findConstraints y
    return (cx <> cy, fromBoolTensorView (VAndTensor dims x' y'))
  VNotTensor _ x -> handleNot x

handleNot ::
  forall m.
  (MonadSearch m) =>
  Value TensorBuiltin ->
  m ConstrainedValue
handleNot expr = do
  loweredExpr <- lowerBoolTensor expr
  case toBoolTensorView loweredExpr of
    VNotTensor {} -> return $ unconstrained expr
    _ -> updateConstrainedValue expr <$> findConstraints loweredExpr
  where
    lowerBoolTensor :: Value TensorBuiltin -> m (Value TensorBuiltin)
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
        VConstBoolTensor v dims -> VConstBoolTensor <$> lowerBool v <*> pure dims
        VOrTensor dims x y -> VAndTensor dims <$> lowerBoolTensor x <*> lowerBoolTensor y
        VAndTensor dims x y -> VOrTensor dims <$> lowerBoolTensor x <*> lowerBoolTensor y
        VStackBoolTensor elemDims n xs -> VStackBoolTensor elemDims n <$> traverse lowerBoolTensor xs
        ---------------------
        -- Unhandled cases --
        ---------------------
        -- We can handle these cases if we know the dimension of the vector concretely?
        VReduceAndTensor dims _ -> return $ VNotTensor dims e
        VReduceOrTensor dims _ -> return $ VNotTensor dims e

    lowerBool :: Value TensorBuiltin -> m (Value TensorBuiltin)
    lowerBool = \case
      INullaryBoolTensorOp (BoolLiteral b) -> return $ INullaryBoolTensorOp (BoolLiteral b)
      e -> developerError $ "Unexpected expression of type Bool:" <+> prettyVerbose e

unfoldEquality ::
  VArg TensorBuiltin ->
  Value TensorBuiltin ->
  Value TensorBuiltin ->
  Value TensorBuiltin
unfoldEquality dims x y =
  fromBoolTensorView $ VAndTensor
    dims
    (fromBoolTensorView $ VOrderRatTensor Le dims x y)
    (fromBoolTensorView $ VOrderRatTensor Ge dims x y)

--------------------------------------------------------------------------------
-- Compilation of inequalities

-- | At the moment we only handle linear constraints, and consider all variables
-- equally. In theory we should be able to handle much more complex domains but
-- issues such as the postivity or negativity of arbitrary expressions come into
-- play when solving the inequalities so leaving at this for now.
handleInequality ::
  (MonadSearch m) =>
  OrderOp ->
  VArg TensorBuiltin ->
  Value TensorBuiltin ->
  Value TensorBuiltin ->
  m ConstrainedValue
handleInequality op dims e1 e2 = do
  result <- compileRatLinearRelation (mkInequality op) e1 e2
  let noResult = (NoConstraints, fromBoolTensorView $ VOrderRatTensor op dims e1 e2)
  let exprDoc = IBoolTensorOp (OrderRatTensor op) [explicit e1, explicit e2]
  case result of
    Left blockingExpr -> do
      logDebug MaxDetail $
        "Couldn't compile"
          <+> prettyVerbose exprDoc
          <+> "to a bound"
          <+> "as encountered a currently unhandled expression"
          <+> prettyVerbose blockingExpr
      return noResult
    Right inequality ->
      return ([inequality], fromBoolTensorView $ VConstBoolTensor (INullaryBoolTensorOp (BoolLiteral True)) (argExpr dims))

compileRatLinearRelation ::
  (MonadLogger m, MonadReader VariableInfo m) =>
  (LinearExpr (Value TensorBuiltin) -> LinearExpr (Value TensorBuiltin) -> relation) ->
  Value TensorBuiltin ->
  Value TensorBuiltin ->
  m (Either (Value TensorBuiltin) relation)
compileRatLinearRelation mkRelation x y = runExceptT $ do
  x' <- compileRatLinearExpr x
  y' <- compileRatLinearExpr y
  return $ mkRelation x' y'

compileRatLinearExpr ::
  forall m.
  (MonadLogger m, MonadReader VariableInfo m, MonadError (Value TensorBuiltin) m) =>
  Value TensorBuiltin ->
  m (LinearExpr (Value TensorBuiltin))
compileRatLinearExpr expr = case toRatTensorView expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensor {} -> return $ constantExpr expr
  VConstRatTensor {} -> return $ constantExpr expr
  VRatTensorVar lv -> do
    VariableInfo {..} <- ask
    case Map.lookup lv variablesByLv of
      Just (varShape, var) -> return $ singletonVarExpr (zeroExpr varShape) var
      Nothing -> return $ constantExpr (VBoundVar lv [])
  ---------------------
  -- Inductive cases --
  ---------------------
  VNegRatTensor _dims x -> scaleExpr (-1) <$> compileRatLinearExpr x
  VAddRatTensor _dims x y -> addExprs 1 1 <$> compileRatLinearExpr x <*> compileRatLinearExpr y
  VSubRatTensor _dims x y -> addExprs 1 (-1) <$> compileRatLinearExpr x <*> compileRatLinearExpr y
  VMulRatTensor _dims x y -> do
    e1' <- compileRatLinearExpr x
    e2' <- compileRatLinearExpr y
    case (getConstant e1', getConstant e2') of
      (Just c1, _) -> return $ scaleExpr c1 e2'
      (_, Just c2) -> return $ scaleExpr c2 e1'
      _ -> unhandled
  VDivRatTensor _dims x y -> do
    x' <- compileRatLinearExpr x
    y' <- compileRatLinearExpr y
    case getConstant y' of
      (Just c2) -> return $ scaleExpr (1 / c2) x'
      _ -> unhandled
  -- Min/max could be handled by splitting into two constraints?
  -----------------
  -- Error cases --
  -----------------
  VSearchRatTensor {} -> throwError expr
  _ -> unhandled
  where
    unhandled :: m (LinearExpr (Value TensorBuiltin))
    unhandled = do
      quantifiedVariables <- asks allVariables
      if Set.disjoint quantifiedVariables (variablesIn expr)
        then return $ constantExpr expr
        else throwError expr

variablesIn :: Value builtin -> Set Lv
variablesIn = \case
  VBoundVar lv spine -> Set.insert lv (variablesInSpine spine)
  VBuiltin _ spine -> variablesInSpine spine
  VFreeVar {} -> unexpectedExprError currentPass "VFreeVar"
  VMeta {} -> unexpectedExprError currentPass "VMeta"
  VLam {} -> unexpectedExprError currentPass "VLam"
  VPi {} -> unexpectedExprError currentPass "VPi"
  VUniverse {} -> unexpectedExprError currentPass "VUniverse"

variablesInSpine :: Spine builtin -> Set Lv
variablesInSpine spine = Set.unions (fmap (variablesIn . argExpr) spine)

currentPass :: CompilerPass
currentPass = "DomainSearch"

  -}
