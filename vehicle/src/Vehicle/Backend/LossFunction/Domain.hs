{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.LossFunction.Domain
  ( extractSearchDomain,
    Domain (..),
  )
where

import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface (pattern IRatLiteral)
import Vehicle.Data.Code.Value

{-
import Control.Monad (foldM, unless, void)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Vehicle.Backend.LossFunction.Core
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.FourierMotzkinElimination (fourierMotzkinElimination)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Builtin.Standard

import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.QuantifiedVariable (reduceTensorVariable, Variable, SliceVariable)
import Vehicle.Data.Tensor (mapTensor, TensorShape)
import Vehicle.Data.Assertion (UnderConstrainedVariableStatus, checkBoundsExist, Bounds(..), Bound, pattern Bound, mkInequality, Inequality(..), LowerBound, UpperBound)
import Data.Set qualified as Set (unions, insert, disjoint)
import Data.Set (Set)
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Builtin.Interface.Normalise
import Data.List (partition)
-}

type MonadDomain m =
  ( MonadCompile m,
    MonadNameContext m
  )

data Domain = Domain
  { lowerBound :: Value Builtin,
    upperBound :: Value Builtin
  }

extractSearchDomain ::
  (MonadDomain m) =>
  DeclProvenance ->
  VBinder Builtin ->
  Lv ->
  Value Builtin ->
  m (Domain, Value Builtin)
extractSearchDomain _propertyProv _binder _lv value = do
  let fakeBound = IRatLiteral 0
  return (Domain fakeBound fakeBound, value)

{-
  let varName = getBinderName binder

  dims <- case typeOf binder of
    ITensorType _ dims -> return dims
    _ -> compilerDeveloperError "Unexpected quantifier type"

  (newNames, elementInfo) <- case getDimensions dims of
    Just tensorShape -> do
      logDebug MaxDetail $ "Found concrete tensor shape" <+> pretty tensorShape
      let (reducedUseVars, _vectorExpr) = reduceTensorVariable lv varName tensorShape
      let elementInfo = Just $ VariableElementsInfo tensorShape (fmap fst reducedUseVars)
      let newNames = varName : fmap snd reducedUseVars
      return (newNames, elementInfo)
    Nothing -> do
      logDebug MaxDetail $ "Found non-concrete tensor shape" <+> prettyVerbose dims
      return ([varName], Nothing)

  let variableInfo = VariableInfo
        { tensorVarLv = lv,
          tensorVarName = varName,
          tensorVarShape = argExpr dims,
          tensorVarElements = elementInfo,
          tensorAndElementVariables = Set.fromList _
        }

  -- Search for constraints
  (constraints, remainder) <- flip runReaderT variableInfo $ findConstraints value
  maybeDomain <- extractDomainFromConstraints variableInfo constraints
  case maybeDomain of
    Left missingCostraints ->
      throwError $ NoQuantifierDomainFound propertyProv (void binder) (Just missingCostraints)
    Right domain -> return (domain, remainder)

--------------------------------------------------------------------------------
-- Constraints

type VariableShape = Value Builtin

type TensorInequality = Inequality (Value Builtin)

type VariableConstraint = TensorInequality

type VariableConstraints = [VariableConstraint]

pattern NoConstraints :: VariableConstraints
pattern NoConstraints = []

type ConstrainedValue = (VariableConstraints, Value Builtin)

unconstrained :: Value Builtin -> ConstrainedValue
unconstrained = (NoConstraints,)

updateConstrainedValue ::
  Value Builtin ->
  ConstrainedValue ->
  ConstrainedValue
updateConstrainedValue originalExpr = \case
  constr@(_ : _, _) -> constr
  ([], _) -> ([], originalExpr)

instance Constant (Value Builtin) where
  isZero = \case
    -- This is only semi-decidable, probably need to think harder about what
    -- to do here.
    IRatLiteral 0 -> True
    _ -> False
  scaleConstant c = IMul MulRat (IRatLiteral c)
  addConstants c1 c2 e1 e2 =
    IAdd AddRat (scaleConstant c1 e1) (scaleConstant c2 e2)

getConstant :: LinearExpr (Value Builtin) -> Maybe Rational
getConstant e = case isConstant e of
  Just x -> case toRatTensorView x of
    VRatConstTensor (IRatLiteral r) _ -> Just r
    _ -> Nothing
  Nothing -> Nothing

zeroExpr :: VariableShape -> Value Builtin
zeroExpr tensorShape = fromRatTensorView $ VRatConstTensor (IRatLiteral 0) tensorShape

--------------------------------------------------------------------------------
-- Constraints

-- | Information for the variable whose domain we are trying to find.
data VariableInfo = VariableInfo
  { tensorVarLv :: Lv,
    tensorVarName :: Name,
    tensorVarShape :: VariableShape,
    tensorAndElementVariables :: Set Variable,
    tensorVarElements :: Maybe VariableElementsInfo
  }

data VariableElementsInfo = VariableElementsInfo
  { concreteShape :: TensorShape
  , elementVariables :: [Variable]
  }

lookupVariableShape :: Lv -> VariableInfo -> Maybe VariableShape
lookupVariableShape lv VariableInfo{..}
  | lv == tensorVarLv = Just tensorVarShape
  | otherwise = fmap (const (dimSingleton 1)) tensorVarElements

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
  let (tensorElementInequalities, tensorInequalities) = splitConstraints tensorVarLv constraints
  (tensorBounds, remainingInequalities) <- fourierMotzkinElimination tensorVarLv tensorInequalities

  unless (null remainingInequalities) $
    compilerDeveloperError "Found unused tensor inequalities when solving for bounds. Not currently implemented."

  case checkBoundsExist (tensorVarLv, tensorBounds) of
    Right (lowerBounds, upperBounds) -> do
      unless (null tensorElementInequalities) $
        compilerDeveloperError "Found mixed tensor and element inequalities when solving for bounds. Not currently implemented."
      let lowerBoundsExpr = convertLowerBounds tensorVarShape lowerBounds
      let upperBoundsExpr = convertUpperBounds tensorVarShape upperBounds
      return $ Right $ Domain lowerBoundsExpr upperBoundsExpr

    Left failedTensorVarResult -> case tensorVarElements of
      Nothing -> return $ Left [failedTensorVarResult]
      Just VariableElementsInfo{..} -> do
        (remainingElementInequalities, results) <- foldM extractVarBounds (tensorElementInequalities, mempty) elementVariables

        unless (null remainingElementInequalities) $
          compilerDeveloperError "Found unused element inequalities when solving for bounds. Not currently implemented."

        let (unsolvedVars, solvedVars) = partitionEithers (fmap checkBoundsExist results)
        if not $ null unsolvedVars
          then return $ Left unsolvedVars
          else do
            let (lowerBounds, upperBounds) = unzip solvedVars
            let lowerBoundElements = fmap (convertLowerBounds (dimSingleton 1)) lowerBounds
            let upperBoundElements = fmap (convertUpperBounds (dimSingleton 1)) upperBounds
            let lowerBoundExpr = tensorLikeToExpr id concreteShape lowerBoundElements
            let upperBoundExpr = tensorLikeToExpr id concreteShape upperBoundElements
            return $ Right $ Domain lowerBoundExpr upperBoundExpr

mkTensorLayer ::
  (HasStandardVecLits expr, HasStandardListLits expr, HasNatLits expr) =>
  TensorShape ->
  [expr] ->
  expr
mkTensorLayer dims xs = do
  let dimsExpr = mkListExpr (INatType mempty) (fmap (INatLiteral) dims)
  let elementType = Arg mempty (Implicit True) Relevant dimsExpr
  let elements = fmap (Arg mempty Explicit Relevant) xs
  mkHomoVector elementType elements

tensorLikeToExpr ::
  (HasStandardVecLits expr, HasStandardListLits expr, HasNatLits expr) =>
  (a -> expr) ->
  TensorShape ->
  [a] ->
  expr
tensorLikeToExpr mkElem = foldMapTensorLike mkElem mkTensorLayer

extractVarBounds ::
  (MonadCompile m) =>
  ([TensorInequality], [(Variable, Bounds (Value Builtin))]) ->
  Variable ->
  m ([TensorInequality], [(Variable, Bounds (Value Builtin))])
extractVarBounds (currentConstraints, solutions) var = do
  (bounds, newInequalities) <- fourierMotzkinElimination var currentConstraints
  return (newInequalities, (var, bounds) : solutions)

convertLowerBounds ::
  VariableShape ->
  NonEmpty (LowerBound (Value Builtin)) ->
  Value Builtin
convertLowerBounds shape = convertBoundsToExpr $ \x y -> do
  let defaultExpr = fromRatTensorView $ VMaxRatTensor (implicitIrrelevant shape) x y
  evalMaxRatTensor defaultExpr [explicit x, explicit y]

convertUpperBounds ::
  VariableShape ->
  NonEmpty (UpperBound (Value Builtin)) ->
  Value Builtin
convertUpperBounds shape = convertBoundsToExpr $ \x y -> do
  let defaultExpr = fromRatTensorView $ VMinRatTensor (implicitIrrelevant shape) x y
  evalMinRatTensor defaultExpr [explicit x, explicit y]

convertBoundsToExpr ::
  (Value Builtin -> Value Builtin -> Value Builtin) ->
  NonEmpty (Bound (Value Builtin)) ->
  Value Builtin
convertBoundsToExpr op bounds = foldr1 op (fmap convertBound bounds)
  where
    convertConstant _first c = c
    convertVariable _first (v, c) = IMul MulRat (IRatLiteral c) (VBoundVar v [])
    convertBound (Bound _strictness value) =
      -- Ignore strictness for the moment.
      linearExprToExpr convertConstant convertVariable (IAdd AddRat) value

splitConstraints :: SliceVariable -> VariableConstraints -> ([TensorInequality], [TensorInequality])
splitConstraints var = partition (\ineq -> inequalityExpr ineq `referencesVariable` var)

--------------------------------------------------------------------------------
-- Constraint search

findConstraints :: (MonadSearch m) => Value Builtin -> m ConstrainedValue
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
  VBoolStackTensor {} -> return $ unconstrained expr
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
  Value Builtin ->
  m ConstrainedValue
handleNot expr = do
  loweredExpr <- lowerBoolTensor expr
  case toBoolTensorView loweredExpr of
    VNotTensor {} -> return $ unconstrained expr
    _ -> updateConstrainedValue expr <$> findConstraints loweredExpr
  where
    lowerBoolTensor :: Value Builtin -> m (Value Builtin)
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
        VBoolStackTensor elemDims n xs -> VBoolStackTensor elemDims n <$> traverse lowerBoolTensor xs
        ---------------------
        -- Unhandled cases --
        ---------------------
        -- We can handle these cases if we know the dimension of the vector concretely?
        VReduceAndTensor dims _ -> return $ VNotTensor dims e
        VReduceOrTensor dims _ -> return $ VNotTensor dims e

    lowerBool :: Value Builtin -> m (Value Builtin)
    lowerBool = \case
      INullaryBoolTensorOp (BoolLiteral b) -> return $ INullaryBoolTensorOp (BoolLiteral b)
      e -> developerError $ "Unexpected expression of type Bool:" <+> prettyVerbose e

unfoldEquality ::
  VArg Builtin ->
  Value Builtin ->
  Value Builtin ->
  Value Builtin
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
  VArg Builtin ->
  Value Builtin ->
  Value Builtin ->
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
  (LinearExpr (Value Builtin) -> LinearExpr (Value Builtin) -> relation) ->
  Value Builtin ->
  Value Builtin ->
  m (Either (Value Builtin) relation)
compileRatLinearRelation mkRelation x y = runExceptT $ do
  x' <- compileRatLinearExpr x
  y' <- compileRatLinearExpr y
  return $ mkRelation x' y'

compileRatLinearExpr ::
  forall m.
  (MonadLogger m, MonadReader VariableInfo m, MonadError (Value Builtin) m) =>
  Value Builtin ->
  m (LinearExpr (Value Builtin))
compileRatLinearExpr expr = case toRatTensorView expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensor {} -> return $ constantExpr expr
  VRatConstTensor {} -> return $ constantExpr expr
  VRatTensorVar var -> do
    variableInfo <- ask
    return $ case lookupVariableShape var variableInfo of
      Just shape -> singletonVarExpr (zeroExpr shape) var
      Nothing -> constantExpr (VBoundVar var [])
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
    unhandled :: m (LinearExpr (Value Builtin))
    unhandled = do
      quantifiedVariables <- asks tensorAndElementVariables
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
