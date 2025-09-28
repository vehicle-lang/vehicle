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
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value

{-
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set (disjoint, member, toList)
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.Assertion (Assertion, Bound, Bounds (..), InequalityRelation (..), NormalisedRelation (..), Relation (..), UnderConstrainedVariableStatus (..), checkBoundsExist, comparisonToAssertion)
import Vehicle.Data.Builtin.Interface.Normalise (evalAtTensor, evalMaxRatTensor, evalMinRatTensor, evalReduceMaxRatTensor, evalReduceMinRatTensor, evalStackTensor, evalSubRatTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (BoolValue (..), NatValue (..), RatTensorValue (..), TypeValue (..), fromBoolValue, fromNatValue, fromRatTensorValue, fromTypeValue, toBoolValue, toRatTensorValue, toTypeValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorIndices, TensorShape)
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

  dims <- case toTypeValue $ typeOf binder of
    VRatTensorType dims -> return dims
    _ -> compilerDeveloperError "Unexpected quantifier type"

  (newNames, elementInfo) <- case getDims dims of
    Just tensorShape -> do
      logDebug MaxDetail $ "Found concrete tensor shape" <+> pretty tensorShape
      let (reducedUseVars, _vectorExpr) = reduceTensorVariable lv varName tensorShape
      let elementInfo = Just $ VariableElementsInfo tensorShape (fmap fst reducedUseVars)
      let newNames = varName : fmap snd reducedUseVars
      return (newNames, elementInfo)
    Nothing -> do
      logDebug MaxDetail $ "Found non-concrete tensor shape" <+> prettyVerbose dims
      return ([varName], Nothing)

  -- Search for constraints
  let variableDims = Map.fromList $ (_ : _) : _
  let searchData =
        SearchData
          { networkVariables = mempty,
            variablesOfInterest = variableDims
          }
  (remainder, constraints) <- runReaderT (findConstraints value) searchData

  -- Extract the domain
  errorOrDomain <- extractDomainFromConstraints _ constraints
  case errorOrDomain of
    Left err -> _
    Right domain -> return (domain, remainder)

{-
--------------------------------------------------------------------------------
-- Constraints

type VariableShape = Value Builtin

type TensorInequality = Inequality (Value Builtin)

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
<<<<<<< HEAD
-}

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

-- type ValueBound = Bound (Value Builtin)

type ValueBounds = Bounds (Value Builtin)

type Constraints = Map Lv ValueBounds

type ConstrainedValue = (Value Builtin, Constraints)

andConstrainedValue :: VArg Builtin -> ConstrainedValue -> ConstrainedValue -> ConstrainedValue
andConstrainedValue dims (x, cx) (y, cy) = do
  let v = fromBoolValue (VAnd (TensorOp2Args dims x y))
  let cv = Map.unionWith (<>) cx cy
  (v, cv)

--------------------------------------------------------------------------------
-- Global variables during search

type MonadDomainSearch m =
  ( MonadNameContext m,
    MonadLogger m,
    MonadReader SearchData m
  )

data SearchData = SearchData
  { networkVariables :: Set Lv,
    variablesOfInterest :: Map Lv (Value Builtin)
  }

data VariableClassification
  = DifferentiableVariable
  | UnrelatedVariable
  | TargetVariable (Value Builtin)

classifyVariable :: Lv -> SearchData -> VariableClassification
classifyVariable lv SearchData {..}
  | Set.member lv networkVariables = DifferentiableVariable
  | otherwise = maybe UnrelatedVariable TargetVariable (Map.lookup lv variablesOfInterest)

--------------------------------------------------------------------------------
-- Search algorithm

findConstraints :: (MonadDomainSearch m) => Value Builtin -> m ConstrainedValue
findConstraints expr = case toBoolValue expr of
  -------------------------
  -- Unuseful base cases --
  -------------------------
  VBoolLiteral {} -> unconstrained
  VQuantifyRatTensor {} -> unconstrained
  -- These two cases need to be altered if we are to handle disjoint domains?
  VOr {} -> unconstrained
  VReduceOrTensor {} -> unconstrained
  -- Maybe we can do something with these?
  VReduceAndTensor {} -> unconstrained
  VNot {} -> unconstrained
  VCompareNat {} -> unconstrained
  VCompareIndex {} -> unconstrained
  VBoolAt {} -> unconstrained
  VBoolIf {} -> unconstrained
  -----------------------
  -- Useful base cases --
  -----------------------
  VCompareRatTensor args -> handleComparison args
  ---------------------
  -- Recursive cases --
  ---------------------
  VAnd (TensorOp2Args dims e1 e2) -> andConstrainedValue dims <$> findConstraints e1 <*> findConstraints e2
  where
    unconstrained = return (expr, mempty)

{-
handleNot ::
  forall m.
  (MonadDomainSearch m) =>
  Value Builtin ->
  m ConstrainedValue
handleNot expr = do
  loweredExpr <- lowerBoolTensor expr
  case toBoolTensorView loweredExpr of
    VNot {} -> return $ unconstrained expr
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
-}

-- | At the moment we only handle linear constraints, and consider all variables
-- equally. In theory we should be able to handle much more complex domains but
-- issues such as the postivity or negativity of arbitrary expressions come into
-- play when solving the inequalities so leaving at this for now.
handleComparison ::
  (MonadDomainSearch m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m ConstrainedValue
handleComparison (op, args@(TensorOp2Args dims e1 e2)) = do
  maybeResult <- case op of
    Ne -> return Nothing
    _ -> do
      let evalSub x y = evalSubRatTensor (TensorOp2Args dims x y)
      NormalisedRelation rel combinedValue <- comparisonToAssertion op evalSub e1 e2
      errorOrResult <- runExceptT $ compileLinearExpr combinedValue
      case errorOrResult of
        Left {} -> return Nothing
        Right linearExpr -> case Map.toList (coefficients linearExpr) of
          [(var, _)] -> do
            let (prevCoefficient, rearrangedExpr) = rearrangeExprToSolveFor var linearExpr
            let bound = constantValue rearrangedExpr
            bounds <- extractVarBounds rel prevCoefficient bound
            return $ Just $ Map.singleton var bounds
          _ -> return Nothing

  let originalValue = fromBoolValue $ VCompareRatTensorReduced (op, args)
  let trivialValue = fromBoolValue $ VBoolLiteral True
  return $ maybe (originalValue, mempty) (trivialValue,) maybeResult

extractVarBounds ::
  (MonadCompile m) =>
  Relation ->
  Coefficient ->
  Value Builtin ->
  m ValueBounds
extractVarBounds rel prevCoefficient value = do
  let (lowerBounds, upperBounds) = case rel of
        OEq -> (Just NonStrict, Just NonStrict)
        OLt
          | prevCoefficient < -1 -> (Just Strict, Nothing)
          | otherwise -> (Nothing, Just Strict)
        OLe
          | prevCoefficient < -1 -> (Just NonStrict, Nothing)
          | otherwise -> (Nothing, Just NonStrict)

  let mkBound re = NormalisedRelation re value
  return $
    Bounds
      { lowerBounds = maybeToList $ fmap mkBound lowerBounds,
        upperBounds = maybeToList $ fmap mkBound upperBounds
      }

compileLinearExpr ::
  (MonadDomainSearch m, MonadError (Value Builtin) m) =>
  Value Builtin ->
  m (LinearExpr Lv (Value Builtin))
compileLinearExpr expr = case toRatTensorValue expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensorLiteral {} -> return $ constantExpr expr
  VRatConstTensor {} -> return $ constantExpr expr
  VRatTensorVar var -> do
    classification <- asks (classifyVariable var)
    case classification of
      TargetVariable dims -> return $ singletonVarExpr dims var
      _ -> unlinearisable
  ---------------------
  -- Inductive cases --
  ---------------------
  VNegRatTensor (TensorOp1Args _ e) -> scaleExpr (-1) <$> compileLinearExpr e
  VAddRatTensor (TensorOp2Args _ e1 e2) -> addExprs 1 1 <$> compileLinearExpr e1 <*> compileLinearExpr e2
  VSubRatTensor (TensorOp2Args _ e1 e2) -> addExprs 1 (-1) <$> compileLinearExpr e1 <*> compileLinearExpr e2
  VMulRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
  {-
    e1' <- compileLinearExpr e1
    e2' <- compileLinearExpr e2
    case (isConstant e1', isConstant e2') of
      (Just c1, _) -> return $ scaleExpr c1 e2'
      (_, Just c2) -> return $ scaleExpr c2 e1'
      _ -> unlinearisable
  -}
  VDivRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
  {-do
    e1' <- compileLinearExpr e1
    e2' <- compileLinearExpr e2
    case isConstant e2' of
      (Just c2) -> return $ scaleExpr (1 / c2) e1'
      _ -> unlinearisable-}
  ---------------------
  -- Unreduced cases --
  ---------------------
  -- The expression is being blocked
  VRatStackTensor {} -> unlinearisable
  VRatAt {} -> unlinearisable
  VNetworkApp {} -> unlinearisable
  VRatForeach {} -> unlinearisable
  VIfRatTensor {} -> unlinearisable
  -----------------------
  -- Unsupported cases --
  -----------------------
  -- Min/max could be handled by splitting into two constraints?
  VMinRatTensor {} -> unlinearisable
  VMaxRatTensor {} -> unlinearisable
  VReduceAddRatTensor {} -> unlinearisable
  VReduceMulRatTensor {} -> unlinearisable
  VReduceMinRatTensor {} -> unlinearisable
  VReduceMaxRatTensor {} -> unlinearisable
  where
    unlinearisable :: m (LinearExpr Lv (Value Builtin))
    unlinearisable = do
      searchData <- ask
      referencedVariables <- fmap (`classifyVariable` searchData) $ Set.toList $ variablesIn expr
      if Set.disjoint quantifiedVariables ()
        then return $ constantExpr expr
        else throwError expr

variablesIn :: Value Builtin -> Set Lv
variablesIn = _

--------------------------------------------------------------------------------
-- Domain

-- | Information for the variable whose domain we are trying to find.
data VariableInfo = VariableInfo
  { tensorVarLv :: Lv,
    tensorVarShape :: TensorShape,
    tensorAndElementVariables :: Set Lv,
    maybeTensorVarElements :: Maybe VariableElementsInfo
  }

type MonadDomainCalculation m = MonadCompile m

data VariableElementsInfo = VariableElementsInfo
  { concreteShape :: TensorShape,
    elementVariables :: [Lv]
  }

data DomainError
  = InsufficientTensorConstraints UnderConstrainedVariableStatus
  | InsufficientElementConstraints (NonEmpty (Lv, UnderConstrainedVariableStatus))

extractDomainFromConstraints ::
  (MonadDomainCalculation m) =>
  VariableInfo ->
  Constraints ->
  m (Either DomainError Domain)
extractDomainFromConstraints (VariableInfo tensorVar _ _ maybeTensorVarElements) constraints
  | Map.null constraints = return $ Left $ InsufficientTensorConstraints Unconstrained
  | otherwise =
      case Map.lookup tensorVar constraints of
        Just tensorBounds | Map.size constraints == 1 -> do
          -- If we *only* have tensor level constraints
          errorOrDomain <- processBounds _ tensorBounds
          case errorOrDomain of
            Left err -> return $ Left $ InsufficientTensorConstraints err
            Right domain -> return $ Right domain
        maybeTensorConstraints -> case maybeTensorVarElements of
          Nothing -> _
          Just VariableElementsInfo {..} -> do
            -- Otherwise if we have a mix of tensor and element level bounds
            let tensorBounds = fromMaybe mempty maybeTensorConstraints
            elementErrorOrDomains <- forM elementVariables $ \elementVar -> do
              let elementVarBoundsFromElement = fromMaybe mempty $ Map.lookup elementVar constraints
              elementVarBoundsFromTensor <- extractElementBoundsFromTensorBounds elementVar tensorBounds
              let elementVarBounds = elementVarBoundsFromElement <> elementVarBoundsFromTensor
              errorOrDomain <- processBounds _ elementVarBounds
              return $ first (elementVar,) errorOrDomain

            let (errs, elementDomains) = partitionEithers elementErrorOrDomains
            case errs of
              (e : es) -> return $ Left $ InsufficientElementConstraints (e :| es)
              [] -> Right <$> combineElementDomains elementDomains

combineElementDomains :: (MonadDomainCalculation m) => TensorShape -> [Domain] -> m Domain
combineElementDomains tensorShape elementDomains = do
  lowerBound <- tensorLikeToExpr tensorShape (fmap lowerBound elementDomains)
  upperBound <- tensorLikeToExpr tensorShape (fmap upperBound elementDomains)
  return $
    Domain
      { lowerBound = lowerBound,
        upperBound = upperBound
      }

extractElementBoundsFromTensorBounds ::
  (MonadDomainCalculation m) =>
  Lv ->
  ValueBounds ->
  m ValueBounds
extractElementBoundsFromTensorBounds lv (Bounds lowerBounds upperBounds) = do
  let extractDim index value = evalAtTensor (AtTensorArgs _ _ _ _ _)
  let indices = _ :: TensorIndices
  let extractBound (NormalisedRelation rel value) = NormalisedRelation rel <$> foldrM extractDim value indices
  lowerBounds' <- traverse extractBound lowerBounds
  upperBounds' <- traverse extractBound lowerBounds
  return $ Bounds lowerBounds' upperBounds'

processBounds ::
  (MonadDomainCalculation m) =>
  VArg Builtin ->
  ValueBounds ->
  m (Either UnderConstrainedVariableStatus Domain)
processBounds dims bounds = case checkBoundsExist bounds of
  Left err -> return $ Left err
  Right (lowerBounds, upperBounds) -> do
    let mkTensor values =
          evalStackTensor $
            StackTensorArgs
              { stackType = implicit $ fromTypeValue VRatType,
                stackFirstDim = fromNatValue $ VNatLiteral (length bounds),
                stackRemainingDims = dims,
                stackElements = NonEmpty.toList values
              }
    let mkTensorReductionArgs values =
          TensorOp2Args
            { tensorOp2Dims = _,
              tensorOp2Arg1 = _,
              tensorOp2Arg2 = _
            }
    let evalBounds evalFn bs = foldrM _ (NonEmpty.head bs) (NonEmpty.tail bs)

    lowerBound <- evalBounds evalMaxRatTensor lowerBounds
    upperBound <- evalBounds evalMinRatTensor upperBounds
    return $ Right $ Domain lowerBound upperBound

tensorLikeToExpr :: (MonadDomainCalculation m) => TensorShape -> [Value Builtin] -> m (Value Builtin)
tensorLikeToExpr = foldMapTensorLike mkElem mkTensorLayer
  where
    mkElem = id
    mkTensorLayer shape values =
      evalStackTensor $
        StackTensorArgs
          { stackType = implicit $ fromTypeValue VRatType,
            stackFirstDim = fromNatValue $ VNatLiteral (length values),
            stackRemainingDims = mkDims shape,
            stackElements = values
          }
-}
