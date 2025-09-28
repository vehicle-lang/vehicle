module Vehicle.Backend.LossFunction.Domain
  ( extractSearchDomain,
    Domain (..),
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Free.Context.Class

{-
import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion (BoundType (..), Inequality, InequalityRelation (..), NormalisedRelation (..), Relation (..), combineInequalityRelations, comparisonToAssertion)
import Vehicle.Data.Builtin.Interface.Normalise (evalAddRatTensor, evalAnd, evalConstTensor, evalMaxRatTensor, evalMinRatTensor, evalMulRatTensor, evalStackTensor, evalSubRatTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView (BoolValue (..), RatTensorValue (..), TypeValue (..), etaReduceTensor, fromBoolValue, fromRatTensorValue, toBoolValue, toRatTensorValue, toTypeValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Tensor
import Vehicle.Data.Tensor (TensorIndices, TensorShape, shapeOf)
import Vehicle.Prelude.Logging (runSilentLogger)
import Data.These (These(..))
-}

data Domain = Domain
  { lowerBound :: Value Builtin,
    upperBound :: Value Builtin
  }

extractSearchDomain ::
  (MonadCompile m, MonadNameContext m, MonadFreeContext Builtin m) =>
  DeclProvenance ->
  VBinder Builtin ->
  Lv ->
  Value Builtin ->
  m (Domain, Value Builtin)
extractSearchDomain _propertyProv _binder _lv value = do
  let fakeBound = IRatLiteral 0
  return (Domain fakeBound fakeBound, value)

{-
  do
  let name = getBinderName binder

  logCompilerSection2 MidDetail ("extracting domain for" <+> quotePretty name) $ do
    let (concreteShapePrefix, abstractShapeSuffix) = case toTypeValue $ typeOf binder of
          VRatTensorType dims -> calculateVariableDimensions dims
          _ -> developerError "Unexpected quantifier type"

    let searchCtx =
          SearchContext
            { targetVariable = mkNestedSliceVariable concreteShapePrefix (coerce lv),
              abstractShapeSuffix = abstractShapeSuffix
            }

    -- Make names for all the slices that we actually know about.
    let newNames = variableNamesForAllSlices name concreteShapePrefix
    addNamesToContext (reverse newNames) $ do
      logDebugM MaxDetail $ do
        nameCtx <- getNameContext

        doc <- prettyFriendlyInCtx value
        return $ pretty nameCtx <> line <> "Body:" <+> doc

      -- Search for constraints
      result@(ConstrainedValue tensorLowerBounds tensorUpperBounds remainder) <- runReaderT (findConstraints value) searchCtx

      logDebugM MidDetail $ do
        nameCtx <- getNameContext
        return $ "Found:" <> lineIndent (prettyConstrainedValue nameCtx result)

      -- Extract the domain
      errorOrLowerBound <- runReaderT (extractSingleBound Lower tensorLowerBounds) (searchCtx, mempty)
      errorOrUpperBounds <- runReaderT (extractSingleBound Upper tensorUpperBounds) (searchCtx, mempty)
      let errorOrDomain = theseErrors Domain errorOrLowerBound errorOrUpperBounds
      case errorOrDomain of
        Right domain -> return (domain, remainder)
        Left err -> throwError $ NoQuantifierDomainFound propertyProv binder err

calculateVariableDimensions :: Value Builtin -> (TensorShape, Value Builtin)
calculateVariableDimensions = \case
  ICons _ (INatLiteral d) ds -> first (d :) $ calculateVariableDimensions ds
  value -> ([], value)

--------------------------------------------------------------------------------
-- Constraint search
--------------------------------------------------------------------------------
-- Definitions

type MonadDomain m =
  ( MonadLogger m,
    MonadReader SearchContext m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  )

-- | Information for the variable whose domain we are trying to find.
data SearchContext = SearchContext
  { targetVariable :: NestedSliceVariable,
    abstractShapeSuffix :: Value Builtin
  }

type MonadTraverseTensor m =
  ( MonadLogger m,
    MonadReader (SearchContext, TensorIndices) m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  )

traverseTensorRows :: (MonadTraverseTensor m) => (a -> m b) -> [a] -> m [b]
traverseTensorRows f rows = do
  let fLocal (i, v) = local (second (i :)) (f v)
  traverse fLocal (zip [0 ..] rows)

currentDimensions :: (MonadTraverseTensor m) => m (Value Builtin)
currentDimensions = do
  (SearchContext {..}, reverseIndices) <- ask
  let remainingShapePrefix = drop (length reverseIndices) (shapeOf targetVariable)
  return $ foldr (\i -> ICons (implicit INatType) (INatLiteral i)) abstractShapeSuffix remainingShapePrefix

childDimensions :: (MonadTraverseTensor m) => m (Value Builtin)
childDimensions = local (second (0 :)) currentDimensions

currentIndices :: (MonadTraverseTensor m) => m TensorIndices
currentIndices = asks (reverse . snd)

runTraverseTensorT :: (MonadDomain m) => ReaderT (SearchContext, TensorIndices) m a -> m a
runTraverseTensorT r = do
  searchCtx <- ask
  runReaderT r (searchCtx, mempty)

-- type ValueBound = Bound (Value Builtin)

type Bound = Inequality (Value Builtin)

andBoundValue :: (MonadTraverseTensor m) => BoundType -> Value Builtin -> Value Builtin -> m (Value Builtin)
andBoundValue boundType v1 v2 = do
  let fn = case boundType of
        Lower -> evalMinRatTensor
        Upper -> evalMaxRatTensor
  dims <- currentDimensions
  fn $
    TensorOp2Args
      { tensorOp2Dims = implicitIrrelevant dims,
        tensorOp2Arg1 = v1,
        tensorOp2Arg2 = v2
      }

andBound :: (MonadTraverseTensor m) => BoundType -> Bound -> Bound -> m Bound
andBound boundType (NormalisedRelation r1 v1) (NormalisedRelation r2 v2) = do
  newValue <- andBoundValue boundType v1 v2
  let newRelation = combineInequalityRelations r1 r2
  return $ NormalisedRelation newRelation newValue

scaleValue :: VArg Builtin -> ScaleConstant (Value Builtin)
scaleValue dims c value = runSilentLogger $ do
  constantTensor <- evalConstTensor $ ConstTensorArgs (implicit IRatType) (IRatLiteral c) (argExpr dims)
  evalMulRatTensor $ TensorOp2Args dims constantTensor value

addValues :: VArg Builtin -> AddConstants (Value Builtin)
addValues dims c1 c2 v1 v2 = runSilentLogger $ do
  let cv1 = scaleValue dims c1 v1
  let cv2 = scaleValue dims c2 v2
  evalAddRatTensor $ TensorOp2Args dims cv1 cv2

prettyBound :: (MonadReader SearchContext m, MonadNameContext m) => Bound -> m Bound
prettyBound (NormalisedRelation r1 v1) = do
  SearchContext{..} <- ask
  _

data TensorBounds = TensorBounds
  { _sliceBound :: Maybe Bound,
    _childConstraints :: Maybe [TensorBounds]
  }

noTensorBounds :: TensorBounds
noTensorBounds = TensorBounds Nothing Nothing

isUnbounded :: TensorBounds -> Bool
isUnbounded (TensorBounds Nothing Nothing) = True
isUnbounded _ = False

singleTensorBound :: Inequality (Value Builtin) -> TensorShape -> TensorIndices -> TensorBounds
singleTensorBound bound shape indices = case (shape, indices) of
  (_, []) -> TensorBounds (Just bound) Nothing
  (d : ds, idx : idxs) -> do
    let childBounds = [if i == idx then singleTensorBound bound ds idxs else noTensorBounds | i <- [0 .. d - 1]]
    TensorBounds Nothing (Just childBounds)
  _ -> developerError "Malformed shape and indices"

andTensorBounds :: (MonadTraverseTensor m) => BoundType -> TensorBounds -> TensorBounds -> m TensorBounds
andTensorBounds boundType (TensorBounds bound1 childBounds1) (TensorBounds bound2 childBounds2) = do
  newBounds <- unionMaybeWithM (andBound boundType) bound1 bound2
  newChildBounds <- unionMaybeWithM (\u v -> traverseTensorRows (uncurry (andTensorBounds boundType)) (zip u v)) childBounds1 childBounds2
  return $ TensorBounds newBounds newChildBounds

prettyTensorBounds :: NamedBoundCtx -> TensorBounds -> Doc a
prettyTensorBounds ctx bounds = vsep $ go bounds
  where
    go :: TensorBounds -> [Doc a]
    go (TensorBounds maybeBound maybeChildBounds) = do
      let boundDoc = maybe [] (\bound -> [prettyFriendly (WithContext bound ctx)]) maybeBound
      let childBoundDocs = maybe [] (concatMap go) maybeChildBounds
      boundDoc <> childBoundDocs

data ConstrainedValue = ConstrainedValue
  { tensorLowerBounds :: TensorBounds,
    tensorUpperBounds :: TensorBounds,
    remainingValue :: Value Builtin
  }

prettyConstrainedValue :: NamedBoundCtx -> ConstrainedValue -> Doc a
prettyConstrainedValue ctx (ConstrainedValue lower upper remainder) = do
  let lowerDoc = prettyTensorBounds ctx lower
  let upperDoc = prettyTensorBounds ctx upper
  let remainderDoc = prettyFriendly (WithContext remainder ctx)
  "Lower bounds:"
    <> lineIndent lowerDoc
    <> line
    <> "Upper bounds:"
    <> lineIndent upperDoc
    <> line
    <> "Remainder:"
    <> lineIndent remainderDoc
    <> line

isUnconstrainedValue :: ConstrainedValue -> Bool
isUnconstrainedValue (ConstrainedValue lower upper _) = isUnbounded lower && isUnbounded upper

unconstrainedValue :: Value Builtin -> ConstrainedValue
unconstrainedValue = ConstrainedValue noTensorBounds noTensorBounds

andConstrainedValue :: (MonadDomain m) => ConstrainedValue -> ConstrainedValue -> m ConstrainedValue
andConstrainedValue (ConstrainedValue l1 u1 v1) (ConstrainedValue l2 u2 v2) = do
  let emptyDims = implicitIrrelevant (INil (implicit INatType))
  newLowerBounds <- runTraverseTensorT $ andTensorBounds Lower l1 l2
  newUpperBounds <- runTraverseTensorT $ andTensorBounds Upper u1 u2
  newRemainder <- evalAnd (TensorOp2Args emptyDims v1 v2)
  return $ ConstrainedValue newLowerBounds newUpperBounds newRemainder

--------------------------------------------------------------------------------
-- Global variables during search

data VariableClassification
  = UnrelatedVariable
  | TargetVariable SliceVariable

classifyVariable :: Lv -> SearchContext -> VariableClassification
classifyVariable lv SearchContext {..} = do
  if lv `isSliceOf` targetVariable
    then TargetVariable (coerce lv)
    else UnrelatedVariable

lookupVarIndices :: (MonadDomain m) => SliceVariable -> m TensorIndices
lookupVarIndices var = do
  SearchContext {..} <- ask
  return $ fst $ findIndicesAndShape targetVariable var

--------------------------------------------------------------------------------
-- Search algorithm

findConstraints :: forall m. (MonadDomain m) => Value Builtin -> m ConstrainedValue
findConstraints expr = logEntryAndExit expr $ case toBoolValue expr of
  -----------------------
  -- Useful base cases --
  -----------------------
  VCompareRatTensor args -> handleComparison args
  -------------------------
  -- Unuseful base cases --
  -------------------------
  VBoolLiteral {} -> return $ unconstrainedValue expr
  VCompareNat {} -> return $ unconstrainedValue expr
  VCompareIndex {} -> return $ unconstrainedValue expr
  -------------------
  -- Blocked cases --
  -------------------
  VReduceAndTensor {} -> tryAndUnblock
  VBoolAt {} -> tryAndUnblock
  ---------------------
  -- Recursive cases --
  ---------------------
  VAnd (TensorOp2Args _ e1 e2) -> do
    c1 <- findConstraints e1
    c2 <- findConstraints e2
    andConstrainedValue c1 c2
  VBoolIf args ->
    findConstraints =<< unfoldIf args
  ----------------
  -- TODO cases --
  ----------------
  -- These two cases need to be altered if we are to handle disjoint domains?
  VOr {} -> return $ unconstrainedValue expr
  VReduceOrTensor {} -> return $ unconstrainedValue expr
  VQuantifyRatTensor {} -> return $ unconstrainedValue expr
  -- Maybe we can do something with these?
  VNot {} -> return $ unconstrainedValue expr
  where
    tryAndUnblock = do
      unblockedValue <- unblockBoolExpr unblockingActions expr
      result <- findConstraints unblockedValue
      return $
        if isUnconstrainedValue result
          then unconstrainedValue expr
          else result

unblockingActions :: (MonadDomain m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = \lv -> return $ VBoundVar lv [],
      unblockNetworkApp = \ident args -> return $ fromRatTensorValue $ VNetworkApp ident args
    }

-- | At the moment we only handle linear constraints, and consider all variables
-- equally. In theory we should be able to handle much more complex domains but
-- issues such as the postivity or negativity of arbitrary expressions come into
-- play when solving the inequalities so leaving at this for now.
handleComparison ::
  (MonadDomain m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m ConstrainedValue
handleComparison (op, args@(TensorOp2Args dims e1 e2))
  | op == Ne = unconstrained
  | otherwise = do
      let evalSub x y = evalSubRatTensor (TensorOp2Args dims x y)
      value@(NormalisedRelation rel combinedValue) <- comparisonToAssertion op evalSub e1 e2
      logDebugM MaxDetail $ prettyFriendlyInCtx value
      errorOrResult <- runExceptT $ compileLinearExpr dims combinedValue
      case errorOrResult of
        Left {} -> unconstrained
        Right linearExpr -> do
          logDebugM MaxDetail $ prettyFriendlyInCtx linearExpr
          case Map.toList (coefficients linearExpr) of
            [(var, _)] -> do
              indices <- lookupVarIndices var
              let (prevCoefficient, rearrangedExpr) = rearrangeExprToSolveForBase (scaleValue dims) var linearExpr
              let bound = constantValue rearrangedExpr
              logDebugM MaxDetail $ prettyFriendlyInCtx bound
              extractVarBounds indices rel prevCoefficient bound
            _ -> unconstrained
  where
    unconstrained = return $ unconstrainedValue $ fromBoolValue $ VCompareRatTensor (op, args)

extractVarBounds ::
  (MonadDomain m) =>
  TensorIndices ->
  Relation ->
  Coefficient ->
  Value Builtin ->
  m ConstrainedValue
extractVarBounds indices rel prevCoefficient value = do
  let strictness = case rel of
        OEq -> These NonStrict NonStrict
        OLt
          | prevCoefficient < -1 -> This Strict
          | otherwise -> That Strict
        OLe
          | prevCoefficient < -1 -> This NonStrict
          | otherwise -> That NonStrict

  shape <- asks (shapeOf . targetVariable)
  let mkBound re = singleTensorBound (NormalisedRelation re value) shape indices

  let bounds = bimap mkBound mkBound strictness
  let (lowerBound, upperBound) = case bounds of
        This lower -> (lower, noTensorBounds)
        That upper -> (noTensorBounds, upper)
        These lower upper -> (lower, upper)

  logDebugM MaxDetail $ do
    ctx <- getNameContext
    return $ case bounds of
      This lower -> "found-bound:" <+> prettyTensorBounds ctx lower
      That upper -> "found-bound:" <+> prettyTensorBounds ctx upper
      These lower upper -> "found-bounds:" <+> prettyTensorBounds ctx lower <> "," <+> prettyTensorBounds ctx upper

  return $
    ConstrainedValue
      { remainingValue = IBoolLiteral True,
        tensorLowerBounds = lowerBound,
        tensorUpperBounds = upperBound
      }

compileLinearExpr ::
  forall m.
  (MonadDomain m, MonadError (Value Builtin) m) =>
  VArg Builtin ->
  Value Builtin ->
  m (LinearExpr SliceVariable (Value Builtin))
compileLinearExpr dims expr = case toRatTensorValue expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensorLiteral {} -> return $ constantExpr expr
  VRatConstTensor {} -> return $ constantExpr expr
  VRatTensorVar var -> do
    classification <- asks (classifyVariable var)
    case classification of
      TargetVariable sliceVar -> do
        zeroValue <-
          evalConstTensor $
            ConstTensorArgs
              { constType = implicit IRatType,
                constValue = IRatLiteral 0,
                constDims = argExpr dims
              }
        return $ singletonVarExpr zeroValue sliceVar
      _ -> unlinearisable
  ---------------------
  -- Inductive cases --
  ---------------------
  VNegRatTensor (TensorOp1Args _ e) -> do scaleExprBase (scaleValue dims) (-1) <$> compileLinearExpr dims e
  VAddRatTensor (TensorOp2Args _ e1 e2) -> addExprsBase (addValues dims) 1 1 <$> compileLinearExpr dims e1 <*> compileLinearExpr dims e2
  VSubRatTensor (TensorOp2Args _ e1 e2) -> addExprsBase (addValues dims) 1 (-1) <$> compileLinearExpr dims e1 <*> compileLinearExpr dims e2
  VMulRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
  VDivRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
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
    unlinearisable :: m (LinearExpr SliceVariable (Value Builtin))
    unlinearisable = throwError expr

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
-}
--------------------------------------------------------------------------------
-- Domain

-- | NOTE: this function discards the strictness information
extractSingleBound ::
  forall m.
  (MonadTraverseTensor m) =>
  BoundType ->
  TensorBounds ->
  m (Either (NonEmpty TensorIndices) (Value Builtin))
extractSingleBound boundType (TensorBounds maybeBound maybeChildBounds) =
  case maybeChildBounds of
    Nothing -> case maybeBound of
      Just bound -> return $ Right $ expression bound
      Nothing -> do
        indices <- currentIndices
        return $ Left [indices]
    Just childTensorBounds -> do
      childDims <- childDimensions
      childErrorOrBounds <- traverseTensorRows (extractSingleBound boundType) childTensorBounds
      let (missingChildIndices, childBounds) = partitionEithers childErrorOrBounds
      case maybeBound of
        Nothing -> case missingChildIndices of
          i : is -> return $ Left (NonEmpty.appendList i (concatMap NonEmpty.toList is))
          [] -> Right <$> stack childDims childBounds
        Just bound -> case missingChildIndices of
          _ : _ -> do
            nameCtx <- getNameContext
            boundElements <- etaReduceTensor nameCtx IRatType (length childTensorBounds) childDims (expression bound)
            combinedElements <- zipWithM (\bi -> either (return . const bi) (andBoundValue boundType bi)) boundElements childErrorOrBounds
            Right <$> stack childDims combinedElements
          [] -> do
            childBound <- stack childDims childBounds
            Right <$> andBoundValue boundType (expression bound) childBound
  where
    stack :: Value Builtin -> [Value Builtin] -> m (Value Builtin)
    stack remainingDims elements =
      evalStackTensor $
        StackTensorArgs
          { stackType = implicit IRatType,
            stackFirstDim = INatLiteral (length elements),
            stackRemainingDims = implicitIrrelevant remainingDims,
            stackElements = elements
          }

logEntryAndExit :: MonadDomain m => Value Builtin -> m ConstrainedValue -> m ConstrainedValue
logEntryAndExit start action = do
  ctx <- getNameContext
  logDebug MaxDetail $ "search-enter:" <+> prettyFriendly (WithContext start ctx)
  incrCallDepth
  result <- action
  decrCallDepth
  logDebug MaxDetail $ "search-exit:" <+> prettyFriendly (WithContext (remainingValue result) ctx)
  return result
  -}
