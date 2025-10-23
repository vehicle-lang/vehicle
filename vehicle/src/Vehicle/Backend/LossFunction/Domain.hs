{-# OPTIONS_GHC -Wno-orphans #-}

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
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Coerce (coerce)
import Vehicle.Compile.Constants.Value
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Assertion (Assertion, comparisonToAssertion)
import Vehicle.Data.Bound
import Vehicle.Data.Builtin.Interface.Normalise (evalAnd)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor.Traversal
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Bound.Tensor
import Vehicle.Data.Variable.Free.Context (MonadFreeContext)

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

{- propertyProv binder lv value = do
  let name = getBinderName binder
  let targetVar = coerce lv

  logCompilerSection2 MidDetail ("extracting domain for" <+> quotePretty name) $ do
    let partialShape = case toTypeValue $ typeOf binder of
          VRatTensorType dims -> extractPartialShape dims
          _ -> developerError "Unexpected quantifier type"

    let searchCtx =
          SearchContext
            { targetVariable = targetVar,
              targetVariableShape = partialShape
            }

    -- Make names for all the slices that we actually know about.
    let newNames = variableNamesForAllSlices name (knownPrefix partialShape)
    addNamesToContext (reverse newNames) $ do
      logDebugM MaxDetail $ do
        nameCtx <- getNameContext
        doc <- prettyFriendlyInCtx value
        return $ pretty nameCtx <> line <> "Body:" <+> doc

      -- Search for constraints
      ConstrainedValue tensorBounds remainder <- runReaderT (findConstraints value) searchCtx
      logDebugM MidDetail $ do
        nameCtx <- getNameContext
        let boundsDoc = prettyFriendly (WithContext (BoundedValue targetVar tensorBounds) nameCtx)
        let remainderDoc = prettyFriendly (WithContext remainder nameCtx)
        return $
          "Found bounds:"
            <> lineIndent boundsDoc
            <> line
            <> "Remainder:"
            <> lineIndent remainderDoc
            <> line

      -- Extract the domain
      errorOrBounds <- flattenTensorBounds tensorBounds
      case errorOrBounds of
        Left err -> throwError $ NoQuantifierDomainFound propertyProv binder err
        Right (lowerBound, upperBound) -> do
          let domain =
                Domain
                  { lowerBound = tensorValue lowerBound,
                    upperBound = tensorValue upperBound
                  }
          return (domain, remainder)

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
  { targetVariable :: UserTensorVariable,
    targetVariableShape :: PartiallyKnownTensorShape
  }

nestedTargetVariable :: SearchContext -> NestedSliceVariable
nestedTargetVariable SearchContext {..} = mkNestedSliceVariable (knownPrefix targetVariableShape) targetVariable

data ConstrainedValue = ConstrainedValue
  { tensorBounds :: TensorBounds TensorValue,
    remainingValue :: Value Builtin
  }

andConstrainedValue :: (MonadDomain m) => ConstrainedValue -> ConstrainedValue -> m ConstrainedValue
andConstrainedValue (ConstrainedValue bounds1 v1) (ConstrainedValue bounds2 v2) = do
  let emptyDims = implicitIrrelevant (INil (implicit INatType))
  newBounds <- andTensorBounds bounds1 bounds2
  newRemainder <- evalAnd (TensorOp2Args emptyDims v1 v2)
  return $ ConstrainedValue newBounds newRemainder

unconstrainedValue :: (MonadDomain m) => Value Builtin -> m ConstrainedValue
unconstrainedValue value = do
  SearchContext {..} <- ask
  let bounds = emptyTensorBounds targetVariable targetVariableShape
  return $ ConstrainedValue bounds value

--------------------------------------------------------------------------------
-- Global variables during search

data VariableClassification
  = UnrelatedVariable
  | TargetVariable SliceVariable

classifyVariable :: Lv -> SearchContext -> VariableClassification
classifyVariable lv ctx = do
  let maybeSliceVar = lv `isSliceOf` nestedTargetVariable ctx
  maybe UnrelatedVariable TargetVariable maybeSliceVar

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
  VBoolLiteral {} -> unconstrainedValue expr
  VCompareNat {} -> unconstrainedValue expr
  VCompareIndex {} -> unconstrainedValue expr
  ---------------------
  -- Recursive cases --
  ---------------------
  VAnd (TensorOp2Args _ e1 e2) -> do
    c1 <- findConstraints e1
    c2 <- findConstraints e2
    andConstrainedValue c1 c2
  VBoolIf args ->
    findConstraints =<< unfoldIf args
  -------------------
  -- Blocked cases --
  -------------------
  VReduceAndTensor {} -> tryAndUnblock
  VBoolAt {} -> tryAndUnblock
  ----------------
  -- TODO cases --
  ----------------
  -- These two cases need to be altered if we are to handle disjoint domains?
  VOr {} -> unconstrainedValue expr
  VReduceOrTensor {} -> unconstrainedValue expr
  VQuantifyRatTensor {} -> unconstrainedValue expr
  -- Maybe we can do something with these?
  VNot {} -> unconstrainedValue expr
  where
    tryAndUnblock = do
      unblockedValue <- unblockBoolExpr unblockingActions expr
      result <- findConstraints unblockedValue
      if isEmptyTensorBounds (tensorBounds result)
        then unconstrainedValue expr
        else return result

unblockingActions :: (MonadDomain m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = \lv -> return $ VBoundVar lv [],
      unblockNetworkApp = \ident args -> return $ fromRatTensorValue $ VNetworkApp ident args
    }

handleComparison ::
  (MonadDomain m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m ConstrainedValue
handleComparison (op, args)
  | op == Ne = unconstrainedValue originalValue
  | otherwise = do
      valueOrAssertion <- compileAssertion (op, args)
      case valueOrAssertion of
        Left {} -> unconstrainedValue originalValue
        Right (Left trivial) -> unconstrainedValue $ IBoolLiteral trivial
        Right (Right assertion) -> do
          lookupFn <- lookupVariableDetails
          let maybeBounds = tryToConvertToTensorBounds lookupFn assertion
          case maybeBounds of
            Nothing -> unconstrainedValue originalValue
            Just (_var, bounds) -> return $ ConstrainedValue bounds (IBoolLiteral True)
  where
    originalValue = fromBoolValue $ VCompareRatTensor (op, args)

lookupVariableDetails ::
  (MonadDomain m) =>
  m (SliceVariable -> Maybe VariableInfo)
lookupVariableDetails = do
  ctx@SearchContext {..} <- ask
  let targetVar = nestedTargetVariable ctx
  return $ \var -> Just $ do
    VariableInfo
      { parentVariable = toTensorVar targetVariable,
        parentShape = targetVariableShape,
        indices = findIndices targetVar var
      }

compileAssertion ::
  forall m.
  (MonadDomain m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m (Either (Value Builtin) (Either Bool (Assertion (LinearExpr SliceVariable TensorValue))))
compileAssertion (op, TensorOp2Args dims e1 e2) = do
  runExceptT $ do
    linX <- compileLinearExpr (argExpr dims) e1
    linY <- compileLinearExpr (argExpr dims) e2
    comparisonToAssertion op linX linY

compileLinearExpr ::
  forall m.
  (MonadDomain m, MonadError (Value Builtin) m) =>
  VDims Builtin ->
  Value Builtin ->
  m (LinearExpr SliceVariable TensorValue)
compileLinearExpr dims expr = case toRatTensorValue expr of
  ----------------
  -- Base cases --
  ----------------
  VRatTensorLiteral {} -> return $ constantExpr $ TensorValue dims expr
  VRatConstTensor {} -> return $ constantExpr $ TensorValue dims expr
  VRatTensorVar var -> do
    maybeExpr <- compileRatTensorVar dims var
    maybe unlinearisable return maybeExpr
  ---------------------
  -- Inductive cases --
  ---------------------
  VNegRatTensor (TensorOp1Args _ e) -> do
    e' <- compileLinearExpr dims e
    return $ scaleExpr (-1) e'
  VAddRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    return $ addExprsUnsafe 1 1 e1' e2'
  VSubRatTensor (TensorOp2Args _ e1 e2) -> do
    e1' <- compileLinearExpr dims e1
    e2' <- compileLinearExpr dims e2
    return $ addExprsUnsafe 1 (-1) e1' e2'
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
  VMulRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
  VDivRatTensor (TensorOp2Args _ _e1 _e2) -> unlinearisable
  where
    unlinearisable :: m (LinearExpr SliceVariable TensorValue)
    unlinearisable = throwError expr

compileRatTensorVar ::
  (MonadDomain m) =>
  Value Builtin ->
  Lv ->
  m (Maybe (LinearExpr SliceVariable TensorValue))
compileRatTensorVar dims var = do
  classification <- asks (classifyVariable var)
  case classification of
    TargetVariable sliceVar -> do
      let zeroValue = constantDimensionedValue dims 0
      return $ Just $ singletonVarExpr zeroValue sliceVar
    _ -> return Nothing

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

logEntryAndExit :: (MonadDomain m) => Value Builtin -> m ConstrainedValue -> m ConstrainedValue
logEntryAndExit start action = do
  ctx <- getNameContext
  logDebug MaxDetail $ "search-enter:" <+> prettyFriendly (WithContext start ctx)
  incrCallDepth
  result <- action
  decrCallDepth
  logDebug MaxDetail $ "search-exit:" <+> prettyFriendly (WithContext (remainingValue result) ctx)
  return result
-}
