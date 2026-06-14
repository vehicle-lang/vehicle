{- HLINT ignore "Use fewer imports" -}
module Vehicle.Backend.Solver.UserVariableElimination
  ( eliminateExists,
    eliminateExistless,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.EliminateExists (eliminateQuantifiedVariable)
import Vehicle.Backend.Solver.UserVariableElimination.LinearExpr (LinearityError (..), compileLinearAssertion)
import Vehicle.Backend.Solver.UserVariableElimination.PurifyAssertion (purifyAssertion)
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Resource
import Vehicle.Compile.Unblock (OperationUnblockingFunction, TypeUnblockingFunction, UnblockingActions (..))
import Vehicle.Compile.Unblock qualified as Unblocking
import Vehicle.Compile.Variable (createUserVar)
import Vehicle.Data.Builtin.Interface.Normalise (evalAtTensor, unoptimisedEvalReduceAndTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Builtin.Standard.Scoping (constructFromTensorFreeVar, constructToTensorFreeVar)
import Vehicle.Data.Code.BooleanExpr (elimIfTree)
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (getNameContext, prettyFriendlyInCtx)
import Vehicle.Data.Variable.Bound.Context.Tensor (replaceTensorVariableWithStackedChildren)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat (QueryFormat (..), supportsStrictInequalities)
import Prelude hiding (Applicative (..))

eliminateExists ::
  (MonadQueryStructure m) =>
  QuantifyRatTensorArgs (Value Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
eliminateExists (QuantifyRatTensorArgs _ binder (Closure env body)) = do
  let varName = getBinderName binder
  let subpassDoc = "elimination of existential quantifier over" <+> quotePretty varName
  logCompilerSection2 MidDetail subpassDoc $ do
    -- Get the shape and name of the quantified variable
    namedCtx <- getNameContext
    propertyProv <- asks propertyProvenance
    (userVarName, userVarShapeValue) <- createUserVar propertyProv namedCtx binder
    userVarShape <- case getDims userVarShapeValue of
      Just shape -> return shape
      _ -> throwError $ VariableSizeTensorQuantification propertyProv namedCtx binder userVarShapeValue

    -- Update the global context
    globalCtx <- get
    (userVar, newGlobalCtx) <- addUserVarToGlobalContext binder userVarShape globalCtx
    put newGlobalCtx

    -- Normalise the expression
    let newEnv = extendEnvWithBound (toLv userVar) binder env
    normExpr <- eval (Just userVarName : namedCtx) newEnv body

    -- Recursively compile the expression.
    (partitions, networkInputEqualities) <-
      logCompilerSection2 MidDetail "reduction of body to assertion tree" $ runWriterT (compileBoolExpr normExpr)

    -- Prepend network equalities to the tree (prepending is important for
    -- performance as the search for constraints will find them first.)
    networkEqPartitions <-
      logCompilerSection2 MidDetail "reduction of network equalities to assertion tree" $ networkEqualitiesToPartition networkInputEqualities

    let finalPartitions = andTrivial andPartitions partitions networkEqPartitions

    -- Solve for the user variable
    eliminateQuantifiedVariable finalPartitions userVar

eliminateExistless ::
  (MonadQueryStructure m) =>
  Value Builtin ->
  m (MaybeTrivial Partitions)
eliminateExistless value = do
  (maybePartitions, equalities) <- runWriterT $ compileBoolExpr value
  networkEqPartitions <- networkEqualitiesToPartition equalities
  return $ andTrivial andPartitions maybePartitions networkEqPartitions

-- | Attempts to compile an arbitrary expression of type `Bool` down to a tree
-- of assertions implicitly existentially quantified by a set of network
-- input/output variables.
compileBoolExpr ::
  (MonadQueryStructure m, MonadWriter [Value Builtin] m) =>
  Value Builtin ->
  m (MaybeTrivial Partitions)
compileBoolExpr expr = do
  showEntry expr
  showExit =<< case toBoolValue expr of
    ----------------
    -- Base cases --
    ----------------
    VBoolLiteral b -> return $ Trivial b
    VCompareRatTensor (op, args) -> purifyAndCompileAssertion op args
    VQuantifyRatTensor (Forall, _) -> throwError catchableUnsupportedAlternatingQuantifiersError
    VQuantifyRecord (Forall, _) -> throwError catchableUnsupportedAlternatingQuantifiersError
    VAnd (TensorOp2Args _dims x y) -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VOr (TensorOp2Args _dims x y) -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VQuantifyRatTensor (Exists, args) -> eliminateExists args
    -- TODO: RECORD SUPPORT
    VQuantifyRecord (Exists, _args) -> compilerDeveloperError "Non top-level record quantifiers are not supported yet"
    ---------------------
    -- Recursive cases --
    ---------------------
    VNot arg -> compileBoolExpr =<< lowerNot arg
    VBoolIf args -> compileBoolExpr =<< unfoldIf args
    VCompareNat {} -> unblockAndRec expr
    VCompareIndex {} -> unblockAndRec expr
    VReduceAndTensor {} -> unblockAndRec expr
    VReduceOrTensor {} -> unblockAndRec expr
    VBoolAt {} -> unblockAndRec expr
  where
    unblockAndRec e = compileBoolExpr =<< Unblocking.unblockBoolExpr unblockingActions e

purifyAndCompileAssertion ::
  (MonadQuantifierBody m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (MaybeTrivial Partitions)
purifyAndCompileAssertion op args
  | op == Ne =
      -- We can't handle negative equalities so just eliminate it
      compileBoolExpr =<< eliminateNotEqualRatTensor args
  | otherwise = do
      logCompilerSection2 MaxDetail "assertion compilation" $ do
        maybePurifiedValue <- purifyAssertion unblockingActions op args
        elimIfTree elimBranch elimLeaf maybePurifiedValue
  where
    elimLeaf :: (MonadQuantifierBody m) => (ComparisonOp, TensorOp2Args (Value Builtin)) -> m (MaybeTrivial Partitions)
    elimLeaf assertion = do
      resultOrError <- compilePurifiedAssertion assertion
      case resultOrError of
        Left recExpr -> compileBoolExpr recExpr
        Right linearAssertion -> return $ mkTrivialPartition linearAssertion

    elimBranch :: (MonadQuantifierBody m) => Value Builtin -> MaybeTrivial Partitions -> MaybeTrivial Partitions -> m (MaybeTrivial Partitions)
    elimBranch c x y = do
      c' <- compileBoolExpr c
      notC' <- compileBoolExpr (fromBoolValue $ VNot $ TensorOp1Args IDimNil c)
      let cAndx = andTrivial andPartitions c' x
      let notCAndy = andTrivial andPartitions notC' y
      return $ orTrivial orPartitions cAndx notCAndy

compilePurifiedAssertion ::
  (MonadQuantifierBody m) =>
  (ComparisonOp, TensorOp2Args (Value Builtin)) ->
  m (Either (Value Builtin) LinearAssertion)
compilePurifiedAssertion (op, args@(TensorOp2Args dims xs ys)) = do
  let shape = case getDims dims of
        Nothing -> developerError $ "Non-concrete dimensions found" <+> prettyVerbose dims
        Just concreteShape -> concreteShape

  maybeLinearAssertion <- compileLinearAssertion findVariableFromLevel op shape xs ys
  case maybeLinearAssertion of
    Right assertion -> do
      return $ Right assertion
    Left NonLinearity ->
      throwError catchableUnsupportedNonLinearConstraint
    Left (UnexpectedExpr e) ->
      developerError ("unexpected expression" <+> prettyVerbose e)
    Left (TrivialExpr b) ->
      return $ Left $ IBoolLiteral b
    Left (UnreducedExpr e) -> do
      logDebugM MaxDetail $ do
        exprDoc <- prettyFriendlyInCtx e
        return $ "non-variable-terms:" <+> exprDoc
      elementComparisonValue <- eliminateTensorAssertion op args
      logDebugM MaxDetail $ do
        newValueDoc <- prettyFriendlyInCtx elementComparisonValue
        return $ "converting-to-element-assertions:" <+> newValueDoc
      return $ Left elementComparisonValue

findVariableFromLevel :: (MonadQueryStructure m) => Lv -> m SliceVariable
findVariableFromLevel = return . SliceVariable

--------------------------------------------------------------------------------
-- Unblocking

type MonadQuantifierBody m =
  ( MonadQueryStructure m,
    MonadWriter [Value Builtin] m
  )

unblockingActions ::
  (MonadPropertyStructure m, MonadState GlobalCtx m, MonadWriter [Value Builtin] m) =>
  UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = unblockQuantifiedBoundVar,
      unblockRecordBoundVar = unblockQuantifiedBoundVar,
      unblockNetworkApp = unblockNetworkApplication,
      unblockDatasetOrParameter = unexpectedExprError "solver compilation" "dataset or parameter"
    }

unblockQuantifiedBoundVar ::
  (MonadPropertyStructure m) =>
  Lv ->
  m (Value Builtin)
unblockQuantifiedBoundVar lv =
  replaceTensorVariableWithStackedChildren (SliceVariable lv)

unblockNetworkApplication ::
  (MonadPropertyStructure m, MonadState GlobalCtx m, MonadWriter [Value Builtin] m) =>
  TypeUnblockingFunction (Value Builtin) m ->
  TypeUnblockingFunction (Value Builtin) m ->
  Identifier ->
  OperationUnblockingFunction NetworkAppArgs (Value Builtin) m
unblockNetworkApplication unblockFnTensor unblockFnRecord ident (NetworkAppArgs arg) = do
  let name = nameOf ident
  networkInfo <- asks (lookupNetworkInfo name . networkCtx)
  let typ = networkType networkInfo

  -- The low-level network representation works over tensors
  -- Create two tensors representing the network input and output
  (inputVarExpr, outputVarExpr) <- addNetworkApplicationToGlobalCtx name networkInfo arg
  ctx <- getNameContext

  -- If our network outputs a tensorisable, convert our output expression to a record
  transformedOutputVarExpr <- case networkOutputType typ of
    RecordIOType (NetworkRecordType _ recordTyp _ _) -> do
      fromTensorFn <- eval ctx emptyBoundEnv (constructFromTensorFreeVar recordTyp mempty)
      evalApp ctx fromTensorFn [explicit outputVarExpr]
    _ -> return outputVarExpr

  -- Create our input equality in terms of tensors (as record equality just converts to tensor equality anyway)
  -- If our network input is a tensorisable, i.e. arg is tensorisable, convert it to a tensor
  transformedArg <- case networkInputType typ of
    RecordIOType (NetworkRecordType _ recordTyp _ _) -> do
      toTensorFn <- eval ctx emptyBoundEnv (constructToTensorFreeVar recordTyp mempty)
      evalApp ctx toTensorFn [explicit arg]
    _ -> return arg

  let inputEquality =
        fromBoolValue $
          VCompareRatTensor
            ( Eq,
              TensorOp2Args
                { tensorOp2Dims = mkDims (inputShape networkInfo),
                  tensorOp2Arg1 = inputVarExpr,
                  tensorOp2Arg2 = transformedArg
                }
            )

  tell [inputEquality]

  logDebugM MaxDetail $ do
    inputEqualityDoc <- prettyFriendlyInCtx inputEquality
    replacementExprDoc <- prettyFriendlyInCtx transformedOutputVarExpr
    return $
      "note-input-equality" <+> inputEqualityDoc
        <> line
        <> "replace-expr" <+> replacementExprDoc

  case networkOutputType typ of
    -- Unblock depending on the type of the output expression from our network
    RecordIOType (NetworkRecordType {}) -> unblockFnRecord transformedOutputVarExpr
    TensorIOType (NetworkTensorType {}) -> unblockFnTensor transformedOutputVarExpr

--------------------------------------------------------------------------------
-- Elimination operations

eliminateNotEqualRatTensor ::
  (MonadQueryStructure m) =>
  TensorOp2Args (Value Builtin) ->
  m (Value Builtin)
eliminateNotEqualRatTensor args@(TensorOp2Args dims _ _) = do
  PropertyMetaData {..} <- ask
  if supportsStrictInequalities queryFormat
    then throwError $ UnsupportedInequality (queryFormatID queryFormat) propertyProvenance
    else do
      let leq = fromBoolValue $ VCompareRatTensor (Le, args)
      let geq = fromBoolValue $ VCompareRatTensor (Ge, args)
      return $ fromBoolValue $ VOr (TensorOp2Args dims leq geq)

eliminateTensorAssertion ::
  forall m.
  (MonadQueryStructure m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (Value Builtin)
eliminateTensorAssertion op (TensorOp2Args dims xs ys) =
  case dims of
    IDimNil -> do
      -- For scalar comparisons, directly apply the comparison
      evalCompareRatTensor op (TensorOp2Args IDimNil xs ys)
    IDimCons d@(INatLiteral n) ds -> do
      -- TODO switch to use `etaReduceTensor`?
      nameCtx <- getNameContext
      let tElem = fromTypeValue VRatType
      let d0Arg = mkDims []
      let mkAt vs i = evalAtTensor nameCtx evalApp eval (AtTensorArgs tElem d ds vs (IIndexLiteral i d))
      let mkStackElement i = do
            xsi <- mkAt xs i
            ysi <- mkAt ys i
            evalCompareRatTensor op (TensorOp2Args ds xsi ysi)
      stackElements <- traverse mkStackElement [0 .. (n - 1)] :: m [Value Builtin]
      let stackExpr = fromBoolTensorValue $ VBoolStackTensor (StackTensorArgs tElem d d0Arg stackElements)
      result <- unoptimisedEvalReduceAndTensor (TensorReductionArgs (mkDims [n]) stackExpr)
      return result
    _ -> compilerDeveloperError ("unexpected dimensions" <+> prettyVerbose dims)

networkEqualitiesToPartition ::
  (MonadQueryStructure m) =>
  [Value Builtin] ->
  m (MaybeTrivial Partitions)
networkEqualitiesToPartition networkEqualities = do
  logDebugM MaxDetail $ do
    networkEqDocs <- traverse prettyFriendlyInCtx networkEqualities
    return $ vsep networkEqDocs <> line

  results <- forM networkEqualities $ \equality -> do
    (partitions, newNetworkEqualities) <- runWriterT (compileBoolExpr equality)
    if null newNetworkEqualities
      then return partitions
      else andTrivial andPartitions partitions <$> networkEqualitiesToPartition newNetworkEqualities

  return $ foldr (andTrivial andPartitions) (Trivial True) results

--------------------------------------------------------------------------------
-- Vector operations preservation

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedAlternatingQuantifiersError :: CompileError
catchableUnsupportedAlternatingQuantifiersError =
  UnsupportedAlternatingQuantifiers x x x
  where
    x = developerError "Evaluating temporary quantifier error"

-- | Constructs a temporary error with no real fields. This should be recaught
-- and populated higher up the query compilation process.
catchableUnsupportedNonLinearConstraint :: CompileError
catchableUnsupportedNonLinearConstraint =
  UnsupportedNonLinearConstraint x x x
  where
    x = developerError "Evaluating temporary quantifier error"

showEntry :: (MonadQueryStructure m) => Value Builtin -> m ()
showEntry v = do
  logDebugM MaxDetail $ do
    vDoc <- prettyFriendlyInCtx v
    return $ "elim-enter" <+> vDoc
  incrCallDepth

showExit ::
  (MonadQueryStructure m) =>
  MaybeTrivial Partitions ->
  m (MaybeTrivial Partitions)
showExit v = do
  decrCallDepth
  logDebugM MaxDetail $ do
    -- vDoc <- prettyExternalInCtx v
    return $ "elim-exit" <+> pretty (partitionsSize v) -- vDoc
  return v
