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
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Map qualified as Map
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.EliminateExists (eliminateQuantifiedVariable)
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Rational.LinearExpr (LinearityError (..), compileLinearRelation)
import Vehicle.Compile.Resource (NetworkTensorType (..), NetworkType (..))
import Vehicle.Compile.Unblock (UnblockingActions (..))
import Vehicle.Compile.Unblock qualified as Unblocking
import Vehicle.Compile.Variable (createUserVar)
import Vehicle.Data.Assertion
import Vehicle.Data.Builtin.Interface.Normalise (evalAtTensor, unoptimisedEvalReduceAndTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.LinearExpr (addExprs)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.Variable.Bound.Context.Name (getNameContext, prettyFriendlyInCtx)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (getFreeEnv)
import Vehicle.Verify.Core (NetworkContextInfo (..))
import Vehicle.Verify.QueryFormat (QueryFormat (..), supportsStrictInequalities)
import Prelude hiding (Applicative (..))

eliminateExists ::
  (MonadQueryStructure m) =>
  VBinder Builtin ->
  Closure Builtin ->
  m (MaybeTrivial Partitions)
eliminateExists binder (Closure env body) = do
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
    (userVar, newGlobalCtx) <- addUserVarToGlobalContext userVarName userVarShape globalCtx
    put newGlobalCtx

    -- Normalise the expression
    let newEnv = extendEnvWithBound (toLv userVar) binder env
    normExpr <- normaliseInEnv (Just userVarName : namedCtx) newEnv body

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
    VQuantifyRatTensor Forall _ _ _ -> throwError catchableUnsupportedAlternatingQuantifiersError
    ---------------------
    -- Recursive cases --
    ---------------------
    VNot arg -> do
      ctx <- getNameContext
      compileBoolExpr =<< lowerNot ctx unblock arg
    VBoolIf args -> compileBoolExpr =<< unfoldIf args
    VAnd (TensorOp2Args _dims x y) -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VOr (TensorOp2Args _dims x y) -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VQuantifyRatTensor Exists _ binder closure -> eliminateExists binder closure
    VCompareNat {} -> unblockAndRec expr
    VCompareIndex {} -> unblockAndRec expr
    VReduceAndTensor {} -> unblockAndRec expr
    VReduceOrTensor {} -> unblockAndRec expr
    VBoolAt {} -> unblockAndRec expr
  where
    unblock = Unblocking.unblockBoolExpr unblockingActions
    unblockAndRec e = compileBoolExpr =<< unblock e

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
      recurseOrResult <- logCompilerSection2 MaxDetail "assertion compilation" $ do
        maybePurifiedValue <- Unblocking.tryPurifyAssertion unblockingActions op args
        case maybePurifiedValue of
          Left purifiedValue -> return $ Left purifiedValue
          Right purifiedArgs -> compilePurifiedAssertion op purifiedArgs

      case recurseOrResult of
        Left value -> compileBoolExpr value
        Right assertion -> return $ mkTrivialPartition assertion

compilePurifiedAssertion ::
  (MonadQuantifierBody m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (Either (Value Builtin) LinearAssertion)
compilePurifiedAssertion op args@(TensorOp2Args dims xs ys) = do
  let shape = case getDims (argExpr dims) of
        Nothing -> developerError $ "Non-concrete dimensions found" <+> prettyVerbose dims
        Just concreteShape -> concreteShape

  maybeLinearRel <- compileLinearRelation findVariableFromLevel shape xs ys
  case maybeLinearRel of
    Right (e1, e2) -> do
      let subExprs u v = return $ addExprs 1 (-1) u v
      Right <$> comparisonToAssertion op subExprs e1 e2
    Left NonLinearity ->
      throwError catchableUnsupportedNonLinearConstraint
    Left (UnexpectedExpr e) ->
      developerError ("unexpected expression" <+> prettyVerbose e)
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

unblockingActions :: (MonadQuantifierBody m) => UnblockingActions m
unblockingActions = UnblockingActions unblockQuantifiedBoundVar unblockNetworkApplication

unblockQuantifiedBoundVar ::
  (MonadQuantifierBody m) =>
  Lv ->
  m (Value Builtin)
unblockQuantifiedBoundVar lv = do
  maybeChildVariablesExpr <- gets $ flip lookupChildVariablesExpr (SliceVariable lv)
  case maybeChildVariablesExpr of
    Just childVariablesExpr -> return childVariablesExpr
    Nothing -> return $ VBoundVar lv []

unblockNetworkApplication ::
  (MonadQuantifierBody m) =>
  Identifier ->
  NetworkAppArgs (Value Builtin) ->
  m (Value Builtin)
unblockNetworkApplication ident (NetworkAppArgs arg) = do
  globalCtx <- get
  let name = nameOf ident
  networkContext <- asks networkCtx
  networkInfo <- case Map.lookup name networkContext of
    Nothing -> compilerDeveloperError $ "Expecting" <+> quotePretty name <+> "to be a @network"
    Just info -> return info

  (inputVarExpr, outputVarExpr, newGlobalCtx) <- addNetworkApplicationToGlobalCtx name networkInfo globalCtx arg
  let inputDims = dimensions (inputTensor (networkType networkInfo))
  let inputDimsExpr = implicitIrrelevant $ mkDims inputDims
  let inputEquality = fromBoolValue $ VCompareRatTensor (Eq, TensorOp2Args inputDimsExpr inputVarExpr arg)
  put newGlobalCtx
  newNameCtx <- getNameContext
  logDebug MaxDetail $ "note-input-equality" <+> prettyFriendly (WithContext inputEquality newNameCtx)
  tell [inputEquality]
  logDebug MaxDetail $ "replace-expr" <+> prettyFriendly (WithContext outputVarExpr newNameCtx)
  return outputVarExpr

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
  case argExpr dims of
    ICons _ d@(INatLiteral n) ds -> do
      freeEnv <- getFreeEnv
      -- TODO switch to use `etaReduceTensor`?
      nameCtx <- getNameContext
      let tElem = implicit $ fromTypeValue VRatType
      let d0Arg = implicitIrrelevant (mkDims [])
      let mkAt vs i = evalAtTensor nameCtx (evalApp freeEnv) (eval freeEnv) (AtTensorArgs tElem (implicitIrrelevant d) (implicitIrrelevant ds) vs (IIndexLiteral i))
      let mkStackElement i = do
            xsi <- mkAt xs i
            ysi <- mkAt ys i
            evalCompareRatTensor op (TensorOp2Args (implicitIrrelevant ds) xsi ysi)
      stackElements <- traverse mkStackElement [0 .. (n - 1)] :: m [Value Builtin]
      let stackExpr = fromBoolTensorValue $ VBoolStackTensor (StackTensorArgs tElem d d0Arg stackElements)
      unoptimisedEvalReduceAndTensor (TensorOp2Args (implicitIrrelevant (mkDims [n])) (IBoolLiteral True) stackExpr)
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
