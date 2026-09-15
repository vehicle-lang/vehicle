{- HLINT ignore "Use fewer imports" -}
module Vehicle.Backend.Solver.UserVariableElimination
  ( eliminateExists,
    eliminateExistsRecord,
    eliminateExistless,
    compileBoolExpr,
  )
where

-- Needed as Applicative is exported by Prelude in GHC 9.6 and above.
import Control.Applicative (Applicative (..))
import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer.Strict (MonadWriter (..), WriterT (..))
import Data.Map qualified as Map
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Backend.Solver.UserVariableElimination.EliminateExists (eliminateQuantifiedVariable)
import Vehicle.Backend.Solver.UserVariableElimination.LinearExpr (LinearityError (..), compileLinearAssertion)
import Vehicle.Backend.Solver.UserVariableElimination.PurifyAssertion (purifyAssertion)
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot)
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.RewriteRules (forceAndRewriteDims, forceAndRewriteTensor)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Resource
import Vehicle.Compile.Scope.Records (constructFromTensorFreeVar, constructToTensorFreeVar)
import Vehicle.Compile.Unblock (OperationUnblockingFunction, TypeUnblockingFunction, UnblockingActions (..))
import Vehicle.Compile.Unblock qualified as Unblocking
import Vehicle.Compile.Variable (createUserVar)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.BooleanExpr (IfTree, elimIfTree)
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (getFreshTensorBinderName, getNameContext, prettyFriendlyInCtx)
import Vehicle.Data.Variable.Bound.Context.Tensor (replaceTensorVariableWithStackedChildren)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Free.Context (getRecordFieldNames, getRecordFields, getRecordProvenance)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat (QueryFormat (..), supportsStrictInequalities)
import Vehicle.Verify.Specification (CompilationStep (..))
import Prelude hiding (Applicative (..))

eliminateExistsRecord ::
  (MonadQueryStructure m) =>
  QuantifyRecordArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
eliminateExistsRecord args = do
  (wrappedBinderArgs, step) <- wrapQuantifyRecord args
  maybePartitions <- eliminateExists wrappedBinderArgs

  return $ case maybePartitions of
    Trivial b -> Trivial b
    NonTrivial (Partitions m) ->
      NonTrivial (Partitions (Map.mapKeys ([step] ++) m))

-- | Takes a record quantifier and wraps the binder & body in a tensor quantifier
--  e.g. given Pair has fields { a : Real, b : Real }
--  forall (r : Pair) . (body)
--  becomes
--  forall (_t0 : tensor Real [2]) . (body (_PairFromTensor _t0))
wrapQuantifyRecord ::
  (MonadQueryStructure m) =>
  QuantifyRecordArgs (Thunk Builtin) (Closure Builtin) ->
  m (QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin), CompilationStep)
wrapQuantifyRecord QuantifyRecordArgs {..} = do
  namedCtx <- getNameContext
  forcedType <- forceThunk quantifyRecordType
  recordTypeIdent <- case forcedType of
    VFreeVar v _spine -> pure v
    _ -> developerError "Record binder is not of expected format."

  -- Construct \r -> body from binder and body in record quantifier args
  let recordQLam = unnormalise (boundCtxLv namedCtx) $ VLam quantifyRecordBinder quantifyRecordBody
  fields <- getRecordFields recordTypeIdent
  shape <- getTensorRecordShape fields
  let dims = Forced $ mkDims shape

  -- Build tensor binder with appropriate dims and type for record
  let Closure boundEnv _body = quantifyRecordBody
  let tensorType = Forced $ ITensorType (Forced IRatType) dims
  let tensorBinderName = getFreshTensorBinderName namedCtx
  let tensorBinder = mkExplicitBinder tensorType (Just (mempty, tensorBinderName))

  let tensorBoundVar = explicit $ BoundVar mempty 0
  recordTypeProv <- getRecordProvenance recordTypeIdent
  -- Construct _PairFromTensor _t0
  let fromTensorExpr = App (FreeVar recordTypeProv $ constructFromTensorFreeVar recordTypeIdent) [tensorBoundVar]

  -- Construct body (_PairFromTensor _t0)
  let nestedBody = App recordQLam [Arg Explicit Relevant fromTensorExpr]
  let ratTensorArgs = QuantifyRatTensorArgs (Forced IDimNil) dims tensorBinder (Closure boundEnv nestedBody)

  fieldNames <- getRecordFieldNames recordTypeIdent
  let name = getBinderName quantifyRecordBinder
  return (ratTensorArgs, ConvertQuantifiedTensorLike tensorBinderName name fieldNames)

eliminateExists ::
  (MonadQueryStructure m) =>
  QuantifyRatTensorArgs (Thunk Builtin) (Closure Builtin) ->
  m (MaybeTrivial Partitions)
eliminateExists (QuantifyRatTensorArgs _pDims _bDims binder closure) = do
  let varName = getBinderName binder
  let subpassDoc = "elimination of existential quantifier over" <+> quotePretty varName
  logCompilerSection2 MidDetail subpassDoc $ do
    -- Get the shape and name of the quantified variable
    namedCtx <- getNameContext
    propertyProv <- asks propertyProvenance
    userVarShapeValue <- createUserVar propertyProv namedCtx binder
    maybeShape <- getDims userVarShapeValue
    userVarShape <- case maybeShape of
      Just shape -> return shape
      _ -> throwError $ VariableSizeTensorQuantification propertyProv namedCtx binder userVarShapeValue

    -- Update the global context
    globalCtx <- get
    (userVar, newGlobalCtx) <- addUserVarToGlobalContext binder (UniModal userVarShape) globalCtx
    put newGlobalCtx

    -- Normalise the expression
    let normBody = extendClosureWithBound closure binder (toLv userVar)

    -- Recursively compile the expression.
    (partitions, networkInputEqualities) <-
      logCompilerSection2 MidDetail "reduction of body to assertion tree" $ runWriterT (compileBoolExpr normBody)

    -- Prepend network equalities to the tree (prepending is important for
    -- performance as the search for constraints will find them first.)
    networkEqPartitions <-
      logCompilerSection2 MidDetail "reduction of network equalities to assertion tree" $ networkEqualitiesToPartition networkInputEqualities

    let finalPartitions = andTrivial andPartitions partitions networkEqPartitions

    -- Solve for the user variable
    eliminateQuantifiedVariable finalPartitions userVar

eliminateExistless ::
  (MonadQueryStructure m) =>
  Thunk Builtin ->
  m (MaybeTrivial Partitions)
eliminateExistless value = do
  (maybePartitions, equalities) <- runWriterT $ compileBoolExpr value
  networkEqPartitions <- networkEqualitiesToPartition equalities
  return $ andTrivial andPartitions maybePartitions networkEqPartitions

-- | Attempts to compile an arbitrary expression of type `Bool` down to a tree
-- of assertions implicitly existentially quantified by a set of network
-- input/output variables.
compileBoolExpr ::
  (MonadQueryStructure m, MonadWriter [Thunk Builtin] m) =>
  Thunk Builtin ->
  m (MaybeTrivial Partitions)
compileBoolExpr value = do
  showEntry value
  forcedValue <- forceAndRewriteTensor value
  showExit =<< case toBoolValue forcedValue of
    ----------------
    -- Base cases --
    ----------------
    VBoolLiteral b -> return $ Trivial b
    VCompareRatTensor (op, args) -> purifyAndCompileAssertion op args
    VQuantifyRatTensor (Forall, _) -> throwError catchableUnsupportedAlternatingQuantifiersError
    VQuantifyRecord (Forall, _) -> throwError catchableUnsupportedAlternatingQuantifiersError
    VAnd (TensorOp2Args _dims x y) -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VOr (TensorOp2Args _dims x y) -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VImplies args -> compileBoolExpr $ elimImplies args
    VQuantifyRatTensor (Exists, args) -> eliminateExists args
    VQuantifyRecord (Exists, args) -> eliminateExistsRecord args
    ---------------------
    -- Recursive cases --
    ---------------------
    VNot arg -> compileBoolExpr =<< lowerNot unblockingActions arg
    VBoolIf args -> compileBoolExpr =<< unfoldIf args
    VCompareNat {} -> unblockAndRec forcedValue
    VCompareIndex {} -> unblockAndRec forcedValue
    VReduceAndTensor {} -> unblockAndRec forcedValue
    VReduceOrTensor {} -> unblockAndRec forcedValue
    VBoolTensorAt {} -> unblockAndRec forcedValue
    VBoolVectorAt {} -> unblockAndRec forcedValue
    VBoolFoldList {} -> unblockAndRec forcedValue
  where
    unblockAndRec e = compileBoolExpr =<< Unblocking.unblockBoolExpr unblockingActions (Forced e)

purifyAndCompileAssertion ::
  (MonadQuantifierBody m) =>
  ComparisonOp ->
  TensorComparisonArgs (Thunk Builtin) ->
  m (MaybeTrivial Partitions)
purifyAndCompileAssertion op args
  | op == Ne =
      -- We can't use non-equalities so just eliminate it
      compileBoolExpr =<< eliminateNotEqualRatTensor args
  | otherwise = do
      logCompilerSection2 MaxDetail "assertion compilation" $ do
        maybePurifiedValue <- purifyAssertion unblockingActions op args
        elimIfTree elimBranch elimLeaf maybePurifiedValue
  where
    elimLeaf :: (MonadQuantifierBody m) => (ComparisonOp, TensorComparisonArgs (Thunk Builtin)) -> m (MaybeTrivial Partitions)
    elimLeaf assertion = do
      resultOrError <- compilePurifiedAssertion assertion
      case resultOrError of
        Left recExpr -> compileBoolExpr recExpr
        Right linearAssertion -> return $ mkTrivialPartition linearAssertion

    elimBranch :: (MonadQuantifierBody m) => Thunk Builtin -> MaybeTrivial Partitions -> MaybeTrivial Partitions -> m (MaybeTrivial Partitions)
    elimBranch c x y = do
      c' <- compileBoolExpr c
      notC' <- compileBoolExpr (Forced $ mkExpr accessNotTensor $ TensorOp1Args (Forced IDimNil) c)
      let cAndx = andTrivial andPartitions c' x
      let notCAndy = andTrivial andPartitions notC' y
      return $ orTrivial orPartitions cAndx notCAndy

compilePurifiedAssertion ::
  (MonadQuantifierBody m) =>
  (ComparisonOp, TensorComparisonArgs (Thunk Builtin)) ->
  m (Either (Thunk Builtin) LinearAssertion)
compilePurifiedAssertion (op, args@(TensorComparisonArgs _pDims dims xs ys)) = do
  maybeShape <- getDims . Forced =<< forceAndRewriteDims dims
  shape <- case maybeShape of
    Just concreteShape -> return concreteShape
    Nothing -> do
      nameCtx <- getNameContext
      developerError $ "Non-concrete dimensions found" <+> prettyFriendly (WithContext dims nameCtx)

  maybeLinearAssertion <- compileLinearAssertion findVariableFromLevel op shape xs ys
  case maybeLinearAssertion of
    Right assertion -> do
      return $ Right assertion
    Left NonLinearity ->
      throwError catchableUnsupportedNonLinearConstraint
    Left (UnexpectedExpr e) ->
      developerError ("unexpected expression" <+> prettyVerbose e)
    Left (TrivialExpr b) ->
      return $ Left $ Forced $ IBoolLiteral b
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
    MonadWriter [Thunk Builtin] m
  )

unblockingActions ::
  (MonadQuantifierBody m) =>
  UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockBoundVar = unblockQuantifiedBoundVar,
      unblockNetworkApp = unblockNetworkApplication,
      unblockDatasetOrParameter = \_ _ -> unexpectedExprError "solver compilation" "dataset or parameter"
    }

unblockQuantifiedBoundVar ::
  (MonadQuantifierBody m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  Lv ->
  UnforcedSpine Builtin ->
  m (IfTree (Thunk Builtin) (Thunk Builtin))
unblockQuantifiedBoundVar unblock lv spine = case spine of
  _ : _ -> unexpectedExprError "purification" "bound var with non-empty spine"
  [] -> unblock =<< replaceTensorVariableWithStackedChildren (SliceVariable lv)

unblockNetworkApplication ::
  (MonadQuantifierBody m) =>
  TypeUnblockingFunction (Thunk Builtin) m ->
  TypeUnblockingFunction (Thunk Builtin) m ->
  Identifier ->
  OperationUnblockingFunction NetworkAppArgs (Thunk Builtin) m
unblockNetworkApplication unblockFnTensor unblockFnRecord ident (NetworkAppArgs arg) = do
  let name = nameOf ident
  networkInfo <- asks (lookupNetworkInfo name . networkCtx)
  let typ = networkType networkInfo

  -- The low-level network representation works over tensors
  -- Create two tensors representing the network input and output
  (inputVarExpr, outputVarExpr) <- addNetworkApplicationToGlobalCtx name networkInfo arg

  -- If our network outputs a tensorisable, convert our output expression to a record
  transformedOutputVarExpr <- case networkOutputType typ of
    UniModal (RecordIOType (NetworkRecordType _ recordTyp _ _)) -> do
      Forced <$> forceFreeVar (constructFromTensorFreeVar recordTyp) [explicit outputVarExpr]
    MultiModal _ -> error "Multimodal IO is not implemented yet"
    _ -> return outputVarExpr

  -- Create our input equality in terms of tensors (as record equality just converts to tensor equality anyway)
  -- If our network input is a tensorisable, i.e. arg is tensorisable, convert it to a tensor
  transformedArg <- case networkInputType typ of
    UniModal (RecordIOType (NetworkRecordType _ recordTyp _ _)) -> do
      Forced <$> forceFreeVar (constructToTensorFreeVar recordTyp) [explicit arg]
    MultiModal _ -> error "Multimodal IO is not implemented yet"
    _ -> return arg

  inputEquality <- case inputShape networkInfo of
    MultiModal _ -> error "MultiModal IO is not implemented yet"
    UniModal dims ->
      toComparison
        ( Eq,
          TensorOp2Args
            { tensorOp2Dims = Forced $ mkDims dims,
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
    UniModal (RecordIOType (NetworkRecordType {})) -> unblockFnRecord transformedOutputVarExpr
    UniModal (TensorIOType (NetworkTensorType {})) -> unblockFnTensor transformedOutputVarExpr
    MultiModal _ -> error "Multimodal IO is not implemented yet"

--------------------------------------------------------------------------------
-- Elimination operations

eliminateNotEqualRatTensor ::
  (MonadQueryStructure m) =>
  TensorComparisonArgs (Thunk Builtin) ->
  m (Thunk Builtin)
eliminateNotEqualRatTensor args = do
  PropertyMetaData {..} <- ask
  if supportsStrictInequalities queryFormat
    then throwError $ UnsupportedInequality (queryFormatID queryFormat) propertyProvenance
    else do
      let leq = Forced $ mkExpr accessCompareRatTensor (Le, args)
      let geq = Forced $ mkExpr accessCompareRatTensor (Ge, args)
      return $ Forced $ mkExpr accessOrTensor $ TensorOp2Args (tensorPointwiseDims args) leq geq

eliminateTensorAssertion ::
  forall m.
  (MonadQueryStructure m) =>
  ComparisonOp ->
  TensorComparisonArgs (Thunk Builtin) ->
  m (Thunk Builtin)
eliminateTensorAssertion op (TensorComparisonArgs _pDims rDims xs ys) = do
  forcedDims <- forceAndRewriteDims rDims
  case forcedDims of
    IDimNil -> do
      -- For scalar comparisons, directly apply the comparison
      toComparison (op, TensorOp2Args (Forced forcedDims) xs ys)
    IDimCons d ds -> do
      forcedDim <- forceThunk d
      case forcedDim of
        INatLiteral n -> do
          -- TODO switch to use `etaReduceTensor`?
          let tElem = Forced IRatType
          let d0Arg = Forced IDimNil
          let mkAt vs i = Forced $ mkExpr accessAtTensor (AtTensorArgs tElem d ds vs (Forced $ IIndexLiteral i d))
          let mkStackElement i = do
                let xsi = mkAt xs i
                let ysi = mkAt ys i
                toComparison (op, TensorOp2Args ds xsi ysi)
          stackElements <- traverse mkStackElement [0 .. (n - 1)] :: m [Thunk Builtin]
          let stackExpr = Forced $ mkExpr accessStackTensor (StackTensorArgs (Forced IBoolType) d d0Arg stackElements)
          let result = Forced $ mkExpr accessReduceAnd (TensorReductionArgs (Forced $ mkDims [n]) stackExpr)
          return result
        _ -> compilerDeveloperError ("unexpected dimension" <+> prettyVerbose d)
    _ -> compilerDeveloperError ("unexpected dimensions" <+> prettyVerbose rDims)

toComparison ::
  (MonadNorm Builtin m) =>
  (ComparisonOp, TensorOp2Args (Thunk Builtin)) ->
  m (Thunk Builtin)
toComparison (op, TensorOp2Args dims xs ys) = do
  return $
    Forced $
      mkExpr
        accessCompareRatTensor
        ( op,
          TensorComparisonArgs
            { tensorPointwiseDims = Forced IDimNil,
              tensorReduceDims = dims,
              tensorOp2Arg1 = xs,
              tensorOp2Arg2 = ys
            }
        )

networkEqualitiesToPartition ::
  (MonadQueryStructure m) =>
  [Thunk Builtin] ->
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

showEntry :: (MonadQueryStructure m) => Thunk Builtin -> m ()
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
