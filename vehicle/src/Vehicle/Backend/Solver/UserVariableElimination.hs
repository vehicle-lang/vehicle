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
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot (lowerNot)
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Rational.LinearExpr (LinearityError (..), compileLinearAssertion)
import Vehicle.Compile.Unblock (UnblockingActions (..))
import Vehicle.Compile.Unblock qualified as Unblocking
import Vehicle.Compile.Variable (createUserVar)
import Vehicle.Data.Builtin.Interface.Normalise (evalAtTensor, unoptimisedEvalReduceAndTensor)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Value
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (getNameContext, prettyFriendlyInCtx)
import Vehicle.Data.Variable.Bound.Context.Tensor (replaceTensorVariableWithStackedChildren)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Verify.QueryFormat (QueryFormat (..), supportsStrictInequalities)
import Prelude hiding (Applicative (..))
import Vehicle.Verify.Core 
import Vehicle.Compile.Resource
import Data.Text qualified as Text

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
    normExpr <- eval (Just userVarName : namedCtx) newEnv body -- here is where we are missing smth from context

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
compileBoolExpr :: -- we have the monad context for unnormalise here
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
    ---------------------
    -- Recursive cases --
    ---------------------
    VNot arg -> compileBoolExpr =<< lowerNot arg
    VBoolIf args -> compileBoolExpr =<< unfoldIf args
    VAnd (TensorOp2Args _dims x y) -> andTrivial andPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VOr (TensorOp2Args _dims x y) -> orTrivial orPartitions <$> compileBoolExpr x <*> compileBoolExpr y
    VQuantifyRatTensor (Exists, args) -> eliminateExists args
    VQuantifyRecord (Exists, _args) -> compilerDeveloperError "LAUREN TODO: hit case in compileBoolExpr"
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
          Left purifiedValue -> do
            _ <- logDebug MidDetail "--------- first value case -------------------"
            return $ Left purifiedValue
          Right purifiedArgs -> do
            _ <- logDebug MidDetail "--------- first assertion case -------------------" -- HITTING THIS HERE
            compilePurifiedAssertion op purifiedArgs

      case recurseOrResult of
        Left value -> do 
          _ <- logDebug MidDetail "--------- recursive value case -------------------"
          compileBoolExpr value
        Right assertion -> do 
          _ <- logDebug MidDetail "--------- recursive assertion case -------------------"
          return $ mkTrivialPartition assertion

compilePurifiedAssertion ::
  (MonadQuantifierBody m) =>
  ComparisonOp ->
  TensorOp2Args (Value Builtin) ->
  m (Either (Value Builtin) LinearAssertion)
compilePurifiedAssertion op args@(TensorOp2Args dims xs ys) = do
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
    Left (UnreducedExpr e) -> do -- hitting this, not sure why. should be getting more reduced by now i think??
      logDebugM MaxDetail $ do -- basically complaining that we can't reduce it (because AtTensor is not reducable?)
        exprDoc <- prettyFriendlyInCtx e
        return $ "non-variable-terms:" <+> exprDoc
      elementComparisonValue <- eliminateTensorAssertion op args -- this is where issues are, should be, I'm reducing down to  f₀[output]!0 <=. [tens!0, tens!1] ! 0 but do I need to go further?
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

unblockingActions :: (MonadQuantifierBody m, MonadPropertyStructure m) => UnblockingActions m
unblockingActions = UnblockingActions unblockQuantifiedBoundVar unblockNetworkApplication

unblockQuantifiedBoundVar ::
  (MonadQuantifierBody m) =>
  Lv ->
  m (Value Builtin)
unblockQuantifiedBoundVar lv =
  replaceTensorVariableWithStackedChildren (SliceVariable lv)

-- this is what we are hitting
unblockNetworkApplication ::
  (MonadQuantifierBody m) =>
  Identifier ->
  NetworkAppArgs (Value Builtin) ->
  m (Value Builtin)
unblockNetworkApplication ident (NetworkAppArgs arg) = do

  let name = nameOf ident
  networkInfo <- asks (lookupNetworkInfo name . networkCtx)

-- need to equate the input var with something of the correct record typw, and substitute the output var for something 
-- of the correct record type - preserve well-typedness

  let typ = networkType networkInfo
  _ <- logDebug MidDetail $ "network type is" <+> pretty (show typ)

  -- network input tensor is toTensor of network input (which is the args being fed into this function)
  -- network gives a tensor as an output then we convert it to a record with toRecord/fromTensor

  -- leave the tensors stored in the global context as they are but just wrap the equality expressions
  -- input:
  -- Pair {} = toRecord (sliceTensor) 
  -- output:
  -- sliceTensor
  -- so for both we would have to do the same thing????

  (inputVarExpr, outputVarExpr) <- addNetworkApplicationToGlobalCtx name networkInfo arg

  transformedInput <- case inputTensor typ of 
    NetworkRecordTypeConstructor (NetworkRecordType _baseType typIdent _dims _fields) -> do 
      let fromTensorName = Text.pack "_" <> identifierName typIdent <> "FromTensor"
      let fromTensorFreeVar = FreeVar mempty (Identifier (modulePath typIdent) fromTensorName)
      let inputVarArg = Arg Explicit Relevant inputVarExpr
      ctx <- getNameContext
      fromTensorValue <- eval ctx emptyBoundEnv fromTensorFreeVar
      evalApp ctx fromTensorValue [inputVarArg]
    _ -> return inputVarExpr

  transformedOutput <- case outputTensor typ of 
    NetworkRecordTypeConstructor (NetworkRecordType _baseType typIdent _dims _fields) -> do 
      let fromTensorName = Text.pack "_" <> identifierName typIdent <> "FromTensor"
      let fromTensorFreeVar = FreeVar mempty (Identifier (modulePath typIdent) fromTensorName)
      let outputVarArg = Arg Explicit Relevant outputVarExpr
      ctx <- getNameContext
      fromTensorValue <- eval ctx emptyBoundEnv fromTensorFreeVar
      evalApp ctx fromTensorValue [outputVarArg]
    _ -> return outputVarExpr

    -- do I need to wrap it in a lam to make it not register as a network application?
    -- may need to eval the application so it subsitutes and doesnt show up as a freevar
    -- conversion functions never get evalled when they are here
    
    --   normalisedTensorType <- eval namedCtx boundEnv tensorType

  -- namedCtx <- getNameContext
  -- normalisedFnApplication<- eval namedCtx boundEnv tensorType

  let inputEquality =
        fromBoolValue $
          VCompareRatTensor -- might have to make a version of this that converts both to records in order to do this?
            ( Eq,
              TensorOp2Args
                { tensorOp2Dims = mkDims (inputShape networkInfo),
                  tensorOp2Arg1 = transformedInput,
                  tensorOp2Arg2 = arg
                }
            )
  tell [inputEquality]

  logDebugM MaxDetail $ do
    inputEqualityDoc <- prettyFriendlyInCtx inputEquality
    replacementExprDoc <- prettyFriendlyInCtx transformedOutput
    return $
      "note-input-equality" <+> inputEqualityDoc
        <> line
        <> "replace-expr" <+> replacementExprDoc

  logDebug MidDetail $ pretty (show transformedOutput)
  return transformedOutput

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
eliminateTensorAssertion op (TensorOp2Args dims xs ys) = do
  _ <- logDebug MidDetail $ "dims are" <+> pretty (show dims) <+> "xs are" <+> pretty (show xs) <+> "ys are" <+> pretty (show ys)
  case dims of
    IDimCons d@(INatLiteral n) ds -> do -- our dims are not of this type (they are dimNil), we never enter here in tensor-only example
      -- TODO switch to use `etaReduceTensor`?
      nameCtx <- getNameContext
      let tElem = fromTypeValue VRatType
      let d0Arg = mkDims []
      let mkAt vs i = evalAtTensor nameCtx evalApp eval (AtTensorArgs tElem d ds vs (IIndexLiteral i))
      let mkStackElement i = do
            xsi <- mkAt xs i
            ysi <- mkAt ys i
            evalCompareRatTensor op (TensorOp2Args ds xsi ysi)
      stackElements <- traverse mkStackElement [0 .. (n - 1)] :: m [Value Builtin]
      let stackExpr = fromBoolTensorValue $ VBoolStackTensor (StackTensorArgs tElem d d0Arg stackElements)
      result <- unoptimisedEvalReduceAndTensor (TensorReductionArgs (mkDims [n]) (IBoolLiteral True) stackExpr)
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
