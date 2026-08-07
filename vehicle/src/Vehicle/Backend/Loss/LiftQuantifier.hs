module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifiers,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.RWS (MonadReader, ask)
import Control.Monad.Reader (runReaderT)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.LowerNot
import Vehicle.Compile.Normalise.Builtin (elimImplies)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard hiding (And)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (getFreeCtx), addDeclEntryToContext, runFreshFreeContextT)
import Vehicle.Verify.Specification (Property, QuerySet (..), propertySize, traverseProperty)

type MonadLiftQuantifiers m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

type OldContextValue = Thunk Builtin

type NewContextValue = Thunk Builtin

type QuantifierData = (Quantifier, Either (UnforcedDims Builtin, UnforcedDims Builtin) (UnforcedType Builtin), UnforcedBinder Builtin)

type HasForall = Bool

type HasExists = Bool

type LiftedData = ([QuantifierData], NewContextValue, HasForall, HasExists)

type LiftedExpr = Thunk Builtin

liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m ([Decl Builtin], [(Name, Property LiftedExpr)])
liftQuantifiers (Main ds) = logCompilerPass LossLogic $
  runFreshFreeContextT (Proxy @Builtin) $ do
    (nonProperties, properties) <- liftQuantifierDecls ds
    return (nonProperties, properties)

liftQuantifierDecls ::
  ( MonadCompile m,
    MonadFreeContext Builtin m
  ) =>
  [Decl Builtin] ->
  m ([Decl Builtin], [(Name, Property LiftedExpr)])
liftQuantifierDecls = \case
  [] -> return ([], [])
  decl : decls -> do
    logDebug MaxDetail $ pretty $ identifierOf decl
    logDebugM MaxDetail $ do
      pretty . Map.keys <$> getFreeCtx (Proxy @Builtin)
    decl' <- liftQuantifierDecl decl
    case decl' of
      Left nonProperty -> do
        (nonProperties, properties) <- addDeclEntryToContext decl $ liftQuantifierDecls decls -- do I add decl' or decl to context?
        return (nonProperty : nonProperties, properties)
      Right property -> do
        (nonProperties, properties) <- addDeclEntryToContext decl $ liftQuantifierDecls decls -- do I add decl' or decl to context?
        return (nonProperties, property : properties)

liftQuantifierDecl ::
  ( MonadCompile m,
    MonadFreeContext Builtin m
  ) =>
  Decl Builtin ->
  m (Either (Decl Builtin) (Name, Property LiftedExpr))
liftQuantifierDecl decl = case decl of
  DefAbstract {} -> return $ Left decl
  DefRecord {} -> return $ Left decl
  DefFunction p ident ann _ expr ->
    if isAnnotatedAsProperty ann
      then do
        let name = nameOf ident
        propertyNotLowered <-
          runFreshNameBoundContextT $
            flip runReaderT (ident, p) $
              applyDeMorgan (Unforced emptyBoundEnv expr)
        (propertyLifted, _) <-
          runFreshNameBoundContextT $
            flip runReaderT (ident, p) $
              liftQuantifierProperty (propertyNotLowered, 0)
        propertyExistential <- runReaderT (eliminateForall propertyLifted) (ident, p)
        propertyReconstructed <- runFreshNameBoundContextT $ reconstructProperty propertyExistential
        -- tell (Map.singleton (identifierName ident) findCounterExample)
        return $ Right (name, propertyReconstructed)
      else return $ Left decl

applyDeMorgan ::
  (MonadLiftQuantifiers m) =>
  OldContextValue ->
  m OldContextValue
applyDeMorgan value = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ -> return value
    VAnd args -> do
      args' <- traverseTensorOp2Args applyDeMorgan args
      return (Forced $ mkExpr accessAndTensor args')
    VOr args -> do
      args' <- traverseTensorOp2Args applyDeMorgan args
      return (Forced $ mkExpr accessOrTensor args')
    VNot args -> do
      lowered <- lowerNot unblockingActions args
      return lowered
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      body' <- addNameToContext binder $ applyDeMorgan normBody
      let newBody = unnormalise lv body'
      newEnv <- namedBoundContextToEnv <$> getNameContext
      return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      body' <- addNameToContext binder $ applyDeMorgan normBody
      let newBody = unnormalise lv body'
      newEnv <- namedBoundContextToEnv <$> getNameContext
      return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))
    VCompareIndex (op, IndexComparisonArgs size1 size2 arg1 arg2) -> do
      arg1' <- applyDeMorgan arg1
      arg2' <- applyDeMorgan arg2
      return (Forced $ mkExpr accessCompareIndex (op, IndexComparisonArgs size1 size2 arg1' arg2'))
    VCompareNat (op, args) -> do
      args' <- traverseOp2Args applyDeMorgan args
      return (Forced $ mkExpr accessCompareNat (op, args'))
    VCompareRatTensor (op, TensorComparisonArgs rDims pDims xs ys) -> do
      xs' <- applyDeMorgan xs
      ys' <- applyDeMorgan ys
      let args' = TensorComparisonArgs rDims pDims xs' ys'
      return (Forced $ mkExpr accessCompareRatTensor (op, args'))
    VBoolIf args -> do
      unfolded <- unfoldIf args
      applyDeMorgan unfolded
    VImplies args -> do
      let unfolded = elimImplies args
      applyDeMorgan unfolded
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      unblocked <- unblockBoolExpr unblockingActions value
      applyDeMorgan unblocked

eliminateForall ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  Property LiftedData ->
  m (Property LiftedData)
eliminateForall = traverseProperty eliminateForallCheckAlternatingQuantifiers

-- | Throws an error if there are alternating quantifiers in the
-- property and makes all quantifiers existential
eliminateForallCheckAlternatingQuantifiers ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  LiftedData ->
  m LiftedData
eliminateForallCheckAlternatingQuantifiers (quantifiers, value, hasForall, hasExists) =
  if hasForall && hasExists
    then do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    else do
      newQuantifiers <- flipForall quantifiers
      return (newQuantifiers, value, False, True)

flipForall ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  [QuantifierData] ->
  m [QuantifierData]
flipForall quantifiers = case quantifiers of
  [] -> return []
  (_, dimsOrType, binder) : qs -> do
    newQuantifiers <- flipForall qs
    return ((Exists, dimsOrType, binder) : newQuantifiers)

reconstructProperty ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  Property LiftedData ->
  m (Property LiftedExpr)
reconstructProperty = traverseProperty reconstructLiftedExpr

reconstructLiftedExpr ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  LiftedData ->
  m LiftedExpr
reconstructLiftedExpr (quantifiers, value, hasForall, hasExists) = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do
    newBody <- addNameToContext binder $ do
      reconstructed <- reconstructLiftedExpr (qs, value, hasForall, hasExists)
      lv <- getBinderDepth
      return $ unnormalise lv reconstructed
    logDebug MaxDetail $ prettyFriendlyEmptyCtx newBody
    newEnv <- namedBoundContextToEnv <$> getNameContext
    case dimsOrType of
      Left (pDims, bDims) -> return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
      Right typ -> return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))

-- A property should never be Trivial?
liftQuantifierProperty ::
  (MonadLiftQuantifiers m) =>
  (OldContextValue, Lv) ->
  m (Property LiftedData, Lv)
liftQuantifierProperty (value, ctxDelta) = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ ->
      return (NonTrivial $ Query $ QuerySet False (DisjunctAll [([], value, False, False)]), 0)
    VAnd (TensorOp2Args dims arg1 arg2) -> do
      (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
      (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
      case (arg1', arg2') of
        (NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers1, newValue1, hasForall1, hasExists1)]))), NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers2, newValue2, hasForall2, hasExists2)])))) -> do
          let hasForall = hasForall1 || hasForall2
          let hasExists = hasExists1 || hasExists2
          if hasExists
            then do
              let query1 = Query (QuerySet False (DisjunctAll [(quantifiers1, newValue1, hasForall1, hasExists1)]))
              let query2 = Query (QuerySet False (DisjunctAll [(quantifiers2, newValue2, hasForall2, hasExists2)]))
              return (NonTrivial $ Conjunct $ ConjunctAll [query1, query2], ctxSize1 + ctxSize2)
            else do
              let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers1 ++ quantifiers2, Forced $ mkExpr accessAndTensor (TensorOp2Args dims newValue1 newValue2), hasForall, hasExists)])
              return (NonTrivial newQuery, ctxSize1 + ctxSize2)
        (NonTrivial conjunctOrDisjunct1, NonTrivial conjunctOrDisjunct2) -> return (NonTrivial $ Conjunct $ ConjunctAll [conjunctOrDisjunct1, conjunctOrDisjunct2], ctxSize1 + ctxSize2)
        _ -> developerError "Conjunct cannot contain trivial args"
    VOr (TensorOp2Args dims arg1 arg2) -> do
      (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
      (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
      case (arg1', arg2') of
        (NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers1, newValue1, hasForall1, hasExists1)]))), NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers2, newValue2, hasForall2, hasExists2)])))) -> do
          let hasForall = hasForall1 || hasForall2
          let hasExists = hasExists1 || hasExists2
          if hasForall
            then do
              let query1 = Query (QuerySet False (DisjunctAll [(quantifiers1, newValue1, hasForall1, hasExists1)]))
              let query2 = Query (QuerySet False (DisjunctAll [(quantifiers2, newValue2, hasForall2, hasExists2)]))
              return (NonTrivial $ Disjunct $ DisjunctAll [query1, query2], ctxSize1 + ctxSize2)
            else do
              let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers1 ++ quantifiers2, Forced $ mkExpr accessOrTensor (TensorOp2Args dims newValue1 newValue2), hasForall, hasExists)])
              return (NonTrivial newQuery, ctxSize1 + ctxSize2)
        (NonTrivial conjunctOrDisjunct1, NonTrivial conjunctOrDisjunct2) -> return (NonTrivial $ Disjunct $ DisjunctAll [conjunctOrDisjunct1, conjunctOrDisjunct2], ctxSize1 + ctxSize2)
        _ -> developerError "Disjunct cannot contain trivial args"
    VNot (TensorOp1Args dims arg) -> do
      (arg', ctxSize) <- liftQuantifierProperty (arg, ctxDelta)
      case arg' of
        NonTrivial (Query (QuerySet _ (DisjunctAll [(quantifiers, newValue, hasForall, hasExists)]))) -> do
          let newQuery = Query $ QuerySet hasForall (DisjunctAll [(quantifiers, Forced $ mkExpr accessNotTensor $ TensorOp1Args dims newValue, hasForall, hasExists)])
          return (NonTrivial newQuery, ctxSize)
        NonTrivial _ -> developerError "Negation must be pushed below and/or"
        _ -> developerError "Negation cannot contain trivial args"
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      let quantifierData = (quantifier, Left (pDims, bDims), binder)
      (body', ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
      case body' of
        NonTrivial (Query querySet) -> do
          newQuerySet <- addQuantifierToQuerySet quantifierData querySet
          return (NonTrivial $ Query newQuerySet, ctxSize + 1)
        NonTrivial conjunctOrDisjunct -> do
          (lowered, newCtxSize) <- lowerQuantifier quantifierData ctxSize (NonTrivial conjunctOrDisjunct)
          return (lowered, newCtxSize)
        _ -> developerError "Quantifier body cannot be trivial"
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      let quantifierData = (quantifier, Right typ, binder)
      (body', ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
      case body' of
        NonTrivial (Query querySet) -> do
          newQuerySet <- addQuantifierToQuerySet quantifierData querySet
          return (NonTrivial $ Query newQuerySet, ctxSize + 1)
        NonTrivial conjunctOrDisjunct -> do
          (lowered, newCtxSize) <- lowerQuantifier quantifierData ctxSize (NonTrivial conjunctOrDisjunct)
          return (lowered, newCtxSize)
        _ -> developerError "Quantifier body cannot be trivial"
    VCompareIndex (op, IndexComparisonArgs size1 size2 arg1 arg2) -> do
      arg1' <- updateIndexBoundVar ctxDelta arg1
      arg2' <- updateIndexBoundVar ctxDelta arg2
      let newQuery = Query $ QuerySet False (DisjunctAll [([], Forced $ mkExpr accessCompareIndex (op, IndexComparisonArgs size1 size2 arg1' arg2'), False, False)])
      return (NonTrivial newQuery, 0)
    VCompareNat (op, args) -> do
      args' <- traverseOp2Args (updateNatBoundVar ctxDelta) args
      let newQuery = Query $ QuerySet False (DisjunctAll [([], Forced $ mkExpr accessCompareNat (op, args'), False, False)])
      return (NonTrivial newQuery, 0)
    VCompareRatTensor (op, TensorComparisonArgs rDims pDims xs ys) -> do
      xs' <- updateRatTensorBoundVar ctxDelta xs
      ys' <- updateRatTensorBoundVar ctxDelta ys
      let args' = TensorComparisonArgs rDims pDims xs' ys'
      let newQuery = Query $ QuerySet False (DisjunctAll [([], Forced $ mkExpr accessCompareRatTensor (op, args'), False, False)])
      return (NonTrivial newQuery, 0)
    VBoolIf args -> do
      unfolded <- unfoldIf args
      liftQuantifierProperty (unfolded, ctxDelta)
    VImplies args -> do
      let unfolded = elimImplies args
      liftQuantifierProperty (unfolded, ctxDelta)
    VBoolVectorAt {} -> unblock
    VBoolFoldList {} -> unblock
    VReduceAndTensor {} -> unblock
    VReduceOrTensor {} -> unblock
    VBoolTensorAt {} -> unblock
  where
    unblock = do
      unblocked <- unblockBoolExpr unblockingActions value
      liftQuantifierProperty (unblocked, ctxDelta)

lowerQuantifier ::
  (MonadLiftQuantifiers m) =>
  QuantifierData ->
  Lv ->
  Property LiftedData ->
  m (Property LiftedData, Lv)
lowerQuantifier quantifierData ctxSize property = case property of
  NonTrivial expr -> do
    newExpr <- traverse (addQuantifierToQuerySet quantifierData) expr
    let newProperty = NonTrivial newExpr
    let newPropertySize = propertySize newProperty
    -- When a quantifier is lowered, it is prepended to every LiftedData's list of quantifier data
    -- so the property's context size grows by how many LiftedData leaves there are
    let newCtxSize = ctxSize + Lv newPropertySize
    return (newProperty, newCtxSize)
  _ -> developerError "Cannot lower a quantifier into an empty property"

addQuantifierToQuerySet ::
  (MonadLiftQuantifiers m) =>
  QuantifierData ->
  QuerySet LiftedData ->
  m (QuerySet LiftedData)
addQuantifierToQuerySet (quantifier, dimsOrType, binder) querySet = case querySet of
  QuerySet _ (DisjunctAll [(quantifiers, value, hasForall, hasExists)]) -> do
    let newQuantifiers = (quantifier, dimsOrType, binder) : quantifiers
    if quantifier == Forall
      then return $ QuerySet True (DisjunctAll [(newQuantifiers, value, True, hasExists)])
      else return $ QuerySet hasForall (DisjunctAll [(newQuantifiers, value, hasForall, True)])
  _ -> developerError "Missing lifted expression"

updateIndexBoundVar ::
  (MonadLiftQuantifiers m) =>
  Lv ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateIndexBoundVar lv value = do
  forcedValue <- forceThunk value
  case toIndexValue forcedValue of
    VIndexLiteral {} ->
      return value
    VIndexBoundVar v spine -> do
      spine' <- traverseArgs (updateIndexBoundVar lv) spine
      return $ Forced $ VBoundVar (v + lv) spine'
    VIndexIf {} -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VIndexAtVector (AtVectorArgs typ dim vector idx) -> do
      vector' <- updateIndexBoundVar lv vector
      return (Forced $ mkExpr accessAtVector (AtVectorArgs typ dim vector' idx))
    VIndexParameter {} ->
      return value
    VIndexRecordAcc typ val fieldName spine -> do
      val' <- updateIndexBoundVar lv val
      spine' <- traverseArgs (updateIndexBoundVar lv) spine
      return (Forced $ VRecordAcc typ val' fieldName spine')

updateNatBoundVar ::
  (MonadLiftQuantifiers m) =>
  Lv ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateNatBoundVar lv value = do
  forcedValue <- forceThunk value
  case toNatValue forcedValue of
    VNatLiteral _ -> return value
    VNatBoundVar v spine -> do
      spine' <- traverseArgs (updateNatBoundVar lv) spine
      return (Forced $ VBoundVar (v + lv) spine')
    VNatIf _ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VNatAdd args -> do
      args' <- traverseOp2Args (updateNatBoundVar lv) args
      return (Forced $ mkExpr accessAddNat args')
    VNatMul args -> do
      args' <- traverseOp2Args (updateNatBoundVar lv) args
      return (Forced $ mkExpr accessMulNat args')
    VNatParameter _ -> return value

updateRatTensorBoundVar ::
  (MonadLiftQuantifiers m) =>
  Lv ->
  Thunk Builtin ->
  m (Thunk Builtin)
updateRatTensorBoundVar lv value = do
  forcedValue <- forceThunk value
  case toRatTensorValue forcedValue of
    VRatTensorLiteral _ ->
      return value
    VNegRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar lv arg
      return (Forced $ mkExpr accessNegRatTensor (TensorOp1Args dims arg'))
    VLogRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar lv arg
      return (Forced $ mkExpr accessLogRatTensor (TensorOp1Args dims arg'))
    VExpRatTensor (TensorOp1Args dims arg) -> do
      arg' <- updateRatTensorBoundVar lv arg
      return (Forced $ mkExpr accessExpRatTensor (TensorOp1Args dims arg'))
    VAddRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessAddRatTensor args')
    VSubRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessSubRatTensor args')
    VMulRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessMulRatTensor args')
    VDivRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessDivRatTensor args')
    VMinRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessMinRatTensor args')
    VMaxRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessMaxRatTensor args')
    VPowRatTensor args -> do
      args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessPowRatTensor args')
    VReduceAddRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessReduceAddRat args')
    VReduceMulRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessReduceMulRat args')
    VReduceMinRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessReduceMinRat args')
    VReduceMaxRatTensor args -> do
      args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessReduceMaxRat args')
    VIfRatTensor _ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
    VRatTensorBoundVar v ->
      return (Forced $ VBoundVar (v + lv) [])
    VNetworkApplication ident (NetworkAppArgs arg) -> do
      arg' <- updateRatTensorBoundVar lv arg
      return (Forced $ VFreeVar ident (mkExpr accessSpine $ NetworkAppArgs arg'))
    VRatConstTensor args -> do
      args' <- traverseConstTensorValue (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessConstTensor args')
    VRatStackTensor args -> do
      args' <- traverseStackTensorElements (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessStackTensor args')
    VRatAtTensor args -> do
      args' <- traverseAtTensorArg (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessAtTensor args')
    VRatTensorTranspose args -> do
      args' <- traverseTransposeTensor (updateRatTensorBoundVar lv) args
      return (Forced $ mkExpr accessTransposeTensor args')
    VRatForeach (ForeachTensorArgs typ d ds fn) -> do
      fn' <- updateRatTensorBoundVar lv fn
      return (Forced $ mkExpr accessForeachTensor (ForeachTensorArgs typ d ds fn'))
    VRatTensorRecordAcc typ val fieldName spine -> do
      val' <- updateRatTensorBoundVar lv val
      spine' <- traverseArgs (updateRatTensorBoundVar lv) spine
      return (Forced $ VRecordAcc typ val' fieldName spine')
    VParameterOrDataset _ ->
      return value
    VRatAtVector (AtVectorArgs typ dim vector idx) -> do
      vector' <- updateRatTensorBoundVar lv vector
      return (Forced $ mkExpr accessAtVector (AtVectorArgs typ dim vector' idx))

unblockingActions :: (MonadLiftQuantifiers m) => UnblockingActions m
unblockingActions =
  UnblockingActions
    { unblockRatTensorBoundVar = \_ -> do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv,
      unblockRecordBoundVar = \_ -> do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv,
      unblockNetworkApp = \_ _ _ _ -> do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv,
      unblockDatasetOrParameter = \_ -> do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv
    }
