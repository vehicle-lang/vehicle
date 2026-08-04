module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifiers,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.RWS (MonadReader, ask)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer.Strict (MonadWriter (..), runWriterT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Normalise.Builtin (elimImplies)
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Vehicle.Data.Builtin.Interface (Accessor (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Variable.Bound.Context.Name (runFreshNameBoundContextT)
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (getFreeCtx), addDeclEntryToContext, runFreshFreeContextT)

type MonadLiftQuantifiers m =
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m,
    MonadReader DeclProvenance m
  )

type OldContextValue = Thunk Builtin

type NewContextValue = Thunk Builtin

type QuantifierData = (Quantifier, Either (UnforcedDims Builtin, UnforcedDims Builtin) (UnforcedType Builtin), UnforcedBinder Builtin)

type PropertyData = Map Name Bool

liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m (PropertyData, Prog Builtin)
liftQuantifiers (Main ds) = logCompilerPass LossLogic $
  runFreshFreeContextT (Proxy @Builtin) $ do
    (ds', propertyData) <- runWriterT (liftQuantifierDecls ds)
    return (propertyData, Main ds')

liftQuantifierDecls ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadWriter PropertyData m
  ) =>
  [Decl Builtin] ->
  m [Decl Builtin]
liftQuantifierDecls = \case
  [] -> return []
  decl : decls -> do
    logDebug MaxDetail $ pretty $ identifierOf decl
    logDebugM MaxDetail $ do
      pretty . Map.keys <$> getFreeCtx (Proxy @Builtin)
    decl' <- liftQuantifierDecl decl
    decls' <- addDeclEntryToContext decl' $ liftQuantifierDecls decls
    return (decl' : decls')

liftQuantifierDecl ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadWriter PropertyData m
  ) =>
  Decl Builtin ->
  m (Decl Builtin)
liftQuantifierDecl decl = case decl of
  DefAbstract {} -> return decl
  DefRecord {} -> return decl
  DefFunction p ident ann typ expr ->
    if isAnnotatedAsProperty ann
      then do
        ((quantifiers, value), _) <-
          runFreshNameBoundContextT $
            flip runReaderT (ident, p) $
              liftQuantifierProperty (Unforced emptyBoundEnv expr, 0)
        ((newQuantifiers, newValue), findCounterExample) <- runReaderT (flipForall (quantifiers, value)) (ident, p)
        liftedValue <- runFreshNameBoundContextT $ reconstructProperty newQuantifiers newValue
        logDebug MaxDetail $ do
          prettyFriendlyEmptyCtx liftedValue
        tell (Map.singleton (identifierName ident) findCounterExample)
        return $ DefFunction p ident ann typ $ unnormalise 0 liftedValue
      else return decl

-- | Returns a Bool representing whether performing a search on the property would produce an adversarial example
-- True = example input found will be an adversarial example to the property
-- False = example input found will not be a counter-example
flipForall ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  ([QuantifierData], NewContextValue) ->
  m (([QuantifierData], NewContextValue), Bool)
flipForall (quantifierData, value) = case quantifierData of
  [] -> return ((quantifierData, value), True)
  (firstQuantifier, _, _) : _ -> do
    newQuantifierData <- checkAlternatingQuantifiers firstQuantifier quantifierData
    return $
      if firstQuantifier == Forall
        then ((newQuantifierData, Forced $ mkExpr accessNotTensor (TensorOp1Args (Forced IDimNil) value)), True) -- not sure what dims to use here
        else ((newQuantifierData, value), False)

-- | Throws an error if there are alternating quantifiers in the
-- property and makes all quantifiers existential
checkAlternatingQuantifiers ::
  ( MonadCompile m,
    MonadReader DeclProvenance m
  ) =>
  Quantifier ->
  [QuantifierData] ->
  m [QuantifierData]
checkAlternatingQuantifiers prevQuantifier quantifierData = case quantifierData of
  [] -> return []
  (quantifier, dimsOrTyp, binder) : qs ->
    if prevQuantifier /= quantifier
      then do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv
      else do
        newData <- checkAlternatingQuantifiers quantifier qs
        return ((Exists, dimsOrTyp, binder) : newData)

reconstructProperty ::
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  ) =>
  [QuantifierData] ->
  NewContextValue ->
  m (Thunk Builtin)
reconstructProperty quantifiers value = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do
    newBody <- addNameToContext binder $ do
      reconstructed <- reconstructProperty qs value
      lv <- getBinderDepth
      return $ unnormalise lv reconstructed
    newEnv <- namedBoundContextToEnv <$> getNameContext
    case dimsOrType of
      Left (pDims, bDims) -> return (Forced $ mkExpr accessQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder (Closure newEnv newBody)))
      Right typ -> return (Forced $ mkExpr accessQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))

liftQuantifierProperty ::
  (MonadLiftQuantifiers m) =>
  (OldContextValue, Lv) ->
  m (([QuantifierData], NewContextValue), Lv)
liftQuantifierProperty (value, ctxDelta) = do
  forcedValue <- forceThunk value
  case toBoolValue forcedValue of
    VBoolLiteral _ ->
      return (([], value), 0)
    VAnd (TensorOp2Args dims arg1 arg2) -> do
      ((quantifiers1, arg1'), ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
      ((quantifiers2, arg2'), ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
      return ((quantifiers1 ++ quantifiers2, Forced $ mkExpr accessAndTensor (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2)
    VOr (TensorOp2Args dims arg1 arg2) -> do
      ((quantifiers1, arg1'), ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
      ((quantifiers2, arg2'), ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
      return ((quantifiers1 ++ quantifiers2, Forced $ mkExpr accessOrTensor (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2)
    VNot (TensorOp1Args dims arg) -> do
      ((quantifiers, arg'), ctxSize) <- liftQuantifierProperty (arg, ctxDelta)
      return ((quantifiers, Forced $ mkExpr accessNotTensor (TensorOp1Args dims arg')), ctxSize)
    VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs pDims bDims binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      ((quantifiers, body'), ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
      return (((quantifier, Left (pDims, bDims), binder) : quantifiers, body'), ctxSize + 1)
    VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
      lv <- getBinderDepth
      let normBody = extendClosureWithBound closure binder lv
      ((quantifiers, body'), ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
      return (((quantifier, Right typ, binder) : quantifiers, body'), ctxSize + 1)
    VBoolIf args -> do
      unfolded <- unfoldIf args
      liftQuantifierProperty (unfolded, ctxDelta)
    VCompareIndex (op, IndexComparisonArgs size1 size2 arg1 arg2) -> do
      arg1' <- updateIndexBoundVar ctxDelta arg1
      arg2' <- updateIndexBoundVar ctxDelta arg2
      return (([], Forced $ mkExpr accessCompareIndex (op, IndexComparisonArgs size1 size2 arg1' arg2')), 0)
    VCompareNat (op, args) -> do
      args' <- traverseOp2Args (updateNatBoundVar ctxDelta) args
      return (([], Forced $ mkExpr accessCompareNat (op, args')), 0)
    VCompareRatTensor (op, TensorComparisonArgs rDims pDims xs ys) -> do
      xs' <- updateRatTensorBoundVar ctxDelta xs
      ys' <- updateRatTensorBoundVar ctxDelta ys
      let args' = TensorComparisonArgs rDims pDims xs' ys'
      return (([], Forced $ mkExpr accessCompareRatTensor (op, args')), 0)
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
      spine' <- traverseArgs (updateRatTensorBoundVar lv) spine
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
