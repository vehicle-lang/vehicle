module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifiers,
  )
where

import Data.Proxy (Proxy (..))
import Control.Monad.Except (MonadError (..))
import Vehicle.Prelude
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (addBinderToContext, getBoundCtx), runFreshBoundContextT, addBinderToContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, runFreshFreeContextT, addDeclEntryToContext)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Value (Closure (..), Value (..), VDecl, boundContextToEnv, VBinder, VDims, VType)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseClosure, evalDecl)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Control.Monad.RWS (MonadReader, ask)
import Control.Monad.Reader (runReaderT)
import Vehicle.Compile.Print (prettyFriendlyEmptyCtx)

type MonadLiftQuantifiers m = 
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadBoundContext (Value Builtin) m,
    MonadReader DeclProvenance m
  )

type OldContextValue = Value Builtin
type NewContextValue = Value Builtin
type QuantifierData = (Quantifier, Either (VDims Builtin) (VType Builtin), VBinder Builtin)

-- I think this should return the Prog containing the list of reconstructed decls plus [(propertyName, Bool)]
liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m([(Identifier, Bool)], Prog Builtin)
liftQuantifiers (Main ds) = 
  runFreshFreeContextT (Proxy @Builtin) $ do
    (declsData, ds') <- liftQuantifierDecls ds
    return (declsData, Main ds')

-- Possibly return [(propertyName, Bool)] in addition to the list of reconstructed decls
liftQuantifierDecls :: 
  (MonadCompile m, MonadFreeContext Builtin m) => 
  [Decl Builtin] ->
  m([(Identifier, Bool)],[Decl Builtin])
liftQuantifierDecls = \case
  [] -> return ([],[])
  decl : decls -> do
    normDecl <- evalDecl decl
    (declData, normDecl') <- liftQuantifierDecl normDecl
    let decl' = fmap (unnormalise 0) normDecl'
    (declsData', decls') <- addDeclEntryToContext normDecl $ liftQuantifierDecls decls 
    return (declData : declsData', decl' : decls')

-- accepts and returns normalised decl
liftQuantifierDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) => 
  VDecl Builtin ->
  m((Identifier, Bool), VDecl Builtin)
liftQuantifierDecl decl = case decl of
  DefAbstract _ ident _ _ -> return ((ident, False), decl)
  DefFunction p ident ann typ expr -> 
    if isAnnotatedAsProperty ann
      then do
        ((quantifiers, value), _) <- runFreshBoundContextT (Proxy @(Value Builtin)) $ runReaderT (liftQuantifierProperty (expr, 0)) (ident, p)
        printValue <- runFreshBoundContextT (Proxy @(Value Builtin)) $ reconstructProperty quantifiers value
        logDebug MinDetail $ prettyFriendlyEmptyCtx  printValue
        ((newQuantifiers, newValue), findCounterExample) <- runReaderT (flipForall (quantifiers, value)) (ident, p)
        liftedValue <- runFreshBoundContextT (Proxy @(Value Builtin)) $ reconstructProperty newQuantifiers newValue
        logDebug MinDetail $ prettyFriendlyEmptyCtx liftedValue
        return ((ident, findCounterExample), DefFunction p ident ann typ liftedValue)
      else return ((ident, False), decl)
  DefRecord _ ident _ _ _ -> return ((ident, False), decl)

-- Returns a Bool representing whether performing a search on the property would produce a counter-example
-- True = example input found will be a counter-example to the property
-- False = example input found will not be a counter-example
flipForall ::
  (MonadCompile m,
   MonadReader DeclProvenance m) =>
  ([QuantifierData], NewContextValue) ->
  m(([QuantifierData], NewContextValue), Bool)
flipForall (quantifierData, value) = case quantifierData of
  [] -> return ((quantifierData, value), True) -- is True here ok?
  (firstQuantifier, _, _) : _ -> do
    newQuantifierData <- flipForallHelper firstQuantifier quantifierData
    if firstQuantifier == Forall
      then return ((newQuantifierData, fromBoolValue $ VNot (TensorOp1Args IDimNil value)), True) -- not sure what dims to use here
      else return ((newQuantifierData, value), False)

flipForallHelper ::
  (MonadCompile m,
   MonadReader DeclProvenance m) =>
  Quantifier ->
  [QuantifierData] ->
  m[QuantifierData]
flipForallHelper prevQuantifier quantifierData = case quantifierData of
  [] -> return []
  (quantifier, dimsOrTyp, binder) : quantifiers -> 
    if prevQuantifier /= quantifier
      then do
        declProv <- ask
        throwError $ UnableToLiftQuantifiersInProperty declProv
      else do
        newData <- flipForallHelper quantifier quantifiers
        return ((Exists, dimsOrTyp, binder) : newData)

reconstructProperty ::
  (MonadCompile m,
  MonadFreeContext Builtin m,
  MonadBoundContext (Value Builtin) m)  =>
  [QuantifierData] -> 
  NewContextValue -> 
  m(Value Builtin)
reconstructProperty quantifiers value = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do  
    newBody <- addBinderToContext binder $ do
      reconstructed <- reconstructProperty qs value
      ctx <- getBoundCtx (Proxy @(Value Builtin))
      let lv = boundCtxLv ctx
      let newBody = unnormalise lv reconstructed
      return newBody
    ctx <- getBoundCtx (Proxy @(Value Builtin))
    let newEnv = boundContextToEnv ctx
    case dimsOrType of
      Left dims -> return (fromBoolValue $ VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs dims binder (Closure newEnv newBody)))
      Right typ -> return (fromBoolValue $ VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)))
    
-- Takes (expression, contextDelta) and returns (expression with quantifiers lifted, number of quantifiers in the expression)
liftQuantifierProperty :: 
  (MonadLiftQuantifiers m) =>
  (OldContextValue, Lv) -> 
  m(([QuantifierData], NewContextValue), Lv)
liftQuantifierProperty (value, ctxDelta) = case toBoolValue value of
  VBoolLiteral _ -> return (([], value), 0)
  VAnd (TensorOp2Args dims arg1 arg2) -> do
    ((quantifiers1, arg1'), ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    ((quantifiers2, arg2'), ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
    return ((quantifiers1 ++ quantifiers2, fromBoolValue $ VAnd (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2)
  VOr (TensorOp2Args dims arg1 arg2) -> do
    ((quantifiers1, arg1'), ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    ((quantifiers2, arg2'), ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta)
    return ((quantifiers1 ++ quantifiers2, fromBoolValue $ VOr (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2) 
  VNot (TensorOp1Args dims arg) -> do
    ((quantifiers, arg'), ctxSize) <- liftQuantifierProperty (arg, ctxDelta)
    return ((quantifiers, fromBoolValue $ VNot (TensorOp1Args dims arg')), ctxSize)
  VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    ((quantifiers, body'), ctxSize) <- addBinderToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    return (((quantifier, Left dims, binder) : quantifiers, body'), ctxSize + 1)
  VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
    normBody <- normaliseClosure binder closure
    ((quantifiers, body'), ctxSize) <- addBinderToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    return (((quantifier, Right typ, binder) : quantifiers, body'), ctxSize + 1)
  VBoolIf args -> do
    unfolded <- unfoldIf args
    liftQuantifierProperty (unfolded, ctxDelta)
  VReduceAndTensor _ -> do
    unblocked <- unblockBoolExpr unblockingActions value
    liftQuantifierProperty (unblocked, ctxDelta)
  VReduceOrTensor _ -> do
    unblocked <- unblockBoolExpr unblockingActions value
    liftQuantifierProperty (unblocked, ctxDelta) 
  VBoolAt _ -> do
    unblocked <- unblockBoolExpr unblockingActions value
    liftQuantifierProperty (unblocked, ctxDelta)
  VCompareIndex (op, IndexComparisonArgs size1 size2 arg1 arg2) -> do
    arg1' <- updateIndexBoundVar ctxDelta arg1
    arg2' <- updateIndexBoundVar ctxDelta arg2
    return (([], fromBoolValue $ VCompareIndex (op, IndexComparisonArgs size1 size2 arg1' arg2')), 0)
  VCompareNat (op, Op2Args arg1 arg2) -> do
    arg1' <- updateNatBoundVar ctxDelta arg1
    arg2' <- updateNatBoundVar ctxDelta arg2
    return (([], fromBoolValue $ VCompareNat (op, Op2Args arg1' arg2')), 0)
  VCompareRatTensor (op, TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar ctxDelta arg1
    arg2' <- updateRatTensorBoundVar ctxDelta arg2
    return (([], fromBoolValue $ VCompareRatTensor (op, TensorOp2Args dims arg1' arg2')), 0)

updateIndexBoundVar :: 
  (MonadLiftQuantifiers m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateIndexBoundVar lv value = case toIndexValue value of
  VIndexLiteral _ _ -> return value
  VIndexBoundVar v spine -> do
    spine' <- traverseArgs (updateIndexBoundVar lv) spine
    return $ VBoundVar (v + lv) spine'
  VIndexIf _  -> do
    declProv <- ask
    throwError $ UnableToLiftQuantifiersInProperty declProv
  VIndexAtVector (AtVectorArgs typ dim vector idx) -> do
    vector' <- updateIndexBoundVar lv vector
    return (fromIndexValue $ VIndexAtVector (AtVectorArgs typ dim vector' idx))
  VIndexParameter _ -> return value

updateNatBoundVar ::
  (MonadLiftQuantifiers m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateNatBoundVar lv value = case toNatValue value of
  VNatLiteral _ -> return value
  VNatBoundVar v spine -> do
    spine' <- traverseArgs (updateNatBoundVar lv) spine
    return (fromNatValue $ VNatBoundVar (v + lv) spine')
  VNatIf _ -> do
    declProv <- ask
    throwError $ UnableToLiftQuantifiersInProperty declProv
  VNatAdd (Op2Args arg1 arg2) -> do
    arg1' <- updateNatBoundVar lv arg1
    arg2' <- updateNatBoundVar lv arg2
    return (fromNatValue $ VNatAdd (Op2Args arg1' arg2')) 
  VNatMul (Op2Args arg1 arg2) -> do
    arg1' <- updateNatBoundVar lv arg1
    arg2' <- updateNatBoundVar lv arg2
    return (fromNatValue $ VNatMul (Op2Args arg1' arg2')) 
  VNatParameter _ -> return value

updateRatTensorBoundVar ::
  (MonadLiftQuantifiers m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateRatTensorBoundVar lv value = case toRatTensorValue value of
  VRatTensorLiteral _ -> return value
  VNegRatTensor (TensorOp1Args dims arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VNegRatTensor (TensorOp1Args dims arg'))
  VLogRatTensor (TensorOp1Args dims arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VLogRatTensor (TensorOp1Args dims arg'))
  VExpRatTensor (TensorOp1Args dims arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VExpRatTensor (TensorOp1Args dims arg'))
  VAddRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VAddRatTensor (TensorOp2Args dims arg1' arg2'))
  VSubRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VAddRatTensor (TensorOp2Args dims arg1' arg2'))
  VMulRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VMulRatTensor (TensorOp2Args dims arg1' arg2'))
  VDivRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VDivRatTensor (TensorOp2Args dims arg1' arg2'))
  VMinRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VMinRatTensor (TensorOp2Args dims arg1' arg2'))
  VMaxRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VMaxRatTensor (TensorOp2Args dims arg1' arg2'))
  VPowRatTensor (TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar lv arg1
    arg2' <- updateRatTensorBoundVar lv arg2
    return (fromRatTensorValue $ VPowRatTensor (TensorOp2Args dims arg1' arg2'))
  VReduceAddRatTensor (TensorReductionArgs dims tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceAddRatTensor (TensorReductionArgs dims tensor'))
  VReduceMulRatTensor (TensorReductionArgs dims tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMulRatTensor (TensorReductionArgs dims tensor'))
  VReduceMinRatTensor (TensorReductionArgs dims tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMinRatTensor (TensorReductionArgs dims tensor'))
  VReduceMaxRatTensor (TensorReductionArgs dims tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMaxRatTensor (TensorReductionArgs dims tensor'))
  VIfRatTensor _ -> do
    declProv <- ask
    throwError $ UnableToLiftQuantifiersInProperty declProv
  VRatTensorBoundVar v -> return (fromRatTensorValue $ VRatTensorBoundVar (v + lv))
  VRatTensorNetworkApp ident (NetworkAppArgs arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VRatTensorNetworkApp ident (NetworkAppArgs arg'))
  VRatConstTensor (ConstTensorArgs typ val dims) -> do
    val' <- updateRatTensorBoundVar lv val
    return (fromRatTensorValue $ VRatConstTensor (ConstTensorArgs typ val' dims))
  VRatStackTensor (StackTensorArgs typ firstDim restDims elems) -> do
    elems' <-  traverse (updateRatTensorBoundVar lv) elems
    return (fromRatTensorValue $ VRatStackTensor (StackTensorArgs typ firstDim restDims elems'))
  VRatAtTensor (AtTensorArgs typ firstDim restDims tensor idx) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VRatAtTensor (AtTensorArgs typ firstDim restDims tensor' idx))
  VRatForeach (ForeachTensorArgs typ firstDim restDims fn) -> do
    fn' <- updateRatTensorBoundVar lv fn
    return (fromRatTensorValue $ VRatForeach (ForeachTensorArgs typ firstDim restDims fn'))
  VRatRecordAcc typ val fieldName spine -> do
    val' <- updateRatTensorBoundVar lv val
    spine' <- traverseArgs (updateRatTensorBoundVar lv) spine
    return (fromRatTensorValue $ VRatRecordAcc typ val' fieldName spine')
  VDatasetOrParameter _ -> return value
  VRatAtVector (AtVectorArgs typ dim vector idx) -> do
    vector' <- updateRatTensorBoundVar lv vector
    return (fromRatTensorValue $ VRatAtVector (AtVectorArgs typ dim vector' idx))
  
unblockingActions :: (MonadLiftQuantifiers m) => UnblockingActions m
unblockingActions =
  UnblockingActions {
    unblockRatTensorBoundVar = \_ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv,
    unblockRecordBoundVar = \_ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv,
    unblockNetworkApp = \_ _ _ _-> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv,
    unblockDatasetOrParameter = \_ -> do
      declProv <- ask
      throwError $ UnableToLiftQuantifiersInProperty declProv
  }

{-showValue :: (MonadLogger m, MonadBoundContext (Value Builtin) m) => (OldContextValue, Lv) -> m(([QuantifierData], NewContextValue), Lv) -> m(([QuantifierData], NewContextValue), Lv)
showValue (value, lv) resultFunction = do
  ctx <- getBoundCtx (Proxy @(Value Builtin))
  logDebug MinDetail $ "lift-enter" <+> prettyVerbose value <+> pretty lv <+> prettyVerbose ctx
  incrCallDepth
  ((quantifiers, newValue), newLv) <- resultFunction
  decrCallDepth
  logDebug MinDetail $ "lift-exit" <+> prettyVerbose newValue <+> pretty newLv <+> prettyVerbose ctx
  return ((quantifiers, newValue), newLv)-}