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

liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m([(Identifier, Bool)], Prog Builtin)
liftQuantifiers (Main ds) = 
  runFreshFreeContextT (Proxy @Builtin) $ do
    (declsData, ds') <- liftQuantifierDecls ds
    return (declsData, Main ds')

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
  DefRecord _ ident _ _ _ _ -> return ((ident, False), decl)

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
    newQuantifierData <- checkAlternatingQuantifiers firstQuantifier quantifierData
    if firstQuantifier == Forall
      then return ((newQuantifierData, fromBoolValue $ VNot (TensorOp1Args IDimNil value)), True) -- not sure what dims to use here
      else return ((newQuantifierData, value), False)

-- Throws an error if there are alternating quantifiers in the property and makes all quantifiers existential
checkAlternatingQuantifiers ::
  (MonadCompile m,
   MonadReader DeclProvenance m) =>
  Quantifier ->
  [QuantifierData] ->
  m[QuantifierData]
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
  VCompareNat (op, args) -> do
    args' <- traverseOp2Args (updateNatBoundVar ctxDelta) args
    return (([], fromBoolValue $ VCompareNat (op, args')), 0)
  VCompareRatTensor (op, args) -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar ctxDelta) args
    return (([], fromBoolValue $ VCompareRatTensor (op, args')), 0)

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
  VNatAdd args -> do
    args' <- traverseOp2Args (updateNatBoundVar lv) args
    return (fromNatValue $ VNatAdd args') 
  VNatMul args -> do
    args' <- traverseOp2Args (updateNatBoundVar lv) args
    return (fromNatValue $ VNatMul args') 
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
  VAddRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VAddRatTensor args')
  VSubRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VSubRatTensor args')
  VMulRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VMulRatTensor args')
  VDivRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VDivRatTensor args')
  VMinRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VMinRatTensor args')
  VMaxRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VMaxRatTensor args')
  VPowRatTensor args -> do
    args' <- traverseTensorOp2Args (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VPowRatTensor args')
  VReduceAddRatTensor args -> do
    args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VReduceAddRatTensor args')
  VReduceMulRatTensor args -> do
    args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VReduceMulRatTensor args')
  VReduceMinRatTensor args -> do
    args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VReduceMinRatTensor args')
  VReduceMaxRatTensor args -> do
    args' <- traverseReductionArgs (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VReduceMaxRatTensor args')
  VIfRatTensor _ -> do
    declProv <- ask
    throwError $ UnableToLiftQuantifiersInProperty declProv
  VRatTensorBoundVar v -> return (fromRatTensorValue $ VRatTensorBoundVar (v + lv))
  VRatTensorNetworkApp ident (NetworkAppArgs arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VRatTensorNetworkApp ident (NetworkAppArgs arg'))
  VRatConstTensor args -> do
    args' <- traverseConstTensorValue (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VRatConstTensor args')
  VRatStackTensor args -> do
    args' <-  traverseStackTensorElements (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VRatStackTensor args')
  VRatAtTensor args -> do
    args' <- traverseAtTensorArg (updateRatTensorBoundVar lv) args
    return (fromRatTensorValue $ VRatAtTensor args')
  VRatForeach (ForeachTensorArgs typ d ds fn) -> do
    fn' <- updateRatTensorBoundVar lv fn
    return (fromRatTensorValue $ VRatForeach (ForeachTensorArgs typ d ds fn'))
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