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
import Vehicle.Compile.Print (prettyVerbose, prettyFriendlyEmptyCtx)
import Vehicle.Data.Variable.Bound.Context.Name (prettyFriendlyInCtx)

type MonadLiftQuantifiers m = 
  ( MonadCompile m,
    MonadFreeContext Builtin m,
    MonadBoundContext (Value Builtin) m,
    MonadReader DeclProvenance m
  )

-- I think this should return the Prog containing the list of reconstructed decls plus [(propertyName, Bool)]
liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m(Prog Builtin)
liftQuantifiers (Main ds) = 
  runFreshFreeContextT (Proxy @Builtin) $ do 
    Main <$> liftQuantifierDecls ds

-- Possibly return [(propertyName, Bool)] in addition to the list of reconstructed decls
liftQuantifierDecls :: 
  (MonadCompile m, MonadFreeContext Builtin m) => 
  [Decl Builtin] ->
  m[Decl Builtin]
liftQuantifierDecls = \case
  [] -> return []
  decl : decls -> do
    normDecl <- evalDecl decl
    normDecl' <- liftQuantifierDecl normDecl
    let decl' = fmap (unnormalise 0) normDecl'
    decls' <- addDeclEntryToContext normDecl $ liftQuantifierDecls decls 
    return (decl':decls')

-- accepts and returns normalised decl
-- Possibly return (propertyName, Bool) in addition to the reconstructed decl representing whether it contains all forall or all exists
liftQuantifierDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) => 
  VDecl Builtin ->
  m(VDecl Builtin)
liftQuantifierDecl decl = case decl of
  DefAbstract {} -> return decl
  DefFunction p ident ann typ expr -> 
    if isAnnotatedAsProperty ann
      then do
        ((quantifiers, newValue), _) <- runFreshBoundContextT (Proxy @(Value Builtin)) $ runReaderT (liftQuantifierProperty (expr, 0)) (ident, p)
        -- call flipUniversal here
        liftedValue <- runFreshBoundContextT (Proxy @(Value Builtin)) $ reconstructExpr quantifiers newValue
        logDebug MinDetail $ prettyFriendlyEmptyCtx liftedValue
        -- _ <- developerError "hi"
        return $ DefFunction p ident ann typ liftedValue
      else return decl
  DefRecord {} -> return decl

type OldContextValue = Value Builtin
type NewContextValue = Value Builtin
type QuantifierData = (Quantifier, Either (VDims (Builtin)) (VType (Builtin)), VBinder (Builtin))

{- 
This function should translate forall x. forall y. P(x, y) to ---> not (exists x. exists y. not P(x, y))
While doing this, also check whether the property has alternating quantifiers and throw an error if this is the case
i.e. this function takes the last quantifier that we have seen, and the output of liftQuantifierProperty
We need to traverse the list of quantifier data outputted by liftQuantifierProperty and remember the last quantifier that we have seen
If the next quantifier is different, that means we have alternating quantifiers so throw an error!
If we get to the end of the list and there are no alternating quantifiers, check if the last quantifier seen is a forall, 
  if so put a negation in front of NewContextValue
When we encounter a forall in the list, we need to replace it with an exists in the list but keep the forall as the last quantifier that we have seen
This returns the same type as liftQuantifierProperty (except NewContextValue may have a negation in front of it) plus a Bool representing whether the 
property contains all forall or exists

flipUniversal ::
  (MonadCompile m) =>
  (Maybe Quantifier) ->
  ([QuantifierData], NewContextValue) ->
  m(([QuantifierData], NewContextValue), Bool)
flipUniversal = _
-}

reconstructExpr ::
  (MonadCompile m,
  MonadFreeContext Builtin m,
  MonadBoundContext (Value Builtin) m)  =>
  [QuantifierData] -> 
  NewContextValue -> 
  m(Value Builtin)
reconstructExpr quantifiers value = case quantifiers of
  [] -> return value
  (quantifier, dimsOrType, binder) : qs -> do 
    newBody <- addBinderToContext binder $ do
      reconstructed <- reconstructExpr qs value
      ctx <- getBoundCtx (Proxy @(Value Builtin))
      let lv = boundCtxLv ctx
      let newBody = unnormalise lv reconstructed
      logDebug MinDetail $ "newBody" <+> prettyVerbose newBody
      logDebugM MinDetail $ prettyFriendlyInCtx newBody
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
liftQuantifierProperty (value, ctxDelta) = showValue (value, ctxDelta) $ case toBoolValue value of
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
  -- how to convert back to Value Builtin for VIndexAtVector
  VIndexAtVector _ -> return value
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

showValue :: (MonadLogger m, MonadBoundContext (Value Builtin) m) => (OldContextValue, Lv) -> m(([QuantifierData], NewContextValue), Lv) -> m(([QuantifierData], NewContextValue), Lv)
showValue (value, lv) resultFunction = do
  ctx <- getBoundCtx (Proxy @(Value Builtin))
  logDebug MinDetail $ "lift-enter" <+> prettyVerbose value <+> pretty lv <+> prettyVerbose ctx
  incrCallDepth
  ((quantifiers, newValue), newLv) <- resultFunction
  decrCallDepth
  logDebug MinDetail $ "lift-exit" <+> prettyVerbose newValue <+> pretty newLv <+> prettyVerbose ctx
  return ((quantifiers, newValue), newLv)