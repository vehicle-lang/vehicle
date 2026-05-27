module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifiers,
    liftQuantifierDecls,
    liftQuantifierDecl,
    liftQuantifierProperty,
    liftForall,
    liftExists,
  )
where

import Data.Proxy (Proxy (..))
import Control.Monad.Except (MonadError (..))
import Vehicle.Prelude
import Vehicle.Data.Variable.Bound.Context.Generic (MonadBoundContext (addBinderToContext, getBoundCtx), runFreshBoundContextT, addBinderToContext)
import Vehicle.Data.Variable.Bound.Context.Core (boundCtxLv)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, runFreshFreeContextT, addDeclEntryToContext)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Interface
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.AST.Expr.Scoped (Decl)
import Vehicle.Data.AST.Prog (GenericProg(Main), Prog)
import Vehicle.Data.Code.Value (Closure (..), Value (..), VDecl, boundContextToEnv)
import Vehicle.Data.Variable.Bound.Level (Lv)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseClosure, evalDecl)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.LiftIf (unfoldIf)
import Vehicle.Compile.Unblock (UnblockingActions (..), unblockBoolExpr)
import Data.IDX (idxDimensions)

liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m(Prog Builtin)
liftQuantifiers (Main ds) = 
  runFreshFreeContextT (Proxy @Builtin) $ do 
    Main <$> liftQuantifierDecls ds

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
liftQuantifierDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) => 
  VDecl Builtin ->
  m(VDecl Builtin)
liftQuantifierDecl decl = case decl of
  DefAbstract {} -> return decl
  DefFunction p ident ann typ expr -> 
    if isAnnotatedAsProperty ann
      then do
        (liftedValue, _) <- 
          catchError
          (runFreshBoundContextT (Proxy @(Value Builtin)) $ liftQuantifierProperty (expr, 0))
          (
            \case
              UnableToUpdateBoundVars -> throwError $ UnableToLiftQuantifiersInProperty (ident, p)
              other -> throwError other
          )
        return $ DefFunction p ident ann typ liftedValue
      else return decl
  DefRecord {} -> return decl

-- using contextDelta and contextSize
liftQuantifierProperty :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadBoundContext (Value Builtin) m) =>
  (Value Builtin, Lv) -> 
  m(Value Builtin, Lv)
liftQuantifierProperty (value, ctxDelta) = case toBoolValue value of
  VBoolLiteral _ -> return (value, ctxDelta)
  VAnd (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    liftForall arg1' $ \arg1'' -> do
      liftExists arg1'' $ \arg1''' -> do
        (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
        liftForall arg2' $ \arg2'' -> do
          liftExists arg2'' $ \arg2''' -> do
            return (fromBoolValue $ VAnd (TensorOp2Args dims arg1''' arg2'''), ctxSize1 + ctxSize2)
  VOr (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta)
    return (fromBoolValue $ VOr (TensorOp2Args dims arg1' arg2'), ctxSize1 + ctxSize2) 
  VNot (TensorOp1Args dims arg) -> do
    (arg', ctxSize) <- liftQuantifierProperty (arg, ctxDelta)
    return (fromBoolValue $ VNot (TensorOp1Args dims arg'), ctxSize)
  VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addBinderToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    ctx <- getBoundCtx (Proxy @(Value Builtin))
    let newEnv = boundContextToEnv ctx
    let lv = boundCtxLv ctx
    let newBody = unnormalise (lv + 1) body'
    return (fromBoolValue $ VQuantifyRatTensor (quantifier, QuantifyRatTensorArgs dims binder (Closure newEnv newBody)), ctxSize + 1)
  VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addBinderToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    ctx <- getBoundCtx (Proxy @(Value Builtin))
    let newEnv = boundContextToEnv ctx
    let lv = boundCtxLv ctx
    let newBody = unnormalise (lv + 1) body'
    return (fromBoolValue $ VQuantifyRecord (quantifier, QuantifyRecordArgs typ binder (Closure newEnv newBody)), ctxSize + 1)
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
  VCompareIndex (op, IndexCompArgs size1 size2 arg1 arg2) -> do
    arg1' <- updateIndexBoundVar ctxDelta arg1
    arg2' <- updateIndexBoundVar ctxDelta arg2
    return (fromBoolValue $ VCompareIndex (op, IndexCompArgs size1 size2 arg1' arg2'), 0)
  VCompareNat (op, Op2Args arg1 arg2) -> do
    arg1' <- updateNatBoundVar ctxDelta arg1
    arg2' <- updateNatBoundVar ctxDelta arg2
    return (fromBoolValue $ VCompareNat (op, Op2Args arg1' arg2'), 0)
  VCompareRatTensor (op, TensorOp2Args dims arg1 arg2) -> do
    arg1' <- updateRatTensorBoundVar ctxDelta arg1
    arg2' <- updateRatTensorBoundVar ctxDelta arg2
    return (fromBoolValue $ VCompareRatTensor (op, TensorOp2Args dims arg1' arg2'), 0)

liftForall :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadBoundContext (Value Builtin) m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftForall expr k = case toBoolValue expr of
  VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addBinderToContext binder $ liftForall normBody k
    ctx <- getBoundCtx (Proxy @(Value Builtin))
    let newEnv = boundContextToEnv ctx
    let lv = boundCtxLv ctx
    let newBody = unnormalise (lv + 1) body'
    return (fromBoolValue $ VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder (Closure newEnv newBody)), ctxSize)
  _ -> k expr

liftExists :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadBoundContext (Value Builtin) m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftExists expr k = case toBoolValue expr of
  VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addBinderToContext binder $ liftExists normBody k
    ctx <- getBoundCtx (Proxy @(Value Builtin))
    let newEnv = boundContextToEnv ctx
    let lv = boundCtxLv ctx
    let newBody = unnormalise (lv + 1) body'
    return (fromBoolValue $ VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder (Closure newEnv newBody)), ctxSize)
  _ -> k expr

updateIndexBoundVar :: 
  (MonadCompile m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateIndexBoundVar lv value = case toIndexValue value of
  VIndexLiteral _ _ -> return value
  VIndexBoundVar v spine -> return $ VBoundVar (v + lv) spine
  VIndexIf _  -> throwError UnableToUpdateBoundVars

updateNatBoundVar ::
  (MonadCompile m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateNatBoundVar lv value = case toNatValue value of
  VNatLiteral _ -> return value
  VNatBoundVar v spine -> return (fromNatValue $ VNatBoundVar (v + lv) spine)
  VNatIf _ -> throwError UnableToUpdateBoundVars
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
  (MonadCompile m) =>
  Lv -> 
  Value Builtin -> 
  m(Value Builtin)
updateRatTensorBoundVar lv value = case toRatTensorValue value of
  VRatTensorLiteral _ -> return value
  VNegRatTensor (TensorOp1Args dims arg) -> do
    arg' <- updateRatTensorBoundVar lv arg
    return (fromRatTensorValue $ VNegRatTensor (TensorOp1Args dims arg'))
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
  VReduceAddRatTensor (TensorReductionArgs dims unit tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceAddRatTensor (TensorReductionArgs dims unit tensor'))
  VReduceMulRatTensor (TensorReductionArgs dims unit tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMulRatTensor (TensorReductionArgs dims unit tensor'))
  VReduceMinRatTensor (TensorReductionArgs dims unit tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMinRatTensor (TensorReductionArgs dims unit tensor'))
  VReduceMaxRatTensor (TensorReductionArgs dims unit tensor) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    return (fromRatTensorValue $ VReduceMaxRatTensor (TensorReductionArgs dims unit tensor'))
  VIfRatTensor _ -> throwError UnableToUpdateBoundVars
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
  VRatAt (AtTensorArgs typ firstDim restDims tensor idx) -> do
    tensor' <- updateRatTensorBoundVar lv tensor
    idx' <- updateRatTensorBoundVar lv idx
    return (fromRatTensorValue $ VRatAt (AtTensorArgs typ firstDim restDims tensor' idx'))
  VRatForeach (ForeachTensorArgs typ firstDim restDims fn) -> do
    fn' <- updateRatTensorBoundVar lv fn
    return (fromRatTensorValue $ VRatForeach (ForeachTensorArgs typ firstDim restDims fn'))
  VRatRecordAcc typ val fieldName spine -> do
    val' <- updateRatTensorBoundVar lv val
    return (fromRatTensorValue $ VRatRecordAcc typ val' fieldName spine)
  VDatasetOrParameter _ -> return value
  
-- I don't know what each of these should do
-- To throw UnableToLiftQuantifiersInProperty error, I need to provide identifier and provenance
unblockingActions :: (MonadCompile m) => UnblockingActions m
unblockingActions =
  UnblockingActions {
    unblockRatTensorBoundVar = developerError "Tensor error",
    unblockRecordBoundVar = developerError "Record error",
    unblockNetworkApp = \_ _ _ -> developerError "Network error",
    unblockDatasetOrParameter = developerError "Dataset error"
  }