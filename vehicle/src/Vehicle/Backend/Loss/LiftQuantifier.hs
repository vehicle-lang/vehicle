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
import Vehicle.Prelude
import Vehicle.Data.Variable.Bound.Context.Name (MonadNameContext, runFreshNameBoundContextT, addNameToContext, getNameContext)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, runFreshFreeContextT, addDeclEntryToContext)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Interface.Args
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

liftQuantifiers ::
  (MonadCompile m) =>
  Prog Builtin ->
  m(Prog Builtin)
liftQuantifiers prog@(Main ds) = 
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
    decl' <- liftQuantifierDecl normDecl
    decls' <- addDeclEntryToContext normDecl $ liftQuantifierDecls decls 
    return (decl':decls')

-- accepts normalised decl and returns it unnormalised
liftQuantifierDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) => 
  VDecl Builtin ->
  m(Decl Builtin)
liftQuantifierDecl decl = case decl of
  DefAbstract {} -> unnormalise 0 decl
  DefFunction p ident ann typ expr -> 
    if isAnnotatedAsProperty ann
      then do
        (liftedValue, _) <- runFreshNameBoundContextT $ liftQuantifierProperty (expr, 0) -- how to go from Value Builtin to Decl Builtin
        let liftedTyp = unnormalise 0 typ
        let liftedExpr = unnormalise 0 liftedValue
        return $ DefFunction p ident ann liftedTyp liftedExpr -- is this ok?
      else return $ unnormalise 0 decl
  DefRecord {} -> unnormalise 0 decl

-- using contextDelta and contextSize
liftQuantifierProperty :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  (Value Builtin, Lv) -> 
  m(Value Builtin, Lv)
liftQuantifierProperty (expr, ctxDelta) = case toBoolValue expr of
  
  VAnd (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    liftForall arg1' $ \arg1'' -> do
      liftExists arg1'' $ \arg1''' -> do
        (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta + ctxSize1)
        liftForall arg2' $ \arg2'' -> do
          liftExists arg2'' $ \arg2''' -> do
            return (fromBoolValue (VAnd (TensorOp2Args dims arg1''' arg2''')), ctxSize1 + ctxSize2)
  
  VOr (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifierProperty (arg1, ctxDelta)
    (arg2', ctxSize2) <- liftQuantifierProperty (arg2, ctxDelta)
    return (fromBoolValue (VOr (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2) 
  
  VNot (TensorOp1Args dims arg) -> do
    (arg', ctxSize) <- liftQuantifierProperty (arg, ctxDelta)
    return (fromBoolValue (VNot (TensorOp1Args dims arg')), ctxSize)
  
  VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder closure) -> do -- use what is the difference with VQuantifyRecord? 
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    let newEnv = boundContextToEnv getNameContext
    let newBody = unnormalise 0 body'
    return (fromBoolValue (VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder (Closure newEnv newBody))), ctxSize + 1)

  -- collapse into same thing as Forall
  VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addNameToContext binder $ liftQuantifierProperty (normBody, ctxDelta)
    let newEnv = boundContextToEnv getNameContext
    let newBody = unnormalise 0 body'
    return (fromBoolValue (VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder (Closure newEnv newBody))), ctxSize + 1)

  -- VBoolIf --> unfoldIf, then call liftQuantifier on result
  VBoolIf args -> do
    unfolded <- unfoldIf args
    liftQuantifierProperty (unfolded, ctxDelta)
  
  -- reduceAnd, reduceOr, At --> call unblockBoolExpr (or just call developerError when pattern matching on these) for now
  VReduceAndTensor args -> do
    unblocked <- unblockBoolExpr tempUnblockingActions (fromBoolValue (VReduceAndTensor args))
    liftQuantifierProperty (unblocked, ctxDelta)

  VReduceOrTensor args -> do
    unblocked <- unblockBoolExpr tempUnblockingActions (fromBoolValue (VReduceOrTensor args))
    liftQuantifierProperty (unblocked, ctxDelta) 

  VBoolAt args -> do
    unblocked <- unblockBoolExpr tempUnblockingActions (fromBoolValue (VBoolAt args))
    liftQuantifierProperty (unblocked, ctxDelta)

  e -> return (fromBoolValue e, ctxDelta)

liftForall :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftForall expr k = case toBoolValue expr of
  VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addNameToContext binder $ liftForall normBody k
    let newEnv = boundContextToEnv getNameContext
    let newBody = unnormalise 0 body'
    return (fromBoolValue (VQuantifyRatTensor (Forall, QuantifyRatTensorArgs dims binder (Closure newEnv newBody))), ctxSize)
  e -> k $ fromBoolValue e

liftExists :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftExists expr k = case toBoolValue expr of
  VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder closure) -> do
    normBody <- normaliseClosure binder closure
    (body', ctxSize) <- addNameToContext binder $ liftExists normBody k
    let newEnv = boundContextToEnv getNameContext
    let newBody = unnormalise 0 body'
    return (fromBoolValue (VQuantifyRatTensor (Exists, QuantifyRatTensorArgs dims binder (Closure newEnv newBody))), ctxSize)
  e -> k $ fromBoolValue e

tempUnblockingActions :: (MonadCompile m) => UnblockingActions m
tempUnblockingActions =
  UnblockingActions {
    unblockRatTensorBoundVar = developerError "Tensor error",
    unblockRecordBoundVar = developerError "Record error",
    unblockNetworkApp = developerError "Network error"
  }