-- add function which takes whole Prog
-- extract decls from Main, pass decls (runFreshFreeContextT here)
-- put in new file!
module Vehicle.Backend.Loss.LiftQuantifier
  ( liftQuantifierDecls,
    liftQuantifierDecl,
    liftQuantifier,
	liftForall,
	liftExists,
  )
where

import Vehicle.Data.Variable.Bound.Context.Name (MonadNameContext, runFreshNameBoundContextT)
import Vehicle.Data.Variable.Free.Context (MonadFreeContext, runFreshFreeContextT)
import Vehicle.Data.Code.TypedView
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Code.Value (Closure (..), Value (..))
import Vehicle.Compile.Normalise.NBE (normaliseClosure)

liftQuantifierDecls :: 
  (MonadCompile m, MonadFreeContext Builtin m) => 
  [Decl Builtin] ->
  m[Decl Builtin]
liftQuantifiers = \case
  [] -> return []
  decl : decls -> do
    (decl', _) <- liftQuantifier (decl, 0)
    decls' <- liftQuantifiers decls --addDeclEntryToContext
    return (decl' : decls')

liftQuantifierDecl ::
  (MonadCompile m, MonadFreeContext Builtin m) => 
  Decl Builtin ->
  m(Decl Builtin)
liftQuantifierDecl = \case
  DefFunction _ _ anns _ body -> 
    -- call isAnnotatedAsProperty to check if the thing is a property (only lift quantifiers for properties)
    -- runFreshNameContext 
    -- eval body with empty context (this will change decl into value builtin)
    liftQuantifier body

-- using contextDelta and contextSize
liftQuantifier :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  (Value Builtin, Lv) -> 
  m(Value Builtin, Lv)
liftQuantifier (expr, ctxDelta) = case toBoolTensorValue expr of -- use toBoolValue (less operator types)
  
  VBoolTensorAnd (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifier (arg1, ctxDelta)
    liftForall arg1' $ \arg1'' -> do
      liftExists arg1'' $ \arg1''' -> do
        (arg2', ctxSize2) <- liftQuantifier (arg2, ctxDelta + ctxSize1)
        liftForall arg2' $ \arg2'' -> do
          liftExists arg2'' $ \arg2''' -> do
            return (fromBoolTensorValue (VBoolTensorAnd (TensorOp2Args dims arg1''' arg2''')), ctxSize1 + ctxSize2)
  
  VBoolTensorOr (TensorOp2Args dims arg1 arg2) -> do
    (arg1', ctxSize1) <- liftQuantifier (arg1, ctxDelta)
    (arg2', ctxSize2) <- liftQuantifier (arg2, ctxDelta)
    return (fromBoolTensorValue (VBoolTensorOr (TensorOp2Args dims arg1' arg2')), ctxSize1 + ctxSize2) 
  
  VBoolTensorNot (TensorOp1Args dims arg) -> do
    (arg', ctxSize) <- liftQuantifier (arg, ctxDelta)
    return (fromBoolTensorValue (VBoolTensorNot (TensorOp1Args dims arg')), ctxSize)
  
  VBoolTensorQuantifyRat (Forall, QuantifyRatTensorArgs dims binder (Closure boundEnv bodyExpr)) -> do
    normBody <- normaliseClosure binder (Closure boundEnv bodyExpr)
    -- add binder to NameContext using addNameToContext
    (body', ctxSize) <- liftQuantifier (normBody, ctxDelta)
    -- call boundContextToEnv, give it name context from MonadReadableNameContext (for reforming Closure)
    return (fromBoolTensorValue (VBoolTensorQuantifyRat (Forall, QuantifyRatTensorArgs dims binder (Closure boundEnv body'))), ctxSize + 1)

  VBoolTensorQuantifyRat (Exists, QuantifyRatTensorArgs dims binder (Closure boundEnv bodyExpr)) -> do
    normBody <- normaliseClosure binder (Closure boundEnv bodyExpr)
    (body', ctxSize) <- liftQuantifier (normBody, ctxDelta)
    return (fromBoolTensorValue (VBoolTensorQuantifyRat (Exists, QuantifyRatTensorArgs dims binder (Closure boundEnv body'))), ctxSize + 1)

  -- VBoolIf --> unfoldIf, then call liftQuantifier on result
  -- reduceAnd, reduceOr, At --> call unblockBoolExpr (or just call developerError when pattern matching on these)

  e -> return (fromBoolTensorValue e, ctxDelta)

liftForall :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftForall expr k = case toBoolTensorValue expr of
  VBoolTensorQuantifyRat (Forall, QuantifyRatTensorArgs dims binder (Closure boundEnv bodyExpr)) -> do
    (body', ctxSize') <- do
      -- let newEnv = extendEnvWithBound ctxSize binder boundEnv (do I have to do this?)
      body <- normaliseClosure binder (Closure boundEnv bodyExpr)
      liftForall body k
    return (fromBoolTensorValue (VBoolTensorQuantifyRat (Forall, QuantifyRatTensorArgs dims binder (Closure boundEnv body'))), ctxSize') -- I don't know if rebuilding this is correct
  e -> k $ fromBoolTensorValue e

liftExists :: 
  (MonadCompile m, MonadFreeContext Builtin m, MonadNameContext m) =>
  Value Builtin -> 
  (Value Builtin -> m(Value Builtin, Lv)) -> 
  m(Value Builtin, Lv)
liftExists expr k = case toBoolTensorValue expr of
  VBoolTensorQuantifyRat (Exists, QuantifyRatTensorArgs dims binder (Closure boundEnv bodyExpr)) -> do
    (body', ctxSize') <- do
      -- let newEnv = extendEnvWithBound ctxSize binder boundEnv (do I have to do this?)
      body <- normaliseClosure binder (Closure boundEnv bodyExpr)
      liftExists body k
    return (fromBoolTensorValue (VBoolTensorQuantifyRat (Exists, QuantifyRatTensorArgs dims binder (Closure boundEnv body'))), ctxSize') -- I don't know if rebuilding this is correct
  e -> k $ fromBoolTensorValue e
