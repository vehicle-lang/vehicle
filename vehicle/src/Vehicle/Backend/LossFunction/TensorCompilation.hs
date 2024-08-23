{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Vehicle.Backend.LossFunction.TensorCompilation
  ( convertExpr,
    runMonadTensorT,
    evalTensorBuiltinApp,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), void)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Backend.LossFunction.Core (pattern VLam2)
import Vehicle.Compile.Arity (Arity)
import Vehicle.Compile.Context.Bound
import Vehicle.Compile.Context.Free.Class (MonadFreeContext, getFreeEnv)
import Vehicle.Compile.Context.Name (MonadNameContext, addNameToContext, getBinderDepth, getNameContext)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin (evalTensorBuiltin, filterOutIrrelevantArgs)
import Vehicle.Compile.Normalise.NBE (normaliseInEnv, traverseClosure)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrettyFriendly, PrettyVerbose, prettyFriendly, prettyVerbose)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions (StdLibFunction (StdForeachIndex), pattern TensorIdent)

--------------------------------------------------------------------------------
-- Monad

type MonadTensorCtx = DeclProvenance

type MonadTensor m =
  ( MonadCompile m,
    MonadReader MonadTensorCtx m,
    MonadFreeContext Builtin m,
    MonadNameContext m
  )

runMonadTensorT ::
  (MonadCompile m) =>
  DeclProvenance ->
  ReaderT MonadTensorCtx m a ->
  m a
runMonadTensorT = flip runReaderT

getDeclProvenance :: (MonadTensor m) => m DeclProvenance
getDeclProvenance = ask

--------------------------------------------------------------------------------
-- Conversion from loss expressions

convertExpr :: (MonadTensor m) => WHNFBoundEnv Builtin -> Expr Builtin -> m (WHNFValue TensorBuiltin)
convertExpr env expr = do
  convertValue =<< normaliseInEnv env expr

convertValue :: forall m. (MonadTensor m) => WHNFValue Builtin -> m (WHNFValue TensorBuiltin)
convertValue = go
  where
    go :: WHNFValue Builtin -> m (WHNFValue TensorBuiltin)
    go e = do
      showEntry "tensor-enter" e
      result <- case e of
        VMeta {} ->
          unexpectedExprError currentPass "VMeta"
        VUniverse l ->
          return $ VUniverse l
        VFreeVar v spine
          | v == identifierOf StdForeachIndex -> convertForeachIndex spine
          | otherwise -> VFreeVar v <$> traverseArgs go spine
        VBoundVar v spine -> do
          VBoundVar v <$> traverseArgs go spine
        VBuiltin b spine ->
          convertBuiltinToTensors b spine
        VPi binder body -> do
          binder' <- traverse go binder
          body' <- addBinderToContext (void binder) $ go body
          return $ VPi binder' body'
        VLam binder closure -> do
          binder' <- traverse go binder
          freeEnv <- getFreeEnv
          closure' <- traverseClosure convertValue freeEnv binder closure
          return $ VLam binder' closure'
      showExit "tensor-exit" result
      return result

convertBuiltinToTensors :: (MonadTensor m) => Builtin -> WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertBuiltinToTensors b args = case b of
  BuiltinType t -> convertBuiltinType t =<< traverseSpine convertValue args
  BuiltinConstructor c -> convertBuiltinConstructor c =<< traverseSpine convertValue args
  BuiltinFunction f -> convertBuiltinFunction f args
  TypeClass {} -> unexpectedExprError currentPass "TypeClass"
  TypeClassOp {} -> unexpectedExprError currentPass "TypeClassOp"
  NatInDomainConstraint -> unexpectedExprError currentPass "NatInDomainConstraint"

convertBuiltinType :: (MonadTensor m) => BuiltinType -> WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertBuiltinType t args = case t of
  Unit -> unexpectedExprError currentPass "Unit"
  Nat -> return $ IDimensionTypeOp DimensionType []
  Index -> convertIndexType args
  List -> unsupportedOperation t
  Bool -> return $ ITensorType (explicit IBoolElementType) (explicit IDimNil)
  Rat -> return $ ITensorType (explicit IRatElementType) (explicit IDimNil)
  Vector -> convertVectorType args

convertBuiltinConstructor :: (MonadTensor m) => BuiltinConstructor -> WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertBuiltinConstructor c args = case c of
  LUnit -> unexpectedExprError currentPass "LUnit"
  Nil -> unsupportedOperation c
  Cons -> unsupportedOperation c
  LIndex n -> return $ IDimensionDataOp (DimensionIndex n) []
  LNat n -> return $ IDimensionDataOp (Dimension n) []
  LBool v -> return $ IBoolConstTensor v (explicit IDimNil)
  LRat v -> return $ IRatConstTensor v (explicit IDimNil)
  LVec {} -> convertVector args

convertBuiltinFunction :: (MonadTensor m) => BuiltinFunction -> WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertBuiltinFunction b args = do
  let tensorArgs = traverseSpine convertValue args
  let prependSize0 = (Arg mempty (Implicit True) Irrelevant IDimNil :)
  case b of
    FromNat {} -> unexpectedExprError currentPass "FromNat"
    FromRat {} -> unexpectedExprError currentPass "FromRat"
    Implies -> unexpectedExprError currentPass "Implies"
    ------------------------
    -- Boolean operations --
    ------------------------
    Not -> IBoolTensorOp NotBoolTensor . prependSize0 <$> tensorArgs
    And -> IBoolTensorOp AndBoolTensor . prependSize0 <$> tensorArgs
    Or -> IBoolTensorOp OrBoolTensor . prependSize0 <$> tensorArgs
    Quantifier q -> IBoolTensorOp (QuantifyRatTensor q) . prependSize0 <$> tensorArgs
    Equals EqRat op -> IBoolTensorOp (EqualsRatTensor op) . prependSize0 <$> tensorArgs
    Order OrderRat op -> IBoolTensorOp (OrderRatTensor op) . prependSize0 <$> tensorArgs
    If -> unsupportedOperation b
    -----------------------
    -- Index operations --
    -----------------------
    Equals EqIndex Eq -> unsupportedOperation b
    Equals EqIndex Neq -> unsupportedOperation b
    Order OrderIndex Le -> unsupportedOperation b
    Order OrderIndex Lt -> unsupportedOperation b
    Order OrderIndex Ge -> unsupportedOperation b
    Order OrderIndex Gt -> unsupportedOperation b
    ------------------------
    -- Natural operations --
    ------------------------
    Add AddNat -> unsupportedOperation b
    Mul MulNat -> unsupportedOperation b
    Equals EqNat Eq -> unsupportedOperation b
    Equals EqNat Neq -> unsupportedOperation b
    Order OrderNat Le -> unsupportedOperation b
    Order OrderNat Lt -> unsupportedOperation b
    Order OrderNat Ge -> unsupportedOperation b
    Order OrderNat Gt -> unsupportedOperation b
    ----------------------
    -- Rational operations --
    ----------------------
    Neg NegRat -> IRatTensorOp NegRatTensor . prependSize0 <$> tensorArgs
    Add AddRat -> IRatTensorOp AddRatTensor . prependSize0 <$> tensorArgs
    Sub SubRat -> IRatTensorOp SubRatTensor . prependSize0 <$> tensorArgs
    Mul MulRat -> IRatTensorOp MulRatTensor . prependSize0 <$> tensorArgs
    Div DivRat -> IRatTensorOp DivRatTensor . prependSize0 <$> tensorArgs
    MinRat -> IRatTensorOp MinRatTensor . prependSize0 <$> tensorArgs
    MaxRat -> IRatTensorOp MaxRatTensor . prependSize0 <$> tensorArgs
    PowRat -> unsupportedOperation b
    -----------------------
    -- Vector operations --
    -----------------------
    FoldVector -> convertFoldVector args
    MapVector -> convertMapVector args
    ZipWithVector -> convertZipWith args
    Indices -> unsupportedOperation b
    At -> convertAt =<< tensorArgs
    ---------------------
    -- List operations --
    ---------------------
    FoldList -> unsupportedOperation b
    MapList -> unsupportedOperation b

unsupportedOperation :: (Pretty builtin, MonadTensor m) => builtin -> m a
unsupportedOperation t = do
  declProv <- getDeclProvenance
  throwError $ UnsupportedLossOperation declProv mempty (pretty t)

--------------------------------------------------------------------------------
-- Simple function conversion

convertIndexType :: (MonadTensor m) => WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertIndexType = \case
  [dim] -> do
    return $ ITensorType (explicit (IDimType dim)) (explicit IDimNil)
  _ -> unexpectedExprError currentPass "Index has incorrect number of arguments"

convertVectorType :: (MonadTensor m) => WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertVectorType = \case
  [elemType, dim] -> case argExpr elemType of
    ITensorType tBaseElem dims -> return $ ITensorType tBaseElem (explicit $ IDimCons (explicit (argExpr dim)) dims)
    unknownElemType -> do
      declProv <- getDeclProvenance
      boundCtx <- getNameContext
      let vecType = VFreeVar TensorIdent [elemType, dim]
      throwError $ HigherOrderVectors declProv boundCtx vecType unknownElemType
  _ -> unexpectedExprError currentPass "Vector has incorrect number of arguments"

convertVector :: (MonadTensor m) => WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertVector args = case args of
  [] -> compilerDeveloperError "Malformed LVec found."
  t : xs -> case argExpr t of
    ITensorType tElem dims -> do
      let mkStack elems = IDimensionDataOp (StackTensor (length elems)) (tElem : dims : elems)
      logDebug MaxDetail $ prettyVerbose tElem
      case argExpr tElem of
        IBoolElementType -> convertVectorElems dims mkStack IBoolTensor getBoolTensor IBoolConstTensor getBoolConstTensor xs
        IRatElementType -> convertVectorElems dims mkStack IRatTensor getRatTensor IRatConstTensor getRatConstTensor xs
        IDimType dim -> convertVectorElems dims mkStack (IDimIndexTensor dim) getDimIndexTensor (IDimIndexConstTensor dim) getDimIndexConstTensor xs
        _ -> compilerDeveloperError $ "Invalid base element" <+> prettyVerbose tElem
    _ -> compilerDeveloperError $ "Invalid vector element" <+> prettyVerbose t

convertVectorElems ::
  (MonadTensor m, Eq a, Pretty a) =>
  WHNFArg TensorBuiltin ->
  (WHNFSpine TensorBuiltin -> WHNFValue TensorBuiltin) ->
  (Tensor a -> WHNFValue TensorBuiltin) ->
  (WHNFValue TensorBuiltin -> Maybe (Provenance, Tensor a)) ->
  (a -> WHNFArg TensorBuiltin -> WHNFValue TensorBuiltin) ->
  (WHNFValue TensorBuiltin -> Maybe a) ->
  WHNFSpine TensorBuiltin ->
  m (WHNFValue TensorBuiltin)
convertVectorElems elementDims mkStack mkLiteralTensor getLiteralTensor mkConstantTensor getConstantTensor = \case
  -- If we're in a zero dimension vector, simply return the empty tensor
  [] -> return $ mkStack []
  x : xs -> do
    let elements = fmap argExpr (x :| xs)
    -- If all elements are literal tensors then combine into a new single literal tensor
    case traverse getLiteralTensor elements of
      Just constantTensors -> do
        return $ mkLiteralTensor $ stack (fmap snd constantTensors)
      -- If all elements are constant tensors with the same value then combine into a new single constant tensor
      Nothing -> do
        case traverse getConstantTensor elements of
          Just (c :| cs) | all (c ==) cs -> return $ mkConstantTensor c (makeExplicitDims (explicit (IDim (length elements))) elementDims)
          -- Otherwise we have to manually stack the tensor
          _ -> return $ mkStack (x : xs)

convertAt :: (MonadTensor m) => WHNFSpine TensorBuiltin -> m (WHNFValue TensorBuiltin)
convertAt = \case
  [argExpr -> ITensorType tElem dims, dim, xs, i] -> do
    let tElem' = setVisibility (Implicit True) tElem
    let dim' = setRelevance Irrelevant $ setVisibility (Implicit True) dim
    let dims' = setRelevance Irrelevant $ setVisibility (Implicit True) dims
    return $ IDimensionDataOp DimensionLookup [tElem', dim', dims', xs, i]
  _ -> unexpectedExprError currentPass "'!'"

convertDim :: (MonadTensor m) => WHNFArg Builtin -> m (WHNFArg TensorBuiltin)
convertDim arg = setVisibility Explicit . setRelevance Relevant <$> traverse convertValue arg

--------------------------------------------------------------------------------
-- Higher order function conversion

convertFoldVector :: forall m. (MonadTensor m) => WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertFoldVector spine = case spine of
  [_dim, _a, _b, argExpr -> VLam2 binder1 env binder2 body, e, xs] -> do
    lv <- getBinderDepth
    (reductionOp, pointwiseOp, dims) <- addNameToContext binder1 $ addNameToContext binder2 $ do
      go lv =<< convertExpr (extendEnvWithBound (lv + 1) binder2 $ extendEnvWithBound lv binder1 env) body

    e' <- traverse convertValue e
    xs' <- traverse convertValue xs

    let dimsArg = Arg mempty (Implicit True) Irrelevant dims
    let foldResult = evalTensorBuiltinApp reductionOp [dimsArg, xs']
    return $ evalTensorBuiltinApp pointwiseOp [dimsArg, e', explicit foldResult]
  _ -> unexpectedExprError currentPass $ "'foldVector'" <+> prettyVerbose spine
  where
    go :: Lv -> WHNFValue TensorBuiltin -> m (TensorBuiltin, TensorBuiltin, WHNFValue TensorBuiltin)
    go lv = convertHigherOrderFunction FoldVector $ \value -> case getSimpleBinaryOp lv value of
      Just (op, dims) -> case op of
        (TensorBool AndBoolTensor) -> return (TensorBool ReduceAndTensor, op, dims)
        (TensorBool OrBoolTensor) -> return (TensorBool ReduceAndTensor, op, dims)
        (TensorRat AddRatTensor) -> return (TensorRat ReduceAddRatTensor, op, dims)
        (TensorRat MulRatTensor) -> return (TensorRat ReduceMulRatTensor, op, dims)
        (TensorRat MinRatTensor) -> return (TensorRat ReduceMulRatTensor, op, dims)
        (TensorRat MaxRatTensor) -> return (TensorRat ReduceMulRatTensor, op, dims)
        _ -> cannotConvertError 2 (Right FoldVector) spine value
      _ -> cannotConvertError 2 (Right FoldVector) spine value

    getSimpleBinaryOp :: Lv -> WHNFValue TensorBuiltin -> Maybe (TensorBuiltin, WHNFValue TensorBuiltin)
    getSimpleBinaryOp lv e = case e of
      VBuiltin b [argExpr -> dims, argExpr -> VBoundVar lv1 [], argExpr -> VBoundVar lv2 []] | lv1 == lv && lv2 == lv + 1 -> Just (b, dims)
      _ -> Nothing

convertZipWith :: forall m. (MonadTensor m) => WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertZipWith spine = case spine of
  [_a, _b, _c, dim, argExpr -> VLam2 binder1 env binder2 body, xs, ys] -> do
    lv <- getBinderDepth
    liftedBody <- addNameToContext binder1 $ addNameToContext binder2 $ do
      tensorBody <- convertExpr (extendEnvWithBound (lv + 1) binder2 $ extendEnvWithBound lv binder1 env) body
      go (lv + 2) tensorBody

    dim' <- convertDim dim
    xs' <- traverse convertValue xs
    ys' <- traverse convertValue ys
    liftedBody dim' xs' ys'
  _ -> unexpectedExprError currentPass $ "'zipWith'" <+> prettyVerbose spine
  where
    go :: Lv -> WHNFValue TensorBuiltin -> m (WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> m (WHNFValue TensorBuiltin))
    go lv = convertHigherOrderFunction ZipWithVector $ \case
      VBuiltin op@(isPointwiseLiftable -> Just {}) [argExpr -> dims, argExpr -> e] -> do
        e' <- go lv e
        return $ \dim xs ys -> do
          e'' <- explicit <$> e' dim xs ys
          return $ evalTensorBuiltinApp op [makeImplicitDims dim dims, e'']
      VBuiltin op@(isPointwiseLiftable -> Just {}) [argExpr -> dims, argExpr -> e1, argExpr -> e2] -> do
        e1' <- go lv e1
        e2' <- go lv e2
        return $ \dim xs ys -> do
          let dims'' = makeImplicitDims dim dims
          e1'' <- explicit <$> e1' dim xs ys
          e2'' <- explicit <$> e2' dim xs ys
          return $ evalTensorBuiltinApp op [dims'', e1'', e2'']
      IConstTensor t x dims -> do
        return $ \dim _xs _ys -> return $ extendConstTensor t x dim dims
      VBoundVar v []
        | v == lv - 2 -> return $ \_dim xs _ys -> return $ argExpr xs
        | v == lv - 1 -> return $ \_dim _xs ys -> return $ argExpr ys
      blockedExpr -> cannotConvertError 2 (Right ZipWithVector) spine blockedExpr

convertMapVector :: forall m. (MonadTensor m) => WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertMapVector spine = case spine of
  [dim, _a, _b, argExpr -> VLam binder (WHNFClosure env body), xs] -> do
    lv <- getBinderDepth
    liftedBody <- addNameToContext binder $ do
      tensorBody <- convertExpr (extendEnvWithBound lv binder env) body
      go (lv + 1) tensorBody

    dim' <- convertDim dim
    xs' <- traverse convertValue xs

    return $ liftedBody dim' xs'
  _ -> unexpectedExprError currentPass $ "'mapVector'" <+> prettyVerbose spine
  where
    go :: Lv -> WHNFValue TensorBuiltin -> m (WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFValue TensorBuiltin)
    go lv = convertHigherOrderFunction MapVector $ \case
      VBuiltin op@(isPointwiseLiftable -> Just {}) [dims, argExpr -> e1, argExpr -> e2] -> do
        -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
        -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
        e1' <- go lv e1
        e2' <- go lv e2
        return $ \dim xs -> evalTensorBuiltinApp op [makeImplicitDims dim (argExpr dims), explicit (e1' dim xs), explicit (e2' dim xs)]
      VBoundVar v []
        | v == lv - 1 -> return $ \_dim xs -> argExpr xs
      IDimensionDataOp ConstTensor [t, x, dims] -> do
        return $ \dim _xs -> extendConstTensor t x dim dims
      blockedExpr ->
        cannotConvertError 1 (Right MapVector) spine blockedExpr

convertForeachIndex :: forall m. (MonadTensor m) => WHNFSpine Builtin -> m (WHNFValue TensorBuiltin)
convertForeachIndex spine = case spine of
  [argExpr -> typ, dim, argExpr -> VLam binder (WHNFClosure env body)] -> do
    typ' <- convertValue typ
    lv <- getBinderDepth
    addNameToContext binder $ do
      dim' <- traverse convertValue dim
      body' <- convertExpr (extendEnvWithBound lv binder env) body
      go lv typ' dim' body'
  _ -> unexpectedExprError currentPass "'foreachIndex'"
  where
    go :: Lv -> WHNFValue TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFValue TensorBuiltin -> m (WHNFValue TensorBuiltin)
    go lv resultType dim = convertHigherOrderFunction StdForeachIndex $ \case
      VBuiltin b@(isPointwiseLiftable -> Just tElem) [argExpr -> dims, e] -> do
        -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
        -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
        let newType = ITensorType (explicit (VBuiltin tElem [])) (explicit dims)
        e' <- traverse (go lv newType dim) e
        return $ evalTensorBuiltinApp b [makeImplicitDims dim dims, e']
      VBuiltin b@(isPointwiseLiftable -> Just tElem) [argExpr -> dims, e1, e2] -> do
        -- Distribute the `forallIndex` across a liftable operation (e.g. `and`).
        -- e.g. `foreach i . x(i) op y(i)` -> `(foreach i . x(i)) op (forall i . y(i))`
        let newType = ITensorType (explicit (VBuiltin tElem [])) (explicit dims)
        e1' <- traverse (go lv newType dim) e1
        e2' <- traverse (go lv newType dim) e2
        return $ evalTensorBuiltinApp b [makeImplicitDims dim dims, e1', e2']
      IDimensionDataOp DimensionLookup [_tElem, _dim, _dims, xs, argExpr -> VBoundVar lv1 []]
        | lv1 == lv ->
            -- Eliminate `forall i . xs ! i` into `xs`
            return $ argExpr xs
      IConstTensor t x dims -> do
        return $ extendConstTensor t x dim dims
      VBoundVar lv1 []
        | lv1 /= lv -> do
            logDebug MaxDetail $ prettyVerbose resultType
            case resultType of
              ITensorType tElem dims@(argExpr -> IDimNil) -> do
                let tElem' = setVisibility (Implicit True) tElem
                let value = explicit (VBoundVar lv1 [])
                return $ IConstTensor tElem' value (makeExplicitDims dim dims)
              _ -> cannotConvertError 1 (Right MapVector) spine (VBoundVar lv1 [])
      blockedExpr -> do
        logDebug MaxDetail $ prettyVerbose blockedExpr
        cannotConvertError 1 (Left StdForeachIndex) spine blockedExpr

-- | Returns Nothing if not liftable, and the argument type if it is liftable
isPointwiseLiftable :: TensorBuiltin -> Maybe TensorBuiltin
isPointwiseLiftable = \case
  TensorBool op -> case op of
    BoolType -> Nothing
    BoolLiteral {} -> Nothing
    AndBoolTensor -> Just (TensorBool BoolType)
    OrBoolTensor -> Just (TensorBool BoolType)
    NotBoolTensor -> Just (TensorBool BoolType)
    EqualsRatTensor {} -> Just (TensorRat RatType)
    OrderRatTensor {} -> Just (TensorRat RatType)
    ReduceAndTensor -> Nothing
    ReduceOrTensor -> Nothing
    BoolTensor {} -> Nothing
    QuantifyRatTensor {} -> Nothing
  TensorRat op -> case op of
    RatTensor {} -> Nothing
    RatType -> Nothing
    RatLiteral {} -> Nothing
    NegRatTensor -> Just (TensorRat RatType)
    AddRatTensor -> Just (TensorRat RatType)
    SubRatTensor -> Just (TensorRat RatType)
    MulRatTensor -> Just (TensorRat RatType)
    DivRatTensor -> Just (TensorRat RatType)
    MinRatTensor -> Just (TensorRat RatType)
    MaxRatTensor -> Just (TensorRat RatType)
    ReduceAddRatTensor -> Nothing
    ReduceMulRatTensor -> Nothing
    ReduceMinRatTensor -> Nothing
    ReduceMaxRatTensor -> Nothing
    SearchRatTensor -> Nothing
  TensorDimData {} -> Nothing
  TensorDimType {} -> Nothing

{-
isReduction :: TensorBuiltin -> Maybe TensorBuiltin
isReduction = \case
  TensorBool op -> case op of
    BoolType -> Nothing
    BoolLiteral {} -> Nothing
    AndBoolTensor -> Nothing
    OrBoolTensor -> Nothing
    NotBoolTensor -> Nothing
    EqualsRatTensor {} -> Nothing
    OrderRatTensor {} -> Nothing
    ReduceAndTensor -> Just (TensorBool BoolType)
    ReduceOrTensor -> Just (TensorBool BoolType)
    BoolTensor {} -> Nothing
    QuantifyRatTensor {} -> Nothing
  TensorRat op -> case op of
    RatTensor {} -> Nothing
    RatType -> Nothing
    RatLiteral {} -> Nothing
    NegRatTensor -> Nothing
    AddRatTensor -> Nothing
    SubRatTensor -> Nothing
    MulRatTensor -> Nothing
    DivRatTensor -> Nothing
    MinRatTensor -> Nothing
    MaxRatTensor -> Nothing
    ReduceAddRatTensor -> Just (TensorRat RatType)
    ReduceMulRatTensor -> Just (TensorRat RatType)
    ReduceMinRatTensor -> Just (TensorRat RatType)
    ReduceMaxRatTensor -> Just (TensorRat RatType)
    SearchRatTensor -> Nothing
  TensorDimData {} -> Nothing
  TensorDimType {} -> Nothing
-}
currentPass :: CompilerPass
currentPass = "conversion to tensors"

cannotConvertError :: (MonadTensor m) => Arity -> Either StdLibFunction BuiltinFunction -> WHNFSpine Builtin -> WHNFValue TensorBuiltin -> m b
cannotConvertError arity fn args problematicExpr = do
  prov <- getDeclProvenance
  problematicCtx <- getNameContext
  let originalCtx = drop arity problematicCtx
  let originalExpr = either (VFreeVar . identifierOf) (VBuiltin . BuiltinFunction) fn args
  throwError $ UnsupportedHigherOrderTensorCode prov originalCtx originalExpr problematicCtx problematicExpr

evalTensorBuiltinApp :: TensorBuiltin -> WHNFSpine TensorBuiltin -> WHNFValue TensorBuiltin
evalTensorBuiltinApp op spine = evalTensorBuiltin op (VBuiltin op spine) (filterOutIrrelevantArgs spine)

makeImplicitDims :: WHNFArg TensorBuiltin -> WHNFValue TensorBuiltin -> WHNFArg TensorBuiltin
makeImplicitDims dim dims = Arg mempty (Implicit True) Irrelevant (IDimCons dim (explicit dims))

makeExplicitDims :: WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin
makeExplicitDims dim dims = Arg mempty Explicit Relevant (IDimCons dim dims)

extendConstTensor :: WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFArg TensorBuiltin -> WHNFValue TensorBuiltin
extendConstTensor t x dim dims = IDimensionDataOp ConstTensor [t, x, makeExplicitDims dim dims]

convertHigherOrderFunction ::
  (MonadLogger m, MonadNameContext m, Pretty fn, PrettyFriendly (Contextualised b NamedBoundCtx), PrettyVerbose b) =>
  fn ->
  (b -> m a) ->
  b ->
  m a
convertHigherOrderFunction origFn convert lamBody = do
  showEntry ("enter-" <> pretty origFn) lamBody
  result <- convert lamBody
  decrCallDepth
  logDebug MaxDetail ("exit-" <> pretty origFn)
  return result

showEntry :: (MonadLogger m, MonadNameContext m, PrettyFriendly (Contextualised b NamedBoundCtx), PrettyVerbose b) => Doc a -> b -> m ()
showEntry doc e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ doc <+> ":" <+> prettyVerbose e
  logDebug MaxDetail $ doc <+> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogger m, MonadNameContext m, PrettyFriendly (Contextualised b NamedBoundCtx), PrettyVerbose b) => Doc a -> b -> m ()
showExit doc e = do
  ctx <- getNameContext
  decrCallDepth
  logDebug MaxDetail $ doc <+> ": " <+> prettyFriendly (WithContext e ctx)
