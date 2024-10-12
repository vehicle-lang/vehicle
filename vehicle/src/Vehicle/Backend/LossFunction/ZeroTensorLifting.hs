module Vehicle.Backend.LossFunction.ZeroTensorLifting
  ( liftZeroDimensionalTensors,
  )
where

import Data.Tuple (swap)
import Vehicle.Compile.Context.Name
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (traverseClosure, traverseClosureGeneric)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (Tensor (..))

--------------------------------------------------------------------------------
-- Lifting

type MonadLiftZeroDims m =
  ( MonadLogger m,
    MonadNameContext m
  )

liftZeroDimensionalTensors ::
  (MonadLiftZeroDims m) =>
  WHNFDecl LossTensorBuiltin ->
  m (WHNFDecl LossTensorBuiltin)
liftZeroDimensionalTensors = \case
  decl@DefAbstract {} -> return decl
  DefFunction p ident anns typ body -> do
    (typ', body') <- liftDecl (typ, body)
    return $ DefFunction p ident anns typ' body'

liftDecl ::
  (MonadLiftZeroDims m) =>
  (WHNFType LossTensorBuiltin, WHNFValue LossTensorBuiltin) ->
  m (WHNFType LossTensorBuiltin, WHNFValue LossTensorBuiltin)
liftDecl (t, e) = case (t, e) of
  (VPi piBinder piBody, VLam lamBinder closure) -> do
    (newClosure, newPiBody) <- traverseClosureGeneric (\lamBody -> liftDecl (piBody, lamBody)) swap mempty lamBinder closure
    return (VPi piBinder newPiBody, VLam lamBinder newClosure)
  (typ, body) -> do
    let (wasZeroDimensional, newType) = liftType typ
    liftedBody <- liftExpr wasZeroDimensional body
    return (newType, liftedBody)

liftExpr :: (MonadLiftZeroDims m) => Bool -> WHNFValue LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftExpr zeroDims e = do
  showEntry zeroDims e
  result <- case e of
    VBoundVar v spine -> do
      VBoundVar v <$> traverseSpine (liftExpr False) spine
    VBuiltin b spine ->
      liftBuiltin zeroDims b spine
    VFreeVar {} -> do
      unexpectedExprError currentPass "VFreeVar"
    VPi {} -> do
      unexpectedExprError currentPass "VPi"
    VLam {} -> do
      unexpectedExprError currentPass "VLam"
    VUniverse {} ->
      unexpectedExprError currentPass "VUniverse"
    VMeta {} ->
      unexpectedExprError currentPass "VMeta"
  showExit result
  return result

liftBuiltin :: (MonadLiftZeroDims m) => Bool -> LossTensorBuiltin -> WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftBuiltin zeroDims b spine = case b of
  LossTensorDimType {} -> unexpected
  LossTensorRat r -> case r of
    RatTensor t -> return $ liftTensor t
    NegRatTensor -> liftPointwiseOp zeroDims b spine
    AddRatTensor -> liftPointwiseOp zeroDims b spine
    SubRatTensor -> liftPointwiseOp zeroDims b spine
    MulRatTensor -> liftPointwiseOp zeroDims b spine
    DivRatTensor -> liftPointwiseOp zeroDims b spine
    MinRatTensor -> liftPointwiseOp zeroDims b spine
    MaxRatTensor -> liftPointwiseOp zeroDims b spine
    SearchRatTensor {} -> liftSearch spine
    RatType -> unexpected
    RatLiteral {} -> unexpected
    ReduceAddRatTensor -> liftReduction b spine
    ReduceMulRatTensor -> liftReduction b spine
    ReduceMinRatTensor -> liftReduction b spine
    ReduceMaxRatTensor -> liftReduction b spine
  LossTensorDimData s -> case s of
    DimensionLookup -> liftDimensionLookup spine
    ConstTensor -> return $ liftConst spine
    StackTensor n -> liftStack n spine
    DimensionIndex {} -> unexpected
    DimensionIndexTensor {} -> unexpected
    Dimension {} -> unexpected
    DimensionNil -> unexpected
    DimensionCons -> unexpected
  where
    unexpected = unexpectedExprError currentPass (pretty b)

liftTensor :: Tensor Rational -> WHNFValue LossTensorBuiltin
liftTensor Tensor {..} = IRatTensor $ Tensor {tensorShape = 1 : tensorShape, ..}

liftPointwiseOp :: (MonadLiftZeroDims m) => Bool -> LossTensorBuiltin -> WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftPointwiseOp zeroDims b (dims : args) = do
  args' <- traverseSpine (liftExpr zeroDims) args
  let dims' = if zeroDims then replaceArgExpr (dimSingleton 1) dims else dims
  return $ VBuiltin b (dims' : args')
liftPointwiseOp _ _ [] = unexpectedExprError currentPass "bad-pointwise-args"

liftReduction :: (MonadLiftZeroDims m) => LossTensorBuiltin -> WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftReduction b (dims : args) = do
  args' <- traverseSpine (liftExpr False) args
  return $ VBuiltin b (dims : args')
liftReduction _ _ = unexpectedExprError currentPass "bad-reduction-args"

liftConst :: WHNFSpine LossTensorBuiltin -> WHNFValue LossTensorBuiltin
liftConst [tElem, value, dims] = IConstTensor tElem value (replaceArgExpr (dimSingleton 1) dims)
liftConst spine = unexpectedExprError currentPass (prettyVerbose $ IDimensionDataOp ConstTensor spine)

liftStack :: (MonadLiftZeroDims m) => Int -> WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftStack n (tElem : dims : values) = do
  values' <- traverseSpine (liftExpr False) values
  return $ IDimensionDataOp (StackTensor n) (tElem : dims : values')
liftStack n spine = unexpectedExprError currentPass (prettyVerbose $ IDimensionDataOp (StackTensor n) spine)

liftSearch :: (MonadLiftZeroDims m) => WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftSearch = \case
  [dims, op, lower, upper, argExpr -> fn] -> case fn of
    VLam binder closure -> do
      let (wasZeroDimensional, newType) = liftType (typeOf binder)
      let newDims = if wasZeroDimensional then replaceArgExpr (dimSingleton 1) dims else dims
      let newBinder = replaceBinderType newType binder
      newClosure <- traverseClosure (liftExpr wasZeroDimensional) mempty newBinder closure
      let newFn = explicit $ VLam newBinder newClosure
      return $ VBuiltin (LossTensorRat SearchRatTensor) [newDims, op, lower, upper, newFn]
    _ -> unexpectedExprError currentPass "bad-search-lam"
  _ -> unexpectedExprError currentPass "bad-search-args"

liftType :: WHNFType LossTensorBuiltin -> (Bool, WHNFType LossTensorBuiltin)
liftType = \case
  ITensorType tElem (argExpr -> IDimNil) -> (True, ITensorType tElem (explicit (dimSingleton 1)))
  typ -> (False, typ)

liftDimensionLookup :: (MonadLiftZeroDims m) => WHNFSpine LossTensorBuiltin -> m (WHNFValue LossTensorBuiltin)
liftDimensionLookup [tElem, dim, dims, xs, i] = do
  xs' <- traverse (liftExpr False) xs
  return $ IDimensionDataOp DimensionLookup [tElem, dim, dims, xs', i]
liftDimensionLookup spine = unexpectedExprError currentPass (prettyVerbose $ IDimensionDataOp DimensionLookup spine)

showEntry :: (MonadLogger m, MonadNameContext m) => Bool -> WHNFValue LossTensorBuiltin -> m ()
showEntry is0D e = do
  ctx <- getNameContext
  -- logDebug MaxDetail $ doc <+> ":" <+> prettyVerbose e
  logDebug MaxDetail $ "enter-lift0D-" <> pretty is0D <+> ":" <+> prettyFriendly (WithContext e ctx)
  incrCallDepth

showExit :: (MonadLogger m, MonadNameContext m) => WHNFValue LossTensorBuiltin -> m ()
showExit e = do
  ctx <- getNameContext
  decrCallDepth
  logDebug MaxDetail $ "exit-lift0D" <+> ": " <+> prettyFriendly (WithContext e ctx)

currentPass :: Doc a
currentPass = "zero-tensor-lifting"
