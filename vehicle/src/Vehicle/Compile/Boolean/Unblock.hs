module Vehicle.Compile.Boolean.Unblock
  ( unblockBoolExpr,
    tryPurifyAssertion,
    UnblockingActions (..),
    ReduceVectorVars,
  )
where

import Vehicle.Compile.Boolean.LiftIf
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Libraries.StandardLibrary.Definitions

--------------------------------------------------------------------------------
-- Unblocking
--------------------------------------------------------------------------------

type MonadUnblock m =
  ( MonadLogger m,
    MonadFreeContext Builtin m
  )

data UnblockingActions m = UnblockingActions
  { unblockFreeVectorVar ::
      (ReduceVectorVars -> WHNFValue Builtin -> m (WHNFValue Builtin)) ->
      ReduceVectorVars ->
      Identifier ->
      WHNFSpine Builtin ->
      m (WHNFValue Builtin),
    unblockBoundVectorVar ::
      Lv ->
      m (WHNFValue Builtin)
  }

unblockBoolExpr ::
  (MonadUnblock m) =>
  NamedBoundCtx ->
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockBoolExpr ctx actions expr = do
  let exprDoc = prettyFriendly (WithContext expr ctx)
  logDebug MaxDetail $ line <> "Unblocking" <+> squotes exprDoc
  incrCallDepth

  unblockedExpr <- unblockNonVector actions expr
  let unblockedExprDoc = prettyFriendly (WithContext unblockedExpr ctx)
  logDebug MaxDetail $ "Unblocked to" <+> squotes unblockedExprDoc
  decrCallDepth
  return unblockedExpr

--------------------------------------------------------------------------------
-- Unblocking types

type ReduceVectorVars = Bool

-- | Lifts all `if`s in the provided expression `e` to the top-level, while
-- preserving the guarantee that the expression is normalised as much as
-- possible.
unblockNonVector ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockNonVector actions expr = case expr of
  IBoolLiteral {} -> return expr
  IIndexLiteral {} -> return expr
  INatLiteral {} -> return expr
  IRatLiteral {} -> return expr
  IAnd {} -> return expr
  IOr {} -> return expr
  INot {} -> return expr
  IIf {} -> return expr
  IForall {} -> return expr
  IExists {} -> return expr
  IVectorEqualFull spine@(IVecEqSpine t _ _ _ _ _)
    | isRatTensor (argExpr t) -> return expr
    | otherwise -> appHiddenStdlibDef StdEqualsVector spine
  IVectorNotEqualFull spine@(IVecEqSpine t _ _ _ _ _)
    | isRatTensor (argExpr t) -> return expr
    | otherwise -> appHiddenStdlibDef StdNotEqualsVector spine
  IEqualOp dom op x y implArgs -> case dom of
    EqIndex -> unblockNonVectorOp2 actions (Equals dom op) (evalEqualsIndex op) x y implArgs
    EqNat -> unblockNonVectorOp2 actions (Equals dom op) (evalEqualsNat op) x y implArgs
    EqRat -> return expr
  IOrderOp dom op x y implArgs -> case dom of
    OrderIndex -> unblockNonVectorOp2 actions (Order dom op) (evalOrderIndex op) x y implArgs
    OrderNat -> unblockNonVectorOp2 actions (Order dom op) (evalOrderNat op) x y implArgs
    OrderRat -> return expr
  IAt t n xs i -> unblockAt actions t n xs i
  IFoldVector t1 t2 n f e xs -> unblockFoldVector actions t1 t2 n f e xs
  _ -> unexpectedExprError "unblocking non-vectors" (prettyVerbose expr)

unblockVector ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  ReduceVectorVars ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockVector actions reduceVectorVars expr = case expr of
  VBoundVar v []
    | reduceVectorVars -> unblockBoundVectorVar actions v
    | otherwise -> return expr
  IVecLiteral {} -> return expr
  IIf {} -> return expr
  IStandardLib StdAddVector (IVecOp2Spine t1 t2 t3 n s xs ys) ->
    unblockVectorOp2 actions reduceVectorVars StdAddVector [t1, t2, t3, n, s] xs ys
  IStandardLib StdSubVector (IVecOp2Spine t1 t2 t3 n s xs ys) ->
    unblockVectorOp2 actions reduceVectorVars StdSubVector [t1, t2, t3, n, s] xs ys
  VFreeVar ident spine -> unblockFreeVectorVar actions (unblockVector actions) reduceVectorVars ident spine
  IMapVector t1 t2 n f xs -> unblockMapVector actions t1 t2 n f xs
  IFoldVector t1 t2 n f e xs -> unblockFoldVector actions t1 t2 n f e xs
  IZipWithVector t1 t2 t3 n f xs ys -> unblockZipWith actions t1 t2 t3 n f xs ys
  IAt t n xs i -> unblockAt actions t n xs i
  VIndices n -> unblockIndices actions n
  _ -> unexpectedExprError "unblocking vector" (prettyVerbose expr)

--------------------------------------------------------------------------------
-- Unblocking operations

unblockNonVectorOp2 ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  BuiltinFunction ->
  (WHNFValue Builtin -> WHNFSpine Builtin -> WHNFValue Builtin) ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFSpine Builtin ->
  m (WHNFValue Builtin)
unblockNonVectorOp2 actions b evalOp2 x y implArgs = do
  x' <- unblockNonVector actions x
  y' <- unblockNonVector actions y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      forceEvalSimple b evalOp2 (implArgs <> [explicit x'', explicit y''])

unblockVectorOp2 ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  ReduceVectorVars ->
  StdLibFunction ->
  WHNFSpine Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockVectorOp2 actions reduceVectorVars = traverseVectorOp2 (unblockVector actions reduceVectorVars)

unblockFoldVector ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockFoldVector actions t1 t2 n f e xs = do
  xs' <- unblockVector actions True xs
  flip liftIf xs' $ \xs'' ->
    forceEval FoldVector (evalFoldVector normaliseApp) [t1, t2, n, explicit f, explicit e, explicit xs'']

unblockMapVector ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockMapVector actions t1 t2 n f xs = do
  xs' <- unblockVector actions True xs
  flip liftIf xs' $ \xs'' ->
    forceEval MapVector (evalMapVector normaliseApp) [t1, t2, n, explicit f, explicit xs'']

unblockZipWith ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockZipWith actions t1 t2 t3 n f xs ys = do
  xs' <- unblockVector actions True xs
  ys' <- unblockVector actions True ys
  flip liftIf xs' $ \xs'' ->
    flip liftIf ys' $ \ys'' ->
      forceEval ZipWithVector (evalZipWith normaliseApp) [t1, t2, t3, n, explicit f, explicit xs'', explicit ys'']

unblockAt ::
  forall m.
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFArg Builtin ->
  WHNFArg Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockAt actions t n c i = case c of
  IVecLiteral {} -> do
    i' <- unblockNonVector actions i
    flip liftIf i' $ \i'' -> do
      forceEvalSimple At evalAt [t, n, explicit c, explicit i'']
  IMapVector _ _ t2 f xs -> appAt f [(t2, n, xs)] i
  IZipWithVector t1 t2 _ _ f xs ys -> appAt f [(t1, n, xs), (t2, n, ys)] i
  IVectorAdd t1 t2 _ _ f xs ys -> appAt (argExpr f) [(t1, n, xs), (t2, n, ys)] i
  IVectorSub t1 t2 _ _ f xs ys -> appAt (argExpr f) [(t1, n, xs), (t2, n, ys)] i
  _ -> do
    -- Don't reduce vector bound variables in container as it may trigger extremely expensive normalisation
    -- that we can avoid because we're only looking up a single element of it.
    c' <- unblockVector actions (isVBoundVar c) c
    unblockAt actions t n c' i
  where
    appAt ::
      (MonadUnblock m) =>
      WHNFValue Builtin ->
      [(WHNFArg Builtin, WHNFArg Builtin, WHNFValue Builtin)] ->
      WHNFValue Builtin ->
      m (WHNFValue Builtin)
    appAt f args index = normaliseApp f =<< traverse (appIndexToArg index) args

    appIndexToArg ::
      (MonadUnblock m) =>
      WHNFValue Builtin ->
      (WHNFArg Builtin, WHNFArg Builtin, WHNFValue Builtin) ->
      m (WHNFArg Builtin)
    appIndexToArg index (t', n', xs) =
      Arg mempty Explicit Relevant
        <$> unblockAt actions t' n' xs index

unblockIndices ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
unblockIndices actions n = do
  n' <- unblockNonVector actions n
  flip liftIf n' $ \n'' ->
    forceEvalSimple Indices (evalIndices (VBuiltinFunction Indices)) (explicit <$> [n''])

forceEval ::
  (MonadLogger m) =>
  BuiltinFunction ->
  (WHNFValue Builtin -> WHNFSpine Builtin -> m (WHNFValue Builtin)) ->
  WHNFSpine Builtin ->
  m (WHNFValue Builtin)
forceEval b evalFn args = evalFn cannotEvalError $ filterOutIrrelevantArgs args
  where
    cannotEvalError = developerError $ "Unexpectedly blocked expression" <+> prettyVerbose (VBuiltin (BuiltinFunction b) args)

forceEvalSimple ::
  (MonadLogger m) =>
  BuiltinFunction ->
  (WHNFValue Builtin -> WHNFSpine Builtin -> WHNFValue Builtin) ->
  WHNFSpine Builtin ->
  m (WHNFValue Builtin)
forceEvalSimple b evalFn = forceEval b (\orig args -> return $ evalFn orig args)

--------------------------------------------------------------------------------
-- Purification

tryPurifyAssertion ::
  (MonadUnblock m) =>
  NamedBoundCtx ->
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (Either (WHNFValue Builtin) (WHNFValue Builtin, WHNFValue Builtin))
tryPurifyAssertion ctx actions assertion = do
  let assertionDoc = prettyFriendly (WithContext assertion ctx)
  logDebug MaxDetail $ line <> "Trying to purify" <+> squotes assertionDoc
  incrCallDepth

  unblockedExpr <- case assertion of
    IEqualRat x y -> purifyRatOp2 actions IEqualRat (evalEqualsRat Eq) x y
    IOrderRat op x y -> purifyRatOp2 actions (IOrderRat op) (evalOrderRat op) x y
    IVectorEqualFull (IVecEqSpine t1 t2 n sol xs ys) -> purifyVectorOp2 actions StdEqualsVector [t1, t2, n, sol] xs ys
    _ -> unexpectedExprError "purifying assertion" assertionDoc

  let unblockedAssertionDoc = prettyFriendly (WithContext unblockedExpr ctx)
  logDebug MaxDetail $ "Result" <+> squotes unblockedAssertionDoc

  let onPurified x y = do
        logDebug MaxDetail "No new boolean structure found."
        decrCallDepth
        return $ Right (x, y)

  let onUnpurified expr = do
        logDebug MaxDetail "New boolean structure found."
        decrCallDepth
        return $ Left expr

  case unblockedExpr of
    IEqual EqRat x y -> onPurified x y
    IOrder OrderRat _ x y -> onPurified x y
    IVectorEqual xs ys -> onPurified xs ys
    _ -> onUnpurified unblockedExpr

purify ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
purify actions expr = case expr of
  -- Rational operators
  IRatLiteral {} -> return expr
  INeg NegRat x -> purifyNegRat actions x
  IAdd AddRat x y -> purifyRatOp2 actions (IAdd AddRat) evalAddRat x y
  ISub SubRat x y -> purifyRatOp2 actions (ISub SubRat) evalSubRat x y
  IMul MulRat x y -> purifyRatOp2 actions (IMul MulRat) evalMulRat x y
  IDiv DivRat x y -> purifyRatOp2 actions (IDiv DivRat) evalDivRat x y
  -- Vector operators
  IVecLiteral t xs -> purifyVectorLiteral actions t xs
  IStandardLib StdAddVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 actions StdAddVector [t1, t2, t3, n, s] xs ys
  IStandardLib StdSubVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 actions StdSubVector [t1, t2, t3, n, s] xs ys
  IMapVector t1 t2 n f xs -> unblockMapVector actions t1 t2 n f xs
  VFreeVar ident spine -> unblockFreeVectorVar actions (unblockVector actions) False ident spine
  -- Polymorphic
  VBoundVar _v [] -> return expr
  IIf {} -> return expr
  IFoldVector t1 t2 n f e xs -> unblockFoldVector actions t1 t2 n f e xs
  IAt t n xs i -> unblockAt actions t n xs i
  -- Other
  _ -> developerError $ "Do not yet support purification of" <+> prettyVerbose expr

purifyVectorLiteral ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFArg Builtin ->
  WHNFSpine Builtin ->
  m (WHNFValue Builtin)
purifyVectorLiteral actions t xs = do
  xs' <- traverse (traverse (purify actions)) xs
  liftIfSpine xs' $ \xs'' ->
    return $ IVecLiteral t xs''

purifyVectorOp2 ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  StdLibFunction ->
  WHNFSpine Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
purifyVectorOp2 actions = traverseVectorOp2 (purify actions)

purifyRatOp2 ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  (WHNFValue Builtin -> WHNFValue Builtin -> WHNFValue Builtin) ->
  (WHNFValue Builtin -> WHNFSpine Builtin -> WHNFValue Builtin) ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
purifyRatOp2 actions mkOp evalOp2 x y = do
  x' <- purify actions x
  y' <- purify actions y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      return $ evalOp2 (mkOp x'' y'') [explicit x'', explicit y'']

purifyNegRat ::
  (MonadUnblock m) =>
  UnblockingActions m ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
purifyNegRat actions x = do
  x' <- purify actions x
  flip liftIf x' $ \x'' ->
    return $ evalNegRat (INeg NegRat x'') [explicit x'']

traverseVectorOp2 ::
  (MonadUnblock m) =>
  (WHNFValue Builtin -> m (WHNFValue Builtin)) ->
  StdLibFunction ->
  WHNFSpine Builtin ->
  WHNFValue Builtin ->
  WHNFValue Builtin ->
  m (WHNFValue Builtin)
traverseVectorOp2 f fn spinePrefix xs ys = do
  xs' <- f xs
  ys' <- f ys
  flip liftIf xs' $ \xs'' ->
    flip liftIf ys' $ \ys'' -> do
      let newSpine = spinePrefix <> (Arg mempty Explicit Relevant <$> [xs'', ys''])
      case (xs'', ys'') of
        (IVecLiteral {}, IVecLiteral {}) -> appHiddenStdlibDef fn newSpine
        _ -> return $ IStandardLib fn newSpine

isRatTensor :: WHNFType Builtin -> Bool
isRatTensor = \case
  IRatType {} -> True
  IVectorType _ tElem _ -> isRatTensor tElem
  _ -> False
