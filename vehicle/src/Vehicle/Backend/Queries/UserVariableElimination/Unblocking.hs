module Vehicle.Backend.Queries.UserVariableElimination.Unblocking
  ( unblockBoolExpr,
    tryPurifyAssertion,
  )
where

import Control.Monad.Reader (asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.Map qualified as Map
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.EliminateIf
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Resource (NetworkTensorType (..), NetworkType (..))
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Expr.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Unblocking
--------------------------------------------------------------------------------

unblockBoolExpr ::
  (MonadQueryStructure m, MonadWriter [WHNFValue QueryBuiltin] m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockBoolExpr expr = do
  exprDoc <- prettyFriendlyInCtx expr
  logDebug MaxDetail $ line <> "Unblocking" <+> squotes exprDoc
  incrCallDepth

  unblockedExpr <- unblockNonVector expr
  unblockedExprDoc <- prettyFriendlyInCtx unblockedExpr
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
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockNonVector expr = case expr of
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
    EqIndex -> unblockNonVectorOp2 (Equals dom op) (evalEqualsIndex op) x y implArgs
    EqNat -> unblockNonVectorOp2 (Equals dom op) (evalEqualsNat op) x y implArgs
    EqRat -> return expr
  IOrderOp dom op x y implArgs -> case dom of
    OrderIndex -> unblockNonVectorOp2 (Order dom op) (evalOrderIndex op) x y implArgs
    OrderNat -> unblockNonVectorOp2 (Order dom op) (evalOrderNat op) x y implArgs
    OrderRat -> return expr
  IAt t n xs i -> unblockAt t n xs i
  IFoldVector t1 t2 n f e xs -> unblockFoldVector t1 t2 n f e xs
  _ -> unexpectedExprError "unblocking non-vectors" (prettyVerbose expr)

unblockVector ::
  (MonadUnblock m) =>
  ReduceVectorVars ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockVector reduceVectorVars expr = case expr of
  VBoundVar v []
    | reduceVectorVars -> reduceBoundVar v
    | otherwise -> return expr
  IVecLiteral {} -> return expr
  IIf {} -> return expr
  IStandardLib StdAddVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> unblockVectorOp2 reduceVectorVars StdAddVector [t1, t2, t3, n, s] xs ys
  IStandardLib StdSubVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> unblockVectorOp2 reduceVectorVars StdSubVector [t1, t2, t3, n, s] xs ys
  VFreeVar ident spine -> unblockNetwork reduceVectorVars ident spine
  IMapVector t1 t2 n f xs -> unblockMapVector t1 t2 n f xs
  IFoldVector t1 t2 n f e xs -> unblockFoldVector t1 t2 n f e xs
  IZipWithVector t1 t2 t3 n f xs ys -> unblockZipWith t1 t2 t3 n f xs ys
  IAt t n xs i -> unblockAt t n xs i
  VIndices n -> unblockIndices n
  _ -> unexpectedExprError "unblocking vector" (prettyVerbose expr)

--------------------------------------------------------------------------------
-- Unblocking operations

unblockNonVectorOp2 ::
  (MonadUnblock m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin) ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockNonVectorOp2 b evalOp2 x y implArgs = do
  x' <- unblockNonVector x
  y' <- unblockNonVector y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      forceEvalSimple b evalOp2 (implArgs <> [explicit x'', explicit y''])

unblockVectorOp2 ::
  (MonadUnblock m) =>
  ReduceVectorVars ->
  StdLibFunction ->
  WHNFSpine QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockVectorOp2 reduceVectorVars = traverseVectorOp2 (unblockVector reduceVectorVars)

unblockNetwork ::
  (MonadUnblock m) =>
  ReduceVectorVars ->
  Identifier ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockNetwork reduceVectorVars ident spine = do
  let networkName = nameOf ident
  networkContext <- asks networkCtx
  networkInfo <- case Map.lookup networkName networkContext of
    Nothing -> compilerDeveloperError $ "Expecting" <+> quotePretty ident <+> "to be a @network"
    Just info -> return info

  unblockedSpine <- traverse (traverse (unblockVector False)) spine
  liftIfSpine unblockedSpine $ \unblockedSpine' ->
    if unblockedSpine' /= unblockedSpine
      then return $ VFreeVar ident unblockedSpine'
      else do
        let networkApp = (networkName, unblockedSpine')
        globalCtx <- get

        case LinkedHashMap.lookup networkApp (networkApplications globalCtx) of
          Just existingAppInfo -> return $ outputVarExpr existingAppInfo
          Nothing -> do
            input <- case spine of
              [inputArg] -> return $ argExpr inputArg
              _ -> do
                exprDoc <- prettyFriendlyInCtx (VFreeVar ident spine)
                compilerDeveloperError $
                  "Found network application with multiple arguments:"
                    <> line
                    <> indent 2 exprDoc

            let (appInfo, newGlobalCtx) = addNetworkApplicationToGlobalCtx networkApp networkInfo globalCtx
            let inputDims = dimensions (inputTensor (networkType networkInfo))
            let inputEquality = mkVVectorEquality inputDims (inputVarExpr appInfo) input
            put newGlobalCtx
            tell [inputEquality]
            unblockVector reduceVectorVars (outputVarExpr appInfo)

unblockFoldVector ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockFoldVector t1 t2 n f e xs = do
  xs' <- unblockVector True xs
  flip liftIf xs' $ \xs'' ->
    forceEval FoldVector (evalFoldVector normaliseApp) [t1, t2, n, explicit f, explicit e, explicit xs'']

unblockMapVector ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockMapVector t1 t2 n f xs = do
  xs' <- unblockVector True xs
  flip liftIf xs' $ \xs'' ->
    forceEval MapVector (evalMapVector normaliseApp) [t1, t2, n, explicit f, explicit xs'']

unblockZipWith ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockZipWith t1 t2 t3 n f xs ys = do
  xs' <- unblockVector True xs
  ys' <- unblockVector True ys
  flip liftIf xs' $ \xs'' ->
    flip liftIf ys' $ \ys'' ->
      forceEval ZipWithVector (evalZipWith normaliseApp) [t1, t2, t3, n, explicit f, explicit xs'', explicit ys'']

unblockAt ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockAt t n c i = case c of
  IVecLiteral {} -> do
    i' <- unblockNonVector i
    flip liftIf i' $ \i'' -> do
      forceEvalSimple At evalAt [t, n, explicit c, explicit i'']
  IMapVector _ _ t2 f xs -> appAt f [(t2, n, xs)] i
  IZipWithVector t1 t2 _ _ f xs ys -> appAt f [(t1, n, xs), (t2, n, ys)] i
  IVectorAdd t1 t2 _ _ f xs ys -> appAt (argExpr f) [(t1, n, xs), (t2, n, ys)] i
  IVectorSub t1 t2 _ _ f xs ys -> appAt (argExpr f) [(t1, n, xs), (t2, n, ys)] i
  _ -> do
    -- Don't reduce vector bound variables in container as it may trigger extremely expensive normalisation
    -- that we can avoid because we're only looking up a single element of it.
    c' <- unblockVector (isVBoundVar c) c
    unblockAt t n c' i
  where
    appAt ::
      (MonadUnblock m) =>
      WHNFValue QueryBuiltin ->
      [(WHNFArg QueryBuiltin, WHNFArg QueryBuiltin, WHNFValue QueryBuiltin)] ->
      WHNFValue QueryBuiltin ->
      m (WHNFValue QueryBuiltin)
    appAt f args index = normaliseApp f =<< traverse (appIndexToArg index) args

    appIndexToArg ::
      (MonadUnblock m) =>
      WHNFValue QueryBuiltin ->
      (WHNFArg QueryBuiltin, WHNFArg QueryBuiltin, WHNFValue QueryBuiltin) ->
      m (WHNFArg QueryBuiltin)
    appIndexToArg index (t', n', xs) =
      Arg mempty Explicit Relevant
        <$> unblockAt t' n' xs index

unblockIndices ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockIndices n = do
  n' <- unblockNonVector n
  flip liftIf n' $ \n'' ->
    forceEvalSimple Indices (evalIndices (VBuiltinFunction Indices)) (explicit <$> [n''])

--------------------------------------------------------------------------------
-- Purification

tryPurifyAssertion ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  (WHNFValue QueryBuiltin -> m a) ->
  (WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> m a) ->
  m a
tryPurifyAssertion assertion whenImpure whenPure = do
  assertionDoc <- prettyFriendlyInCtx assertion
  logDebug MaxDetail $ line <> "Trying to purify" <+> squotes assertionDoc
  incrCallDepth

  unblockedExpr <- case assertion of
    IEqualRat x y -> purifyRatOp2 IEqualRat (evalEqualsRat Eq) x y
    IOrderRat op x y -> purifyRatOp2 (IOrderRat op) (evalOrderRat op) x y
    IVectorEqualFull (IVecEqSpine t1 t2 n sol xs ys) -> purifyVectorOp2 StdEqualsVector [t1, t2, n, sol] xs ys
    _ -> unexpectedExprError "purifying assertion" assertionDoc

  unblockedAssertionDoc <- prettyFriendlyInCtx unblockedExpr
  logDebug MaxDetail $ "Result" <+> squotes unblockedAssertionDoc

  let onPurified x y = do
        logDebug MaxDetail "No new boolean structure found."
        decrCallDepth
        whenPure x y

  let onUnpurified expr = do
        logDebug MaxDetail "New boolean structure found."
        decrCallDepth
        whenImpure expr

  case unblockedExpr of
    IEqual EqRat x y -> onPurified x y
    IOrder OrderRat _ x y -> onPurified x y
    IVectorEqual xs ys -> onPurified xs ys
    _ -> onUnpurified unblockedExpr

purify ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purify expr = case expr of
  -- Rational operators
  IRatLiteral {} -> return expr
  INeg NegRat x -> purifyNegRat x
  IAdd AddRat x y -> purifyRatOp2 (IAdd AddRat) evalAddRat x y
  ISub SubRat x y -> purifyRatOp2 (ISub SubRat) evalSubRat x y
  IMul MulRat x y -> purifyRatOp2 (IMul MulRat) evalMulRat x y
  IDiv DivRat x y -> purifyRatOp2 (IDiv DivRat) evalDivRat x y
  -- Vector operators
  IVecLiteral t xs -> purifyVectorLiteral t xs
  IStandardLib StdAddVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 StdAddVector [t1, t2, t3, n, s] xs ys
  IStandardLib StdSubVector (IVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 StdSubVector [t1, t2, t3, n, s] xs ys
  IMapVector t1 t2 n f xs -> unblockMapVector t1 t2 n f xs
  VFreeVar ident spine -> unblockNetwork False ident spine
  -- Polymorphic
  VBoundVar _v [] -> return expr
  IIf {} -> return expr
  IFoldVector t1 t2 n f e xs -> unblockFoldVector t1 t2 n f e xs
  IAt t n xs i -> unblockAt t n xs i
  -- Other
  _ -> do
    exprDoc <- prettyFriendlyInCtx expr
    compilerDeveloperError $ "Do not yet support purification of" <+> exprDoc

purifyVectorLiteral ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyVectorLiteral t xs = do
  xs' <- traverse (traverse purify) xs
  liftIfSpine xs' $ \xs'' ->
    return $ IVecLiteral t xs''

purifyVectorOp2 ::
  (MonadUnblock m) =>
  StdLibFunction ->
  WHNFSpine QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyVectorOp2 = traverseVectorOp2 purify

purifyRatOp2 ::
  (MonadUnblock m) =>
  (WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin -> WHNFValue QueryBuiltin) ->
  (WHNFValue QueryBuiltin -> WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin) ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyRatOp2 mkOp evalOp2 x y = do
  x' <- purify x
  y' <- purify y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      return $ evalOp2 (mkOp x'' y'') [explicit x'', explicit y'']

purifyNegRat ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyNegRat x = do
  x' <- purify x
  flip liftIf x' $ \x'' ->
    return $ evalNegRat (INeg NegRat x'') [explicit x'']

traverseVectorOp2 ::
  (MonadUnblock m) =>
  (WHNFValue QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  StdLibFunction ->
  WHNFSpine QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
traverseVectorOp2 f fn spinePrefix xs ys = do
  xs' <- f xs
  ys' <- f ys
  flip liftIf xs' $ \xs'' ->
    flip liftIf ys' $ \ys'' -> do
      let newSpine = spinePrefix <> (Arg mempty Explicit Relevant <$> [xs'', ys''])
      case (xs'', ys'') of
        (IVecLiteral {}, IVecLiteral {}) -> appHiddenStdlibDef fn newSpine
        _ -> return $ IStandardLib fn newSpine

forceEval ::
  (MonadCompile m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
forceEval b evalFn args = evalFn cannotEvalError $ filterOutIrrelevantArgs args
  where
    cannotEvalError = developerError $ "Unexpectedly blocked expression" <+> prettyVerbose (VBuiltin (BuiltinFunction b) args)

forceEvalSimple ::
  (MonadCompile m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> WHNFSpine QueryBuiltin -> WHNFValue QueryBuiltin) ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
forceEvalSimple b evalFn = forceEval b (\orig args -> return $ evalFn orig args)

reduceBoundVar :: (MonadQueryStructure m) => Lv -> m (WHNFValue QueryBuiltin)
reduceBoundVar lv = do
  maybeReduction <- getReducedVariableExprFor lv
  case maybeReduction of
    Just vectorReduction -> return vectorReduction
    Nothing -> return $ VBoundVar lv []

isRatTensor :: WHNFType QueryBuiltin -> Bool
isRatTensor = \case
  IRatType {} -> True
  IVectorType _ tElem _ -> isRatTensor tElem
  _ -> False
