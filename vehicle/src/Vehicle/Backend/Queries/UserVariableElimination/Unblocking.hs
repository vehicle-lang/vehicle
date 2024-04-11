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
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Compile.Resource (NetworkTensorType (..), NetworkType (..))
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
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
  VBoolLiteral {} -> return expr
  VIndexLiteral {} -> return expr
  VNatLiteral {} -> return expr
  VRatLiteral {} -> return expr
  VAnd {} -> return expr
  VOr {} -> return expr
  VNot {} -> return expr
  VIf {} -> return expr
  VForall {} -> return expr
  VExists {} -> return expr
  VVectorEqualFull spine@(VVecEqSpine t _ _ _ _ _)
    | isRatTensor (argExpr t) -> return expr
    | otherwise -> appHiddenStdlibDef StdEqualsVector spine
  VVectorNotEqualFull spine@(VVecEqSpine t _ _ _ _ _)
    | isRatTensor (argExpr t) -> return expr
    | otherwise -> appHiddenStdlibDef StdNotEqualsVector spine
  VEqualOp dom op x y
    | dom == EqRat -> return expr
    | otherwise -> unblockNonVectorOp2 (Equals dom op) (evalEquals dom op) x y
  VOrder dom op x y
    | dom == OrderRat -> return expr
    | otherwise -> unblockNonVectorOp2 (Order dom op) (evalOrder dom op) x y
  VAt xs i -> unblockAt xs i
  VFoldVector f e xs -> unblockFoldVector f e xs
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
  VVecLiteral {} -> return expr
  VIf {} -> return expr
  VStandardLib StdAddVector (VVecOp2Spine t1 t2 t3 n s xs ys) -> unblockVectorOp2 reduceVectorVars StdAddVector [t1, t2, t3, n, s] xs ys
  VStandardLib StdSubVector (VVecOp2Spine t1 t2 t3 n s xs ys) -> unblockVectorOp2 reduceVectorVars StdSubVector [t1, t2, t3, n, s] xs ys
  VFreeVar ident spine -> unblockNetwork reduceVectorVars ident spine
  VMapVector f xs -> unblockMapVector f xs
  VFoldVector f e xs -> unblockFoldVector f e xs
  VBuiltinFunction ZipWithVector [_, _, _, _, f, xs, ys] -> unblockZipWith f (argExpr xs) (argExpr ys)
  VAt xs i -> unblockAt xs i
  VBuiltinFunction Indices [n] -> unblockIndices (argExpr n)
  _ -> unexpectedExprError "unblocking vector" (prettyVerbose expr)

--------------------------------------------------------------------------------
-- Unblocking operations

unblockNonVectorOp2 ::
  (MonadUnblock m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> [WHNFValue QueryBuiltin] -> WHNFValue QueryBuiltin) ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockNonVectorOp2 b evalOp2 x y = do
  x' <- unblockNonVector x
  y' <- unblockNonVector y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      forceEvalSimple b evalOp2 [x'', y'']

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
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockFoldVector f e xs = do
  xs' <- unblockVector True xs
  flip liftIf xs' $ \xs'' ->
    forceEval FoldVector (evalFoldVector normaliseApp) [f, e, xs'']

unblockMapVector ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockMapVector f xs = do
  xs' <- unblockVector True xs
  flip liftIf xs' $ \xs'' ->
    forceEval MapVector (evalMapVector normaliseApp) [f, xs'']

unblockZipWith ::
  (MonadUnblock m) =>
  WHNFArg QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockZipWith f xs ys = do
  xs' <- unblockVector True xs
  ys' <- unblockVector True ys
  flip liftIf xs' $ \xs'' ->
    flip liftIf ys' $ \ys'' ->
      forceEval ZipWithVector (evalZipWith normaliseApp) [argExpr f, xs'', ys'']

unblockAt ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockAt c i = case c of
  VVecLiteral {} -> do
    i' <- unblockNonVector i
    flip liftIf i' $ \i'' -> do
      forceEvalSimple At evalAt [c, i'']
  VBuiltinFunction MapVector [t, _, n, f, xs] -> appAt f [(t, n, xs)] i
  VBuiltinFunction ZipWithVector [t1, t2, _, n, f, xs, ys] -> appAt f [(t1, n, xs), (t2, n, ys)] i
  VStandardLib StdAddVector [t1, t2, _, n, f, xs, ys] -> appAt f [(t1, n, xs), (t2, n, ys)] i
  VStandardLib StdSubVector [t1, t2, _, n, f, xs, ys] -> appAt f [(t1, n, xs), (t2, n, ys)] i
  _ -> do
    -- Don't reduce vector vars in container as it may trigger extremely expensive normalisation
    -- that we can avoid because we're only looking up a single element of it.
    c' <- unblockVector (isVBoundVar c) c
    unblockAt c' i
  where
    appAt ::
      (MonadUnblock m) =>
      WHNFArg QueryBuiltin ->
      [(WHNFArg QueryBuiltin, WHNFArg QueryBuiltin, WHNFArg QueryBuiltin)] ->
      WHNFValue QueryBuiltin ->
      m (WHNFValue QueryBuiltin)
    appAt f args index = normaliseApp (argExpr f) =<< traverse (appIndexToArg index) args

    appIndexToArg ::
      (MonadUnblock m) =>
      WHNFValue QueryBuiltin ->
      (WHNFArg QueryBuiltin, WHNFArg QueryBuiltin, WHNFArg QueryBuiltin) ->
      m (WHNFArg QueryBuiltin)
    appIndexToArg index (_t, _n, xs) =
      Arg mempty Explicit Relevant
        <$> unblockAt (argExpr xs) index

unblockIndices ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
unblockIndices n = do
  n' <- unblockNonVector n
  flip liftIf n' $ \n'' ->
    forceEvalSimple Indices evalIndices [n'']

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
    VEqualRat x y -> purifyRatOp2 VEqualRat (evalEqualityRat Eq) x y
    VOrderRat op x y -> purifyRatOp2 (VOrderRat op) (evalOrderRat op) x y
    VVectorEqualFull (VVecEqSpine t1 t2 n sol xs ys) -> purifyVectorOp2 StdEqualsVector [t1, t2, n, sol] xs ys
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
    VEqual EqRat x y -> onPurified x y
    VOrder OrderRat _ x y -> onPurified x y
    VVectorEqual xs ys -> onPurified xs ys
    _ -> onUnpurified unblockedExpr

purify ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purify expr = case expr of
  -- Rational operators
  VRatLiteral {} -> return expr
  VNeg NegRat x -> purifyNegRat x
  VAdd AddRat x y -> purifyRatOp2 (VAdd AddRat) evalAddRat x y
  VSub SubRat x y -> purifyRatOp2 (VSub SubRat) evalSubRat x y
  VMul MulRat x y -> purifyRatOp2 (VMul MulRat) evalMulRat x y
  VDiv DivRat x y -> purifyRatOp2 (VDiv DivRat) evalDivRat x y
  -- Vector operators
  VConstructor (LVec n) (t : xs) -> purifyVectorLiteral n t xs
  VStandardLib StdAddVector (VVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 StdAddVector [t1, t2, t3, n, s] xs ys
  VStandardLib StdSubVector (VVecOp2Spine t1 t2 t3 n s xs ys) -> purifyVectorOp2 StdSubVector [t1, t2, t3, n, s] xs ys
  VMapVector f xs -> unblockMapVector f xs
  VFreeVar ident spine -> unblockNetwork False ident spine
  -- Polymorphic
  VBoundVar _v [] -> return expr
  VIf {} -> return expr
  VFoldVector f e xs -> unblockFoldVector f e xs
  VAt xs i -> unblockAt xs i
  -- Other
  _ -> do
    exprDoc <- prettyFriendlyInCtx expr
    compilerDeveloperError $ "Do not yet support purification of" <+> exprDoc

purifyVectorLiteral ::
  (MonadUnblock m) =>
  Int ->
  WHNFArg QueryBuiltin ->
  WHNFSpine QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyVectorLiteral n t xs = do
  xs' <- traverse (traverse purify) xs
  liftIfSpine xs' $ \xs'' ->
    return $ VConstructor (LVec n) (t : xs'')

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
  (WHNFValue QueryBuiltin -> [WHNFValue QueryBuiltin] -> WHNFValue QueryBuiltin) ->
  WHNFValue QueryBuiltin ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyRatOp2 mkOp evalOp2 x y = do
  x' <- purify x
  y' <- purify y
  flip liftIf x' $ \x'' ->
    flip liftIf y' $ \y'' ->
      return $ evalOp2 (mkOp x'' y'') [x'', y'']

purifyNegRat ::
  (MonadUnblock m) =>
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
purifyNegRat x = do
  x' <- purify x
  flip liftIf x' $ \x'' ->
    return $ evalNegRat (VNeg NegRat x'') [x'']

--------------------------------------------------------------------------------
-- If lifting

liftIf ::
  (MonadCompile m) =>
  (WHNFValue QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  WHNFValue QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
liftIf k (VBuiltinFunction If [t, cond, e1, e2]) = do
  e1' <- traverse (liftIf k) e1
  e2' <- traverse (liftIf k) e2
  return $ VBuiltinFunction If [t, cond, e1', e2']
liftIf k e = k e

liftIfArg ::
  (MonadCompile m) =>
  (WHNFArg QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  WHNFArg QueryBuiltin ->
  m (WHNFValue QueryBuiltin)
liftIfArg k (Arg p v r e) = liftIf (k . Arg p v r) e

liftIfSpine ::
  (MonadUnblock m) =>
  WHNFSpine QueryBuiltin ->
  (WHNFSpine QueryBuiltin -> m (WHNFValue QueryBuiltin)) ->
  m (WHNFValue QueryBuiltin)
liftIfSpine [] k = k []
liftIfSpine (x : xs) k = liftIfArg (\a -> liftIfSpine xs (\as -> k (a : as))) x

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
        (VVecLiteral {}, VVecLiteral {}) -> appHiddenStdlibDef fn newSpine
        _ -> return $ VStandardLib fn newSpine

forceEval ::
  (MonadCompile m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> [WHNFValue QueryBuiltin] -> m (WHNFValue QueryBuiltin)) ->
  [WHNFValue QueryBuiltin] ->
  m (WHNFValue QueryBuiltin)
forceEval b evalFn args = evalFn cannotEvalError args
  where
    cannotEvalError = developerError $ "Unexpectedly blocked expression" <+> prettyVerbose (VBuiltin (BuiltinFunction b) (Arg mempty Explicit Relevant <$> args))

forceEvalSimple ::
  (MonadCompile m) =>
  BuiltinFunction ->
  (WHNFValue QueryBuiltin -> [WHNFValue QueryBuiltin] -> WHNFValue QueryBuiltin) ->
  [WHNFValue QueryBuiltin] ->
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
  VRatType -> True
  VVectorType tElem _ -> isRatTensor tElem
  _ -> False
