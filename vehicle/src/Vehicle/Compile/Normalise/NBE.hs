{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf,
    eval,
    evalApp,
    evalBuiltin,
    liftEnvOverBinder,
    forceExpr,
    reeval,
  )
where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Foldable (foldrM)
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map qualified as Map (lookup)
import Data.Maybe (isJust)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.VariableContext (DeclSubstitution, MetaSubstitution)
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

whnf ::
  MonadCompile m =>
  DBLevel ->
  DeclSubstitution ->
  MetaSubstitution ->
  CheckedExpr ->
  m NormExpr
whnf dbLevel declCtx metaSubst e = do
  let env = mkNoOpEnv dbLevel
  runReaderT (eval env e) (declCtx, metaSubst)

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm m =
  ( MonadCompile m,
    MonadReader (DeclSubstitution, MetaSubstitution) m
  )

-- TODO change to return a tuple of NF and WHNF?
eval :: MonadNorm m => Env -> CheckedExpr -> m NormExpr
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Literal _ l -> return $ VLiteral l
    Ann _ e _ -> eval env e
    LVec _ xs -> VLVec <$> traverse (eval env) xs <*> pure []
    Lam _ binder body -> do
      binder' <- evalBinder env binder
      return $ VLam binder' env body
    Pi _ binder body ->
      VPi <$> evalBinder env binder <*> eval (liftEnvOverBinder env) body
    Var _ v -> case v of
      Bound i -> i `lookupIn` env
      Free ident -> do
        (declCtx, _) <- ask
        let declExpr = Map.lookup ident declCtx
        return $ case declExpr of
          Just x -> x
          Nothing -> VFreeVar ident []
    Let _ bound _binder body -> do
      boundNormExpr <- eval env bound
      eval (boundNormExpr : env) body
    App _ fun args -> do
      fun' <- eval env fun
      args' <- traverse (traverse (eval env)) args
      evalApp fun' (NonEmpty.toList args')

  showExit result
  return result

evalBinder :: MonadNorm m => Env -> CheckedBinder -> m NormBinder
evalBinder env = traverse (eval env)

evalApp :: MonadNorm m => NormExpr -> Spine -> m NormExpr
evalApp fun [] = return fun
evalApp fun (arg : args) = case fun of
  VMeta v spine -> return $ VMeta v (spine <> (arg : args))
  VFreeVar v spine -> return $ VFreeVar v (spine <> (arg : args))
  VBoundVar v spine -> return $ VBoundVar v (spine <> (arg : args))
  VLVec xs spine -> return $ VLVec xs (spine <> (arg : args))
  VLam _binder env body -> do
    body' <- eval (argExpr arg : env) body
    case args of
      [] -> return body'
      (a : as) -> evalApp body' (a : as)
  VBuiltin b spine -> evalBuiltin b (spine <> (arg : args))
  VUniverse {} -> unexpectedExprError currentPass "VUniverse"
  VPi {} -> unexpectedExprError currentPass "VPi"
  VLiteral {} -> unexpectedExprError currentPass "VLiteral"

lookupIn :: MonadCompile m => DBIndex -> Env -> m NormExpr
lookupIn i env = case lookupVar env i of
  Just value -> return value
  Nothing ->
    compilerDeveloperError $
      "Environment of size"
        <+> pretty (length env)
        <+> "in which NBE is being performed"
        <+> "is smaller than the found DB index"
        <+> pretty i

-----------------------------------------------------------------------------
-- Full evaluation of builtins

-- TODO Separate builtins from syntactic sugar
--
-- TODO Pass in the right number of arguments ensuring all literals
evalBuiltin :: MonadNorm m => Builtin -> Spine -> m NormExpr
evalBuiltin b args
  | isDerived b = evalDerivedBuiltin b args
  | otherwise = do
      let relevantArgs = filter isRelevant args

      let result = case b of
            Constructor {} -> Nothing
            Quantifier {} -> Nothing
            Not -> evalNot relevantArgs
            And -> evalAnd relevantArgs
            Or -> evalOr relevantArgs
            Neg dom -> evalNeg dom relevantArgs
            Add dom -> evalAdd dom relevantArgs
            Sub dom -> evalSub dom relevantArgs
            Mul dom -> evalMul dom relevantArgs
            Div dom -> evalDiv dom relevantArgs
            Equals dom op -> evalEquals dom op relevantArgs
            Order dom op -> evalOrder dom op relevantArgs
            If -> evalIf relevantArgs
            At -> evalAt relevantArgs
            Fold dom -> evalFold dom relevantArgs
            Foreach -> evalForeach relevantArgs
            FromNat _ dom -> evalFromNat dom relevantArgs
            FromRat dom -> evalFromRat dom relevantArgs
            FromVec _n dom -> evalFromVec dom relevantArgs
            TypeClassOp {} -> Just $ compilerDeveloperError $ "Found derived builtin" <+> pretty b
            Implies -> Just $ compilerDeveloperError $ "Found derived builtin" <+> pretty b
            Map {} -> Just $ compilerDeveloperError $ "Found derived builtin" <+> pretty b

      -- when (b == And) $ do
      --   logDebug MaxDetail $ prettyVerbose (VBuiltin b args)
      --   case result of
      --     Nothing -> logDebug MaxDetail "not normalised"
      --     Just x -> do
      --       x' <- x
      --       logDebug MaxDetail (prettyVerbose x')

      case result of
        Nothing -> return $ VBuiltin b args
        Just r -> r

isDerived :: Builtin -> Bool
isDerived = \case
  TypeClassOp {} -> True
  Implies {} -> True
  Map {} -> True
  _ -> False

evalDerivedBuiltin :: MonadNorm m => Builtin -> Spine -> m NormExpr
evalDerivedBuiltin b args = case b of
  TypeClassOp op -> evalTypeClassOp op args
  Implies -> evalImplies args
  Map dom -> evalMap dom args
  _ -> compilerDeveloperError $ "Invalid derived builtin" <+> quotePretty b

-----------------------------------------------------------------------------
-- Reevaluation

reeval :: MonadNorm m => NormExpr -> m NormExpr
reeval expr = case expr of
  VUniverse {} -> return expr
  VLiteral {} -> return expr
  VLam {} -> return expr
  VPi {} -> return expr
  VLVec xs spine -> VLVec <$> traverse reeval xs <*> reevalSpine spine
  VMeta m spine -> VMeta m <$> reevalSpine spine
  VFreeVar v spine -> VFreeVar v <$> reevalSpine spine
  VBoundVar v spine -> VBoundVar v <$> reevalSpine spine
  VBuiltin b spine -> evalApp (VBuiltin b []) =<< reevalSpine spine

reevalSpine :: MonadNorm m => Spine -> m Spine
reevalSpine = traverse (traverse reeval)

-----------------------------------------------------------------------------
-- Indvidual builtins

type EvalBuiltin = forall m. MonadNorm m => Spine -> Maybe (m NormExpr)

pattern VBool :: Bool -> GenericArg NormExpr
pattern VBool x <- ExplicitArg _ (VLiteral (LBool x))

pattern VIndex :: Int -> GenericArg NormExpr
pattern VIndex x <- ExplicitArg _ (VLiteral (LIndex _ x))

pattern VNat :: Int -> GenericArg NormExpr
pattern VNat x <- ExplicitArg _ (VLiteral (LNat x))

pattern VInt :: Int -> GenericArg NormExpr
pattern VInt x <- ExplicitArg _ (VLiteral (LInt x))

pattern VRat :: Rational -> GenericArg NormExpr
pattern VRat x <- ExplicitArg _ (VLiteral (LRat x))

-- TODO a lot of duplication in the below. Once we have separated out the
-- derived builtins we should be able to

evalNot :: EvalBuiltin
evalNot e = case e of
  [VBool x] -> Just $ return $ VLiteral (LBool (not x))
  _ -> Nothing

evalAnd :: EvalBuiltin
evalAnd = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral (LBool (x && y))
  _ -> Nothing

evalOr :: EvalBuiltin
evalOr = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral (LBool (x && y))
  _ -> Nothing

evalNeg :: NegDomain -> EvalBuiltin
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalBuiltin
evalNegInt = \case
  [VInt x] -> Just $ return $ VLiteral (LInt (-x))
  _ -> Nothing

evalNegRat :: EvalBuiltin
evalNegRat = \case
  [VRat x] -> Just $ return $ VLiteral (LRat (-x))
  _ -> Nothing

evalAdd :: AddDomain -> EvalBuiltin
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalBuiltin
evalAddNat = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LNat (x + y))
  _ -> Nothing

evalAddInt :: EvalBuiltin
evalAddInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x + y))
  _ -> Nothing

evalAddRat :: EvalBuiltin
evalAddRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x + y))
  _ -> Nothing

evalSub :: SubDomain -> EvalBuiltin
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalBuiltin
evalSubInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x - y))
  _ -> Nothing

evalSubRat :: EvalBuiltin
evalSubRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x - y))
  _ -> Nothing

evalMul :: MulDomain -> EvalBuiltin
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalBuiltin
evalMulNat = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LNat (x * y))
  _ -> Nothing

evalMulInt :: EvalBuiltin
evalMulInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x * y))
  _ -> Nothing

evalMulRat :: EvalBuiltin
evalMulRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x * y))
  _ -> Nothing

evalDiv :: DivDomain -> EvalBuiltin
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalBuiltin
evalDivRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x * y))
  _ -> Nothing

evalOrder :: OrderDomain -> OrderOp -> EvalBuiltin
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalBuiltin
evalOrderIndex op = \case
  [VIndex x, VIndex y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderNat :: OrderOp -> EvalBuiltin
evalOrderNat op = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderInt :: OrderOp -> EvalBuiltin
evalOrderInt op = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderRat :: OrderOp -> EvalBuiltin
evalOrderRat op = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalEquals :: EqualityDomain -> EqualityOp -> EvalBuiltin
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalBuiltin
evalEqualityIndex op = \case
  [_, _, VIndex x, VIndex y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityNat :: EqualityOp -> EvalBuiltin
evalEqualityNat op = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityInt :: EqualityOp -> EvalBuiltin
evalEqualityInt op = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityRat :: EqualityOp -> EvalBuiltin
evalEqualityRat op = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalFromNat :: FromNatDomain -> EvalBuiltin
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalBuiltin
evalFromNatToIndex = \case
  [ImplicitArg _ (VLiteral (LNat n)), VNat x] -> Just $ return $ VLiteral $ LIndex n x
  _ -> Nothing

evalFromNatToNat :: EvalBuiltin
evalFromNatToNat = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalFromNatToInt :: EvalBuiltin
evalFromNatToInt = \case
  [VNat x] -> Just $ return $ VLiteral $ LInt x
  _ -> Nothing

evalFromNatToRat :: EvalBuiltin
evalFromNatToRat = \case
  [VNat x] -> Just $ return $ VLiteral $ LRat (fromIntegral x)
  _ -> Nothing

evalFromRat :: FromRatDomain -> EvalBuiltin
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalBuiltin
evalFromRatToRat = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalIf :: EvalBuiltin
evalIf = \case
  [_, VBool True, e1, _e2] -> Just $ return $ argExpr e1
  [_, VBool False, _e1, e2] -> Just $ return $ argExpr e2
  _ -> Nothing

evalAt :: EvalBuiltin
evalAt = \case
  [_, _, ExplicitArg _ (VLVec es _), VIndex i] -> Just $ return $ es !! fromIntegral i
  _ -> Nothing

evalFold :: FoldDomain -> EvalBuiltin
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList :: EvalBuiltin
evalFoldList = \case
  [_, _, _f, e, ExplicitArg _ (VConstructor Nil [_])] ->
    Just $ return $ argExpr e
  [toT, fromT, f, e, ExplicitArg _ (VConstructor Cons [_, x, xs'])] -> Just $ do
    r <- evalBuiltin (Fold FoldList) [toT, fromT, f, e, xs']
    evalApp (argExpr f) [x, ExplicitArg mempty r]
  _ -> Nothing

evalFoldVector :: EvalBuiltin
evalFoldVector = \case
  [_, _, _, f, e, ExplicitArg _ (VLVec v _)] ->
    Just $
      foldrM f' (argExpr e) v
    where
      f' x r = evalApp (argExpr f) [ExplicitArg mempty x, ExplicitArg mempty r]
  _ -> Nothing

evalForeach :: EvalBuiltin
evalForeach = \case
  [tRes, ImplicitArg _ (VLiteral (LNat n)), ExplicitArg _ f] -> Just $ do
    let fn i = evalApp f [ExplicitArg mempty (VLiteral (LIndex n i))]
    xs <- traverse fn [0 .. (n - 1 :: Int)]
    return $ mkVLVec xs (argExpr tRes)
  _ -> Nothing

evalFromVec :: FromVecDomain -> EvalBuiltin
evalFromVec = \case
  FromVecToVec -> evalFromVecToVec
  FromVecToList -> evalFromVecToList

evalFromVecToList :: EvalBuiltin
evalFromVecToList args = case args of
  [tElem, ExplicitArg _ (VLVec xs _)] -> Just $ return $ mkVList (argExpr tElem) xs
  _ -> Nothing

evalFromVecToVec :: EvalBuiltin
evalFromVecToVec = \case
  [_, ExplicitArg _ e] -> Just $ return e
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived = forall m. MonadNorm m => Spine -> m NormExpr

-- TODO define in terms of language

evalTypeClassOp :: TypeClassOp -> EvalDerived
evalTypeClassOp _op args = do
  let (inst, remainingArgs) = findInstanceArg args
  evalApp inst remainingArgs

evalImplies :: EvalDerived
evalImplies = \case
  [e1, e2] -> do
    ne1 <- ExplicitArg mempty <$> evalBuiltin Not [e1]
    evalBuiltin Or [ne1, e2]
  args -> return $ VBuiltin Implies args

evalMap :: MapDomain -> EvalDerived
evalMap = \case
  MapList -> evalMapList
  MapVector -> evalMapVec

evalMapList :: EvalDerived
evalMapList = \case
  [_, tTo, _f, ExplicitArg _ (VConstructor Nil _)] ->
    return $ VConstructor Nil [tTo]
  [tFrom, tTo, f, ExplicitArg _ (VConstructor Cons [x, xs])] -> do
    x' <- ExplicitArg mempty <$> evalApp (argExpr f) [x]
    xs' <- ExplicitArg mempty <$> evalMapList [tFrom, tTo, f, xs]
    return $ VConstructor Cons [tTo, x', xs']
  args -> return $ VBuiltin (Map MapList) args

evalMapVec :: EvalDerived
evalMapVec = \case
  [_n, _t1, t2, ExplicitArg _ f, ExplicitArg _ (VLVec xs _)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg mempty x]) xs
    return $ mkVLVec xs' (argExpr t2)
  args -> return $ VBuiltin (Map MapVector) args

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr :: forall m. MonadNorm m => NormExpr -> m (Maybe NormExpr, MetaSet)
forceExpr = go
  where
    go :: NormExpr -> m (Maybe NormExpr, MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> goBuiltin b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> Spine -> m (Maybe NormExpr, MetaSet)
    goMeta m spine = do
      (_, metaSubst) <- ask
      case MetaMap.lookup m metaSubst of
        Just solution -> do
          normMetaExpr <- evalApp (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

    goBuiltin :: Builtin -> Spine -> m (Maybe NormExpr, MetaSet)
    goBuiltin b spine = case b of
      Constructor {} -> return (Nothing, mempty)
      Foreach -> return (Nothing, mempty)
      TypeClassOp {} -> return (Nothing, mempty)
      _ -> do
        (argResults, argsReduced, argBlockingMetas) <- unzip3 <$> traverse goBuiltinArg spine
        let anyArgsReduced = or argsReduced
        let blockingMetas = MetaSet.unions argBlockingMetas
        result <-
          if not anyArgsReduced
            then return Nothing
            else do
              Just <$> evalBuiltin b argResults
        return (result, blockingMetas)

    goBuiltinArg :: NormArg -> m (NormArg, Bool, MetaSet)
    goBuiltinArg arg
      -- We assume that non-explicit args aren't depended on computationally
      -- (this may not hold in future.)
      | not (isExplicit arg) = return (arg, False, mempty)
      | otherwise = do
          (maybeResult, blockingMetas) <- go (argExpr arg)
          let result = maybe arg (`replaceArgExpr` arg) maybeResult
          let reduced = isJust maybeResult
          return (result, reduced, blockingMetas)

{-
-----------------------------------------------------------------------------
-- Blocking

-- | A generic scheme for evaluating builtins.
type EvaluationBlocker = NormExpr -> Bool

-- | A manually blocked expression. This is a massive hack that relies on us
-- constructing a lambda expression that can never be written by the user,
-- and therefore never generated by user code. We may eventually need to
-- break this out into its own constructor somehow.
pattern BlockedExpr :: Provenance -> Env -> DBExpr -> NormExpr
pattern BlockedExpr p env expr = VLam p BlockingBinder env expr

pattern BlockingBinder :: NormBinder
pattern BlockingBinder <- Binder _ BlockingBinderDisplayFrom Explicit Relevant () (VLiteral _ LUnit)
  where BlockingBinder = Binder mempty BlockingBinderDisplayFrom Explicit Relevant () (VLiteral mempty LUnit)

pattern BlockingBinderDisplayFrom :: BinderDisplayForm
pattern BlockingBinderDisplayFrom = BinderDisplayForm (OnlyName "@BLOCKED@") True

noBlocking :: EvaluationBlocker
noBlocking = const False

blockOnBooleanPropositions :: EvaluationBlocker
blockOnBooleanPropositions = \case
  VBuiltin _ b _ -> case b of
    Equals{} -> True
    Order{} -> True
    _ -> False
  _ -> False
-}
-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: MonadNorm m => Env -> CheckedExpr -> m ()
showEntry _env _expr = do
  -- logDebug MaxDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { env=" <+> prettyVerbose env <+> "}")
  incrCallDepth

showExit :: MonadNorm m => NormExpr -> m ()
showExit _result = do
  decrCallDepth

-- logDebug  MaxDetail ("nbe-exit" <+> prettyVerbose result)
