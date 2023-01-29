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
    Meta p m -> return $ VMeta p m []
    Universe p u -> return $ VUniverse p u
    Builtin p b -> return $ VBuiltin p b []
    Literal p l -> return $ VLiteral p l
    Ann _ e _ -> eval env e
    LVec p xs -> VLVec p <$> traverse (eval env) xs <*> pure []
    Lam p binder body -> do
      binder' <- evalBinder env binder
      return $ VLam p binder' env body
    Pi p binder body ->
      VPi p <$> evalBinder env binder <*> eval (liftEnvOverBinder p env) body
    Var p v -> case v of
      Bound i -> i `lookupIn` env
      Free ident -> do
        (declCtx, _) <- ask
        let declExpr = Map.lookup ident declCtx
        return $ case declExpr of
          Just x -> x
          Nothing -> VFreeVar p ident []
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
  VMeta p v spine -> return $ VMeta p v (spine <> (arg : args))
  VFreeVar p v spine -> return $ VFreeVar p v (spine <> (arg : args))
  VBoundVar p v spine -> return $ VBoundVar p v (spine <> (arg : args))
  VLVec p xs spine -> return $ VLVec p xs (spine <> (arg : args))
  VLam _ _binder env body -> do
    body' <- eval (argExpr arg : env) body
    case args of
      [] -> return body'
      (a : as) -> evalApp body' (a : as)
  VBuiltin p b spine -> evalBuiltin p b (spine <> (arg : args))
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

evalBuiltin :: MonadNorm m => Provenance -> Builtin -> Spine -> m NormExpr
evalBuiltin p b args = case b of
  -- TODO rearrange builtin constructors so we don't have to do this.

  -- Derived
  TypeClassOp op -> evalTypeClassOp op p args
  Implies -> evalImplies p args
  Map dom -> evalMap dom p args
  _ -> do
    let relevantArgs = filter isRelevant args
    let result = case b of
          Constructor {} -> Nothing
          Quantifier {} -> Nothing
          Not -> evalNot p relevantArgs
          And -> evalAnd p relevantArgs
          Or -> evalOr p relevantArgs
          Neg dom -> evalNeg dom p relevantArgs
          Add dom -> evalAdd dom p relevantArgs
          Sub dom -> evalSub dom p relevantArgs
          Mul dom -> evalMul dom p relevantArgs
          Div dom -> evalDiv dom p relevantArgs
          Equals dom op -> evalEquals dom op p relevantArgs
          Order dom op -> evalOrder dom op p relevantArgs
          If -> evalIf p relevantArgs
          At -> evalAt p relevantArgs
          Fold dom -> evalFold dom p relevantArgs
          Foreach -> evalForeach p relevantArgs
          FromNat _ dom -> evalFromNat dom p relevantArgs
          FromRat dom -> evalFromRat dom p relevantArgs
          FromVec _n dom -> evalFromVec dom p relevantArgs

    -- when (b == And) $ do
    --   logDebug MaxDetail $ prettyVerbose (VBuiltin p b args)
    --   case result of
    --     Nothing -> logDebug MaxDetail "not normalised"
    --     Just x -> do
    --       x' <- x
    --       logDebug MaxDetail (prettyVerbose x')

    case result of
      Nothing -> return $ VBuiltin p b args
      Just r -> r

-----------------------------------------------------------------------------
-- Reevaluation

reeval :: MonadNorm m => NormExpr -> m NormExpr
reeval expr = case expr of
  VUniverse {} -> return expr
  VLiteral {} -> return expr
  VLam {} -> return expr
  VPi {} -> return expr
  VLVec p xs spine -> VLVec p <$> traverse reeval xs <*> reevalSpine spine
  VMeta p m spine -> VMeta p m <$> reevalSpine spine
  VFreeVar p v spine -> VFreeVar p v <$> reevalSpine spine
  VBoundVar p v spine -> VBoundVar p v <$> reevalSpine spine
  VBuiltin p b spine -> evalApp (VBuiltin p b []) =<< reevalSpine spine

reevalSpine :: MonadNorm m => Spine -> m Spine
reevalSpine = traverse (traverse reeval)

-----------------------------------------------------------------------------
-- Indvidual builtins

type EvalBuiltin = forall m. MonadNorm m => Provenance -> Spine -> Maybe (m NormExpr)

pattern VBool :: Bool -> GenericArg NormExpr
pattern VBool x <- ExplicitArg _ (VLiteral _ (LBool x))

pattern VIndex :: Int -> GenericArg NormExpr
pattern VIndex x <- ExplicitArg _ (VLiteral _ (LIndex _ x))

pattern VNat :: Int -> GenericArg NormExpr
pattern VNat x <- ExplicitArg _ (VLiteral _ (LNat x))

pattern VInt :: Int -> GenericArg NormExpr
pattern VInt x <- ExplicitArg _ (VLiteral _ (LInt x))

pattern VRat :: Rational -> GenericArg NormExpr
pattern VRat x <- ExplicitArg _ (VLiteral _ (LRat x))

-- TODO a lot of duplication in the below. Once we have separated out the
-- derived builtins we should be able to

evalNot :: EvalBuiltin
evalNot p e = case e of
  [VBool x] -> Just $ return $ VLiteral p (LBool (not x))
  _ -> Nothing

evalAnd :: EvalBuiltin
evalAnd p = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral p (LBool (x && y))
  _ -> Nothing

evalOr :: EvalBuiltin
evalOr p = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral p (LBool (x && y))
  _ -> Nothing

evalNeg :: NegDomain -> EvalBuiltin
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalBuiltin
evalNegInt p = \case
  [VInt x] -> Just $ return $ VLiteral p (LInt (-x))
  _ -> Nothing

evalNegRat :: EvalBuiltin
evalNegRat p = \case
  [VRat x] -> Just $ return $ VLiteral p (LRat (-x))
  _ -> Nothing

evalAdd :: AddDomain -> EvalBuiltin
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalBuiltin
evalAddNat p = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral p (LNat (x + y))
  _ -> Nothing

evalAddInt :: EvalBuiltin
evalAddInt p = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral p (LInt (x + y))
  _ -> Nothing

evalAddRat :: EvalBuiltin
evalAddRat p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LRat (x + y))
  _ -> Nothing

evalSub :: SubDomain -> EvalBuiltin
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalBuiltin
evalSubInt p = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral p (LInt (x - y))
  _ -> Nothing

evalSubRat :: EvalBuiltin
evalSubRat p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LRat (x - y))
  _ -> Nothing

evalMul :: MulDomain -> EvalBuiltin
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalBuiltin
evalMulNat p = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral p (LNat (x * y))
  _ -> Nothing

evalMulInt :: EvalBuiltin
evalMulInt p = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral p (LInt (x * y))
  _ -> Nothing

evalMulRat :: EvalBuiltin
evalMulRat p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LRat (x * y))
  _ -> Nothing

evalDiv :: DivDomain -> EvalBuiltin
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalBuiltin
evalDivRat p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LRat (x * y))
  _ -> Nothing

evalOrder :: OrderDomain -> OrderOp -> EvalBuiltin
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalBuiltin
evalOrderIndex op p = \case
  [VIndex x, VIndex y] -> Just $ return $ VLiteral p (LBool (orderOp op x y))
  _ -> Nothing

evalOrderNat :: OrderOp -> EvalBuiltin
evalOrderNat op p = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral p (LBool (orderOp op x y))
  _ -> Nothing

evalOrderInt :: OrderOp -> EvalBuiltin
evalOrderInt op p = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral p (LBool (orderOp op x y))
  _ -> Nothing

evalOrderRat :: OrderOp -> EvalBuiltin
evalOrderRat op p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LBool (orderOp op x y))
  _ -> Nothing

evalEquals :: EqualityDomain -> EqualityOp -> EvalBuiltin
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalBuiltin
evalEqualityIndex op p = \case
  [_, _, VIndex x, VIndex y] -> Just $ return $ VLiteral p (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityNat :: EqualityOp -> EvalBuiltin
evalEqualityNat op p = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral p (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityInt :: EqualityOp -> EvalBuiltin
evalEqualityInt op p = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral p (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityRat :: EqualityOp -> EvalBuiltin
evalEqualityRat op p = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral p (LBool (equalityOp op x y))
  _ -> Nothing

evalFromNat :: FromNatDomain -> EvalBuiltin
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalBuiltin
evalFromNatToIndex p = \case
  [ImplicitArg _ (VLiteral _ (LNat n)), VNat x] -> Just $ return $ VLiteral p $ LIndex n x
  _ -> Nothing

evalFromNatToNat :: EvalBuiltin
evalFromNatToNat _p = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalFromNatToInt :: EvalBuiltin
evalFromNatToInt p = \case
  [VNat x] -> Just $ return $ VLiteral p $ LInt x
  _ -> Nothing

evalFromNatToRat :: EvalBuiltin
evalFromNatToRat p = \case
  [VNat x] -> Just $ return $ VLiteral p $ LRat (fromIntegral x)
  _ -> Nothing

evalFromRat :: FromRatDomain -> EvalBuiltin
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalBuiltin
evalFromRatToRat _ = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalIf :: EvalBuiltin
evalIf _p = \case
  [_, VBool True, e1, _e2] -> Just $ return $ argExpr e1
  [_, VBool False, _e1, e2] -> Just $ return $ argExpr e2
  _ -> Nothing

evalAt :: EvalBuiltin
evalAt _p = \case
  [_, _, ExplicitArg _ (VLVec _ es _), VIndex i] -> Just $ return $ es !! fromIntegral i
  _ -> Nothing

evalFold :: FoldDomain -> EvalBuiltin
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList :: EvalBuiltin
evalFoldList p = \case
  [_, _, _f, e, ExplicitArg _ (VConstructor _ Nil [_])] ->
    Just $ return $ argExpr e
  [toT, fromT, f, e, ExplicitArg _ (VConstructor _ Cons [_, x, xs'])] -> Just $ do
    r <- evalBuiltin p (Fold FoldList) [toT, fromT, f, e, xs']
    evalApp (argExpr f) [x, ExplicitArg p r]
  _ -> Nothing

evalFoldVector :: EvalBuiltin
evalFoldVector p = \case
  [_, _, _, f, e, ExplicitArg _ (VLVec _ v _)] ->
    Just $
      foldrM f' (argExpr e) v
    where
      f' x r = evalApp (argExpr f) [ExplicitArg p x, ExplicitArg p r]
  _ -> Nothing

evalForeach :: EvalBuiltin
evalForeach p = \case
  [tRes, ImplicitArg _ (VLiteral _ (LNat n)), ExplicitArg _ f] -> Just $ do
    let fn i = evalApp f [ExplicitArg p (VLiteral p (LIndex n i))]
    xs <- traverse fn [0 .. (n - 1 :: Int)]
    return $ mkVLVec p xs (argExpr tRes)
  _ -> Nothing

evalFromVec :: FromVecDomain -> EvalBuiltin
evalFromVec = \case
  FromVecToVec -> evalFromVecToVec
  FromVecToList -> evalFromVecToList

evalFromVecToList :: EvalBuiltin
evalFromVecToList p args = case args of
  [tElem, ExplicitArg _ (VLVec _ xs _)] -> Just $ return $ mkVList p (argExpr tElem) xs
  _ -> Nothing

evalFromVecToVec :: EvalBuiltin
evalFromVecToVec _p = \case
  [_, ExplicitArg _ e] -> Just $ return e
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived = forall m. MonadNorm m => Provenance -> Spine -> m NormExpr

-- TODO define in terms of language

evalTypeClassOp :: TypeClassOp -> EvalDerived
evalTypeClassOp _p _op args = do
  let (inst, remainingArgs) = findInstanceArg args
  evalApp inst remainingArgs

evalImplies :: EvalDerived
evalImplies p = \case
  [e1, e2] -> do
    ne1 <- ExplicitArg p <$> evalBuiltin p Not [e1]
    evalBuiltin p Or [ne1, e2]
  args -> return $ VBuiltin p Implies args

evalMap :: MapDomain -> EvalDerived
evalMap = \case
  MapList -> evalMapList
  MapVector -> evalMapVec

evalMapList :: EvalDerived
evalMapList p = \case
  [_, tTo, _f, ExplicitArg _ (VConstructor _ Nil _)] ->
    return $ VConstructor p Nil [tTo]
  [tFrom, tTo, f, ExplicitArg _ (VConstructor _ Cons [x, xs])] -> do
    x' <- ExplicitArg p <$> evalApp (argExpr f) [x]
    xs' <- ExplicitArg p <$> evalMapList p [tFrom, tTo, f, xs]
    return $ VConstructor p Cons [tTo, x', xs']
  args -> return $ VBuiltin p (Map MapList) args

evalMapVec :: EvalDerived
evalMapVec p = \case
  [_n, _t1, t2, ExplicitArg _ f, ExplicitArg _ (VLVec _ xs _)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg p x]) xs
    return $ mkVLVec p xs' (argExpr t2)
  args -> return $ VBuiltin p (Map MapVector) args

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr :: forall m. MonadNorm m => NormExpr -> m (Maybe NormExpr, MetaSet)
forceExpr = go
  where
    go :: NormExpr -> m (Maybe NormExpr, MetaSet)
    go = \case
      VMeta _ m spine -> goMeta m spine
      VBuiltin p b spine -> goBuiltin p b spine
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

    goBuiltin :: Provenance -> Builtin -> Spine -> m (Maybe NormExpr, MetaSet)
    goBuiltin p b spine = case b of
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
              Just <$> evalBuiltin p b argResults
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
