{-# HLINT ignore "Use <|>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf,
    eval,
    evalApp,
    evalBuiltin,
    extendEnv,
    extendEnvOverBinder,
    forceHead,
    forceArg,
    reeval,
    NormT,
    runNormT,
    runEmptyNormT,
    MonadNorm (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT)
import Data.Foldable (foldrM)
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map qualified as Map (lookup)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

-----------------------------------------------------------------------------
-- Main method

whnf ::
  MonadNorm types m =>
  Env types ->
  NormalisableExpr types ->
  m (NormExpr types)
whnf = eval

-----------------------------------------------------------------------------
-- Normalisation monad

class (MonadCompile m, PrintableBuiltin types) => MonadNorm types m where
  getDeclSubstitution :: m (DeclSubstitution types)
  getMetaSubstitution :: m (MetaSubstitution types)

instance MonadNorm types m => MonadNorm types (StateT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

instance (Monoid s, MonadNorm types m) => MonadNorm types (WriterT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

instance MonadNorm types m => MonadNorm types (ReaderT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

newtype NormT types m a = NormT
  { unnormT :: ReaderT (DeclSubstitution types, MetaSubstitution types) m a
  }
  deriving (Functor, Applicative, Monad)

runNormT :: DeclSubstitution types -> MetaSubstitution types -> NormT types m a -> m a
runNormT declSubst metaSubst x = runReaderT (unnormT x) (declSubst, metaSubst)

runEmptyNormT :: NormT types m a -> m a
runEmptyNormT = runNormT mempty mempty

instance MonadTrans (NormT types) where
  lift = NormT . lift

instance MonadLogger m => MonadLogger (NormT types m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance MonadError e m => MonadError e (NormT types m) where
  throwError = lift . throwError
  catchError m f = NormT (catchError (unnormT m) (unnormT . f))

instance (MonadCompile m, PrintableBuiltin types) => MonadNorm types (NormT types m) where
  getDeclSubstitution = NormT $ asks fst
  getMetaSubstitution = NormT $ asks snd

-----------------------------------------------------------------------------
-- Evaluation

-- TODO change to return a tuple of NF and WHNF?
eval :: MonadNorm types m => Env types -> NormalisableExpr types -> m (NormExpr types)
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Ann _ e _ -> eval env e
    Lam _ binder body -> do
      binder' <- evalBinder env binder
      return $ VLam binder' env body
    Pi _ binder body -> do
      binder' <- evalBinder env binder
      let newEnv = extendEnvOverBinder binder env
      body' <- eval newEnv body
      return $ VPi binder' body'
    BoundVar p i -> lookupIn p i env
    FreeVar _ ident -> do
      declSubst <- getDeclSubstitution
      let declExpr = Map.lookup ident declSubst
      return $ case declExpr of
        Just x -> x
        Nothing -> VFreeVar ident []
    Let _ bound binder body -> do
      boundNormExpr <- eval env bound
      let newEnv = extendEnv binder boundNormExpr env
      eval newEnv body
    App _ fun args -> do
      fun' <- eval env fun
      args' <- traverse (traverse (eval env)) (NonEmpty.toList args)
      evalApp fun' args'

  showExit env result
  return result

evalBinder :: MonadNorm types m => Env types -> NormalisableBinder types -> m (NormBinder types)
evalBinder env = traverse (eval env)

evalApp :: MonadNorm types m => NormExpr types -> Spine types -> m (NormExpr types)
evalApp fun [] = return fun
evalApp fun (arg : args) = do
  showApp fun (arg : args)
  case fun of
    VMeta v spine -> return $ VMeta v (spine <> (arg : args))
    VFreeVar v spine -> return $ VFreeVar v (spine <> (arg : args))
    VBoundVar v spine -> return $ VBoundVar v (spine <> (arg : args))
    VLam binder env body
      | not (visibilityMatches binder arg) ->
          compilerDeveloperError $ "Mismatch in visibilities" <+> prettyVerbose binder <+> prettyVerbose arg
      | otherwise -> do
          let newEnv = extendEnv binder (argExpr arg) env
          body' <- eval newEnv body
          case args of
            [] -> return body'
            (a : as) -> evalApp body' (a : as)
    VBuiltin b spine
      | not (isTypeClassOperation b) -> do
          evalBuiltin b (spine <> mapMaybe getExplicitArg (arg : args))
      | otherwise -> do
          let (inst, remainingArgs) = findInstanceArg args
          evalApp inst remainingArgs
    VUniverse {} -> unexpectedExprError currentPass "VUniverse"
    VPi {} -> unexpectedExprError currentPass "VPi"

lookupIn :: MonadCompile m => Provenance -> DBIndex -> Env types -> m (NormExpr types)
lookupIn p i env = case lookupVar env i of
  Just (_, value) -> return value
  Nothing ->
    compilerDeveloperError $
      "Environment of size"
        <+> pretty (length env)
        <+> "in which NBE is being performed"
        <+> "is smaller than the found DB index"
        <+> pretty i
        <+> parens (pretty p)

-----------------------------------------------------------------------------
-- Reevaluation

reeval :: MonadNorm types m => NormExpr types -> m (NormExpr types)
reeval expr = case expr of
  VUniverse {} -> return expr
  VLam {} -> return expr
  VPi {} -> return expr
  VMeta m spine -> VMeta m <$> reevalSpine spine
  VFreeVar v spine -> VFreeVar v <$> reevalSpine spine
  VBoundVar v spine -> VBoundVar v <$> reevalSpine spine
  VBuiltin b spine -> evalBuiltin b =<< traverse reeval spine

reevalSpine :: MonadNorm types m => Spine types -> m (Spine types)
reevalSpine = traverse (traverse reeval)

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead :: MonadNorm types m => ConstraintContext types -> NormExpr types -> m (NormExpr types, MetaSet)
forceHead ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      let dbCtx = boundContextOf ctx
      logDebug MaxDetail $ "forced" <+> prettyFriendly (WithContext expr dbCtx) <+> "to" <+> prettyFriendly (WithContext forcedExpr dbCtx)
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr :: forall types m. MonadNorm types m => NormExpr types -> m (Maybe (NormExpr types), MetaSet)
forceExpr = go
  where
    go :: NormExpr types -> m (Maybe (NormExpr types), MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> forceBuiltin b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> Spine types -> m (Maybe (NormExpr types), MetaSet)
    goMeta m spine = do
      metaSubst <- getMetaSubstitution
      case MetaMap.lookup m metaSubst of
        Just solution -> do
          normMetaExpr <- evalApp (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

forceArg :: MonadNorm types m => NormExpr types -> m (NormExpr types, Bool, MetaSet)
forceArg expr = do
  (maybeResult, blockingMetas) <- forceExpr expr
  let result = fromMaybe expr maybeResult
  let reduced = isJust maybeResult
  return (result, reduced, blockingMetas)

forceBuiltin ::
  MonadNorm types m =>
  NormalisableBuiltin types ->
  ExplicitSpine types ->
  m (Maybe (NormExpr types), MetaSet)
forceBuiltin b spine = case b of
  CConstructor {} -> return (Nothing, mempty)
  CType {} -> return (Nothing, mempty)
  CFunction {} -> do
    (argResults, argsReduced, argBlockingMetas) <- unzip3 <$> traverse forceArg spine
    let anyArgsReduced = or argsReduced
    let blockingMetas = MetaSet.unions argBlockingMetas
    result <-
      if not anyArgsReduced
        then return Nothing
        else do
          Just <$> evalBuiltin b argResults
    return (result, blockingMetas)

-----------------------------------------------------------------------------
-- Normalisation of builtins
-----------------------------------------------------------------------------

evalBuiltin ::
  MonadNorm types m =>
  NormalisableBuiltin types ->
  ExplicitSpine types ->
  m (NormExpr types)
evalBuiltin b args = case b of
  CConstructor {} -> return $ VBuiltin b args
  CType {} -> return $ VBuiltin b args
  CFunction f -> evalBuiltinFunction f args

evalBuiltinFunction :: MonadNorm types m => BuiltinFunction -> ExplicitSpine types -> m (NormExpr types)
evalBuiltinFunction b args
  | isDerived b = evalDerivedBuiltin b args
  | otherwise = do
      let result = case b of
            Quantifier {} -> Nothing
            Not -> evalNot args
            And -> evalAnd args
            Or -> evalOr args
            Neg dom -> evalNeg dom args
            Add dom -> evalAdd dom args
            Sub dom -> evalSub dom args
            Mul dom -> evalMul dom args
            Div dom -> evalDiv dom args
            Equals dom op -> evalEquals dom op args
            Order dom op -> evalOrder dom op args
            If -> evalIf args
            At -> evalAt args
            ConsVector -> evalConsVector args
            Fold dom -> evalFold dom args
            FromNat _ dom -> evalFromNat dom args
            FromRat dom -> evalFromRat dom args
            Implies -> Just $ compilerDeveloperError $ "Found derived types" <+> pretty b
            Indices -> evalIndices args

      case result of
        Nothing -> return $ VBuiltinFunction b args
        Just r -> r

isDerived :: BuiltinFunction -> Bool
isDerived = \case
  Implies {} -> True
  _ -> False

evalDerivedBuiltin ::
  MonadNorm types m =>
  BuiltinFunction ->
  ExplicitSpine types ->
  m (NormExpr types)
evalDerivedBuiltin b args = case b of
  Implies -> evalImplies args
  _ -> compilerDeveloperError $ "Invalid derived types" <+> quotePretty b

-----------------------------------------------------------------------------
-- Indvidual builtins

type EvalBuiltin types m = ExplicitSpine types -> Maybe (m (NormExpr types))

evalNot :: MonadNorm types m => EvalBuiltin types m
evalNot e = case e of
  [VBoolLiteral x] -> Just $ return $ VBoolLiteral (not x)
  _ -> Nothing

evalAnd :: MonadNorm types m => EvalBuiltin types m
evalAnd = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ return $ VBoolLiteral (x && y)
  _ -> Nothing

evalOr :: MonadNorm types m => EvalBuiltin types m
evalOr = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ return $ VBoolLiteral (x && y)
  _ -> Nothing

evalNeg :: MonadNorm types m => NegDomain -> EvalBuiltin types m
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: MonadNorm types m => EvalBuiltin types m
evalNegInt = \case
  [VIntLiteral x] -> Just $ return $ VIntLiteral (-x)
  _ -> Nothing

evalNegRat :: MonadNorm types m => EvalBuiltin types m
evalNegRat = \case
  [VRatLiteral x] -> Just $ return $ VRatLiteral (-x)
  _ -> Nothing

evalAdd :: MonadNorm types m => AddDomain -> EvalBuiltin types m
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: MonadNorm types m => EvalBuiltin types m
evalAddNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ return $ VNatLiteral (x + y)
  _ -> Nothing

evalAddInt :: MonadNorm types m => EvalBuiltin types m
evalAddInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ return $ VIntLiteral (x + y)
  _ -> Nothing

evalAddRat :: MonadNorm types m => EvalBuiltin types m
evalAddRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VRatLiteral (x + y)
  _ -> Nothing

evalSub :: MonadNorm types m => SubDomain -> EvalBuiltin types m
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: MonadNorm types m => EvalBuiltin types m
evalSubInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ return $ VIntLiteral (x - y)
  _ -> Nothing

evalSubRat :: MonadNorm types m => EvalBuiltin types m
evalSubRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VRatLiteral (x - y)
  _ -> Nothing

evalMul :: MonadNorm types m => MulDomain -> EvalBuiltin types m
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: MonadNorm types m => EvalBuiltin types m
evalMulNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ return $ VNatLiteral (x * y)
  _ -> Nothing

evalMulInt :: MonadNorm types m => EvalBuiltin types m
evalMulInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ return $ VIntLiteral (x * y)
  _ -> Nothing

evalMulRat :: MonadNorm types m => EvalBuiltin types m
evalMulRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VRatLiteral (x * y)
  _ -> Nothing

evalDiv :: MonadNorm types m => DivDomain -> EvalBuiltin types m
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: MonadNorm types m => EvalBuiltin types m
evalDivRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VRatLiteral (x * y)
  _ -> Nothing

evalOrder :: MonadNorm types m => OrderDomain -> OrderOp -> EvalBuiltin types m
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: MonadNorm types m => OrderOp -> EvalBuiltin types m
evalOrderIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ return $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderNat :: MonadNorm types m => OrderOp -> EvalBuiltin types m
evalOrderNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ return $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderInt :: MonadNorm types m => OrderOp -> EvalBuiltin types m
evalOrderInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ return $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderRat :: MonadNorm types m => OrderOp -> EvalBuiltin types m
evalOrderRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalEquals :: MonadNorm types m => EqualityDomain -> EqualityOp -> EvalBuiltin types m
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: MonadNorm types m => EqualityOp -> EvalBuiltin types m
evalEqualityIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ return $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityNat :: MonadNorm types m => EqualityOp -> EvalBuiltin types m
evalEqualityNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ return $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityInt :: MonadNorm types m => EqualityOp -> EvalBuiltin types m
evalEqualityInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ return $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityRat :: MonadNorm types m => EqualityOp -> EvalBuiltin types m
evalEqualityRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ return $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalFromNat :: MonadNorm types m => FromNatDomain -> EvalBuiltin types m
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: MonadNorm types m => EvalBuiltin types m
evalFromNatToIndex = \case
  [VNatLiteral x] -> Just $ return $ VIndexLiteral x
  _ -> Nothing

evalFromNatToNat :: MonadNorm types m => EvalBuiltin types m
evalFromNatToNat = \case
  [x] -> Just $ return x
  _ -> Nothing

evalFromNatToInt :: MonadNorm types m => EvalBuiltin types m
evalFromNatToInt = \case
  [VNatLiteral x] -> Just $ return $ VIntLiteral x
  _ -> Nothing

evalFromNatToRat :: MonadNorm types m => EvalBuiltin types m
evalFromNatToRat = \case
  [VNatLiteral x] -> Just $ return $ VRatLiteral (fromIntegral x)
  _ -> Nothing

evalFromRat :: MonadNorm types m => FromRatDomain -> EvalBuiltin types m
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: MonadNorm types m => EvalBuiltin types m
evalFromRatToRat = \case
  [x] -> Just $ return x
  _ -> Nothing

evalIf :: MonadNorm types m => EvalBuiltin types m
evalIf = \case
  [VBoolLiteral True, e1, _e2] -> Just $ return e1
  [VBoolLiteral False, _e1, e2] -> Just $ return e2
  _ -> Nothing

evalAt :: MonadNorm types m => EvalBuiltin types m
evalAt = \case
  [VVecLiteral xs, VIndexLiteral i] -> Just $ return $ xs !! fromIntegral i
  _ -> Nothing

evalConsVector :: MonadNorm types m => EvalBuiltin types m
evalConsVector = \case
  [x, VVecLiteral xs] -> Just $ return $ VVecLiteral (x : xs)
  _ -> Nothing

evalFold :: MonadNorm types m => FoldDomain -> EvalBuiltin types m
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList :: MonadNorm types m => EvalBuiltin types m
evalFoldList = \case
  [_f, e, VNil] ->
    Just $ return e
  [f, e, VCons [x, xs']] -> Just $ do
    r <- evalBuiltinFunction (Fold FoldList) [f, e, xs']
    evalApp f [ExplicitArg mempty x, ExplicitArg mempty r]
  _ -> Nothing

evalFoldVector :: MonadNorm types m => EvalBuiltin types m
evalFoldVector = \case
  [f, e, VVecLiteral xs] ->
    Just $
      foldrM f' e (zip [0 ..] xs)
    where
      f' (l, x) r =
        evalApp
          f
          [ ImplicitArg mempty (VNatLiteral l),
            ExplicitArg mempty x,
            ExplicitArg mempty r
          ]
  _ -> Nothing

evalIndices :: MonadNorm types m => EvalBuiltin types m
evalIndices = \case
  [VNatLiteral n] -> Just $ do
    let xs = fmap VIndexLiteral [0 .. n - 1]
    return $ mkVLVec xs
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived types m = ExplicitSpine types -> m (NormExpr types)

-- TODO define in terms of language

evalImplies :: MonadNorm types m => EvalDerived types m
evalImplies = \case
  [e1, e2] -> do
    ne1 <- evalBuiltinFunction Not [e1]
    evalBuiltinFunction Or [ne1, e2]
  args -> return $ VBuiltinFunction Implies args

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: MonadNorm types m => Env types -> NormalisableExpr types -> m ()
showEntry _env _expr = do
  -- logDebug MaxDetail $ "nbe-entry" <+> prettyVerbose expr <+> "   { env=" <+> prettyVerbose env <+> "}"
  -- logDebug MaxDetail $ "nbe-entry" <+> prettyFriendly (WithContext expr (fmap fst env)) <+> "   { env=" <+> prettyVerbose env <+> "}"
  incrCallDepth

showExit :: MonadNorm types m => Env types -> NormExpr types -> m ()
showExit _env _result = do
  decrCallDepth
  -- logDebug MaxDetail $ "nbe-exit" <+> prettyVerbose result
  -- logDebug MaxDetail $ "nbe-exit" <+> prettyFriendly (WithContext result (fmap fst env))
  return ()

showApp :: MonadNorm types m => NormExpr types -> Spine types -> m ()
showApp _fun _spine =
  return ()

-- logDebug MaxDetail $ "nbe-app" <+> prettyVerbose fun <+> prettyVerbose spine
