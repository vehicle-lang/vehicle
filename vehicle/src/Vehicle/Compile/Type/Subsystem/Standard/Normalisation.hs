{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Type.Subsystem.Standard.Normalisation where

import Data.Foldable (foldrM)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

-----------------------------------------------------------------------------
-- Normalisation of builtins
-----------------------------------------------------------------------------

instance NormalisableBuiltin Builtin where
  evalBuiltin = evalStandardBuiltin
  forceBuiltin = forceStandardBuiltin

type MonadBasicNorm m = MonadNorm Builtin m

evalStandardBuiltin ::
  MonadBasicNorm m =>
  Builtin ->
  StandardSpine ->
  m StandardNormExpr
evalStandardBuiltin b args = case b of
  Constructor {} -> return $ VBuiltin b args
  TypeClassOp op -> evalTypeClassOp op args
  BuiltinFunction f -> evalBuiltinFunction f args

evalBuiltinFunction :: MonadBasicNorm m => BuiltinFunction -> StandardSpine -> m StandardNormExpr
evalBuiltinFunction b args
  | isDerived b = evalDerivedBuiltin b args
  | otherwise = do
      let relevantArgs = filter isRelevant args

      let result = case b of
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
        Nothing -> return $ VBuiltinFunction b args
        Just r -> r

isDerived :: BuiltinFunction -> Bool
isDerived = \case
  Implies {} -> True
  Map {} -> True
  _ -> False

evalDerivedBuiltin :: MonadBasicNorm m => BuiltinFunction -> StandardSpine -> m StandardNormExpr
evalDerivedBuiltin b args = case b of
  Implies -> evalImplies args
  Map dom -> evalMap dom args
  _ -> compilerDeveloperError $ "Invalid derived builtin" <+> quotePretty b

-----------------------------------------------------------------------------
-- Indvidual builtins

type EvalBuiltin m = StandardSpine -> Maybe (m StandardNormExpr)

pattern VBool :: Bool -> StandardNormArg
pattern VBool x <- ExplicitArg _ (VLiteral (LBool x))

pattern VIndex :: Int -> StandardNormArg
pattern VIndex x <- ExplicitArg _ (VLiteral (LIndex _ x))

pattern VNat :: Int -> StandardNormArg
pattern VNat x <- ExplicitArg _ (VLiteral (LNat x))

pattern VInt :: Int -> StandardNormArg
pattern VInt x <- ExplicitArg _ (VLiteral (LInt x))

pattern VRat :: Rational -> StandardNormArg
pattern VRat x <- ExplicitArg _ (VLiteral (LRat x))

-- TODO a lot of duplication in the below. Once we have separated out the
-- derived builtins we should be able to

evalNot :: MonadBasicNorm m => EvalBuiltin m
evalNot e = case e of
  [VBool x] -> Just $ return $ VLiteral (LBool (not x))
  _ -> Nothing

evalAnd :: MonadBasicNorm m => EvalBuiltin m
evalAnd = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral (LBool (x && y))
  _ -> Nothing

evalOr :: MonadBasicNorm m => EvalBuiltin m
evalOr = \case
  [VBool x, VBool y] -> Just $ return $ VLiteral (LBool (x && y))
  _ -> Nothing

evalNeg :: MonadBasicNorm m => NegDomain -> EvalBuiltin m
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: MonadBasicNorm m => EvalBuiltin m
evalNegInt = \case
  [VInt x] -> Just $ return $ VLiteral (LInt (-x))
  _ -> Nothing

evalNegRat :: MonadBasicNorm m => EvalBuiltin m
evalNegRat = \case
  [VRat x] -> Just $ return $ VLiteral (LRat (-x))
  _ -> Nothing

evalAdd :: MonadBasicNorm m => AddDomain -> EvalBuiltin m
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: MonadBasicNorm m => EvalBuiltin m
evalAddNat = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LNat (x + y))
  _ -> Nothing

evalAddInt :: MonadBasicNorm m => EvalBuiltin m
evalAddInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x + y))
  _ -> Nothing

evalAddRat :: MonadBasicNorm m => EvalBuiltin m
evalAddRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x + y))
  _ -> Nothing

evalSub :: MonadBasicNorm m => SubDomain -> EvalBuiltin m
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: MonadBasicNorm m => EvalBuiltin m
evalSubInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x - y))
  _ -> Nothing

evalSubRat :: MonadBasicNorm m => EvalBuiltin m
evalSubRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x - y))
  _ -> Nothing

evalMul :: MonadBasicNorm m => MulDomain -> EvalBuiltin m
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: MonadBasicNorm m => EvalBuiltin m
evalMulNat = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LNat (x * y))
  _ -> Nothing

evalMulInt :: MonadBasicNorm m => EvalBuiltin m
evalMulInt = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LInt (x * y))
  _ -> Nothing

evalMulRat :: MonadBasicNorm m => EvalBuiltin m
evalMulRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x * y))
  _ -> Nothing

evalDiv :: MonadBasicNorm m => DivDomain -> EvalBuiltin m
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: MonadBasicNorm m => EvalBuiltin m
evalDivRat = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LRat (x * y))
  _ -> Nothing

evalOrder :: MonadBasicNorm m => OrderDomain -> OrderOp -> EvalBuiltin m
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: MonadBasicNorm m => OrderOp -> EvalBuiltin m
evalOrderIndex op = \case
  [VIndex x, VIndex y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderNat :: MonadBasicNorm m => OrderOp -> EvalBuiltin m
evalOrderNat op = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderInt :: MonadBasicNorm m => OrderOp -> EvalBuiltin m
evalOrderInt op = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalOrderRat :: MonadBasicNorm m => OrderOp -> EvalBuiltin m
evalOrderRat op = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LBool (orderOp op x y))
  _ -> Nothing

evalEquals :: MonadBasicNorm m => EqualityDomain -> EqualityOp -> EvalBuiltin m
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: MonadBasicNorm m => EqualityOp -> EvalBuiltin m
evalEqualityIndex op = \case
  [_, _, VIndex x, VIndex y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityNat :: MonadBasicNorm m => EqualityOp -> EvalBuiltin m
evalEqualityNat op = \case
  [VNat x, VNat y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityInt :: MonadBasicNorm m => EqualityOp -> EvalBuiltin m
evalEqualityInt op = \case
  [VInt x, VInt y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalEqualityRat :: MonadBasicNorm m => EqualityOp -> EvalBuiltin m
evalEqualityRat op = \case
  [VRat x, VRat y] -> Just $ return $ VLiteral (LBool (equalityOp op x y))
  _ -> Nothing

evalFromNat :: MonadBasicNorm m => FromNatDomain -> EvalBuiltin m
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: MonadBasicNorm m => EvalBuiltin m
evalFromNatToIndex = \case
  [ImplicitArg _ (VLiteral (LNat n)), VNat x] -> Just $ return $ VLiteral $ LIndex n x
  _ -> Nothing

evalFromNatToNat :: MonadBasicNorm m => EvalBuiltin m
evalFromNatToNat = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalFromNatToInt :: MonadBasicNorm m => EvalBuiltin m
evalFromNatToInt = \case
  [VNat x] -> Just $ return $ VLiteral $ LInt x
  _ -> Nothing

evalFromNatToRat :: MonadBasicNorm m => EvalBuiltin m
evalFromNatToRat = \case
  [VNat x] -> Just $ return $ VLiteral $ LRat (fromIntegral x)
  _ -> Nothing

evalFromRat :: MonadBasicNorm m => FromRatDomain -> EvalBuiltin m
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: MonadBasicNorm m => EvalBuiltin m
evalFromRatToRat = \case
  [ExplicitArg _ x] -> Just $ return x
  _ -> Nothing

evalIf :: MonadBasicNorm m => EvalBuiltin m
evalIf = \case
  [_, VBool True, e1, _e2] -> Just $ return $ argExpr e1
  [_, VBool False, _e1, e2] -> Just $ return $ argExpr e2
  _ -> Nothing

evalAt :: MonadBasicNorm m => EvalBuiltin m
evalAt = \case
  [_, _, ExplicitArg _ (VLVec es _), VIndex i] -> Just $ return $ es !! fromIntegral i
  _ -> Nothing

evalFold :: MonadBasicNorm m => FoldDomain -> EvalBuiltin m
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList :: MonadBasicNorm m => EvalBuiltin m
evalFoldList = \case
  [_, _, _f, e, ExplicitArg _ (VConstructor Nil [_])] ->
    Just $ return $ argExpr e
  [toT, fromT, f, e, ExplicitArg _ (VConstructor Cons [_, x, xs'])] -> Just $ do
    r <- evalBuiltinFunction (Fold FoldList) [toT, fromT, f, e, xs']
    evalApp (argExpr f) [x, ExplicitArg mempty r]
  _ -> Nothing

evalFoldVector :: MonadBasicNorm m => EvalBuiltin m
evalFoldVector = \case
  [_, _, _, f, e, ExplicitArg _ (VLVec v _)] ->
    Just $
      foldrM f' (argExpr e) v
    where
      f' x r = evalApp (argExpr f) [ExplicitArg mempty x, ExplicitArg mempty r]
  _ -> Nothing

evalForeach :: MonadBasicNorm m => EvalBuiltin m
evalForeach = \case
  [tRes, ImplicitArg _ (VLiteral (LNat n)), ExplicitArg _ f] -> Just $ do
    let fn i = evalApp f [ExplicitArg mempty (VLiteral (LIndex n i))]
    xs <- traverse fn [0 .. (n - 1 :: Int)]
    return $ mkVLVec xs (argExpr tRes)
  _ -> Nothing

evalFromVec :: MonadBasicNorm m => FromVecDomain -> EvalBuiltin m
evalFromVec = \case
  FromVecToVec -> evalFromVecToVec
  FromVecToList -> evalFromVecToList

evalFromVecToList :: MonadBasicNorm m => EvalBuiltin m
evalFromVecToList args = case args of
  [tElem, ExplicitArg _ (VLVec xs _)] -> Just $ return $ mkVList (argExpr tElem) xs
  _ -> Nothing

evalFromVecToVec :: MonadBasicNorm m => EvalBuiltin m
evalFromVecToVec = \case
  [_, ExplicitArg _ e] -> Just $ return e
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived m = StandardSpine -> m StandardNormExpr

-- TODO define in terms of language

evalTypeClassOp :: MonadBasicNorm m => TypeClassOp -> EvalDerived m
evalTypeClassOp _op args = do
  let (inst, remainingArgs) = findInstanceArg args
  evalApp inst remainingArgs

evalImplies :: MonadBasicNorm m => EvalDerived m
evalImplies = \case
  [e1, e2] -> do
    ne1 <- ExplicitArg mempty <$> evalBuiltinFunction Not [e1]
    evalBuiltinFunction Or [ne1, e2]
  args -> return $ VBuiltinFunction Implies args

evalMap :: MonadBasicNorm m => MapDomain -> EvalDerived m
evalMap = \case
  MapList -> evalMapList
  MapVector -> evalMapVec

evalMapList :: MonadBasicNorm m => EvalDerived m
evalMapList = \case
  [_, tTo, _f, ExplicitArg _ (VConstructor Nil _)] ->
    return $ VConstructor Nil [tTo]
  [tFrom, tTo, f, ExplicitArg _ (VConstructor Cons [x, xs])] -> do
    x' <- ExplicitArg mempty <$> evalApp (argExpr f) [x]
    xs' <- ExplicitArg mempty <$> evalMapList [tFrom, tTo, f, xs]
    return $ VConstructor Cons [tTo, x', xs']
  args -> return $ VBuiltinFunction (Map MapList) args

evalMapVec :: MonadBasicNorm m => EvalDerived m
evalMapVec = \case
  [_n, _t1, t2, ExplicitArg _ f, ExplicitArg _ (VLVec xs _)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg mempty x]) xs
    return $ mkVLVec xs' (argExpr t2)
  args -> return $ VBuiltinFunction (Map MapVector) args

-----------------------------------------------------------------------------
-- Forcing

forceStandardBuiltin :: MonadBasicNorm m => Builtin -> StandardSpine -> m (Maybe StandardNormExpr, MetaSet)
forceStandardBuiltin b spine = case b of
  Constructor {} -> return (Nothing, mempty)
  BuiltinFunction Foreach -> return (Nothing, mempty)
  TypeClassOp {} -> return (Nothing, mempty)
  _ -> do
    (argResults, argsReduced, argBlockingMetas) <- unzip3 <$> traverse forceArg spine
    let anyArgsReduced = or argsReduced
    let blockingMetas = MetaSet.unions argBlockingMetas
    result <-
      if not anyArgsReduced
        then return Nothing
        else do
          Just <$> evalStandardBuiltin b argResults
    return (result, blockingMetas)
