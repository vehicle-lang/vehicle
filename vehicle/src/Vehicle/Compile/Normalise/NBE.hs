{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf
  , evalBuiltin
  , evalApp
  ) where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map (lookup)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NormExpr
import Vehicle.Compile.Prelude
import Vehicle.Language.Print
import Vehicle.Compile.Normalise.Quote (liftEnvOverBinder)

whnf :: MonadCompile m => Int -> DeclCtx GluedExpr -> CheckedExpr -> m NormExpr
whnf boundCtxSize declCtx e = do
  -- logDebug MaxDetail $ "Normalising" <+> squotes (prettyVerbose e) <+> "in a context of size" <+> pretty boundCtxSize
  runReaderT (eval env e) (fmap normalised declCtx)
  where env = [VVar mempty (Bound i) [] | i <- [0..boundCtxSize - 1]]

-----------------------------------------------------------------------------
-- Evaluation

type MonadNorm m =
  ( MonadCompile m
  , MonadReader (DeclCtx NormExpr) m
  )
-- WHNF - does And call eval on bodies.
-- NF - does call eval on bodies.

-- TODO change to return a tuple of NF and WHNF?
eval :: MonadNorm m => Env -> CheckedExpr -> m NormExpr
eval env expr = do
  --logDebug MaxDetail ("nbe-entry" <+> prettyVerbose expr)
  incrCallDepth
  result <- case expr of
    Hole{} -> resolutionError currentPass "Hole"

    Meta     p m -> return $ VMeta p m []
    Universe p u -> return $ VUniverse p u
    Builtin  p b -> return $ VBuiltin p b []
    Literal  p l -> return $ VLiteral p l

    Ann      _ e _   -> eval env e
    LVec     p xs    -> VLVec p <$> traverse (eval env) xs <*> pure []

    Lam p binder body -> do
      binder' <- evalBinder env binder
      return $ VLam p binder' env body

    Pi  p binder body ->
      VPi p <$> evalBinder env binder <*> eval (liftEnvOverBinder p env) body

    Var p v -> case v of
      Bound i -> case env !!? i of
        Just value -> return value
        Nothing    -> compilerDeveloperError $
          "Environment" <+> prettyVerbose env <+> "in which NBE is being performed" <+>
          "is smaller than the found DB index" <+> pretty i
      Free ident -> do
        declExpr <- asks (Map.lookup ident)
        return $ case declExpr of
          Just x  -> x
          Nothing -> VVar p v []

    Let _ bound _binder body -> do
      boundNormExpr <- eval env bound
      eval (boundNormExpr : env) body

    App _ fun args -> do
      fun'  <- eval env fun
      args' <- traverse (traverse (eval env)) args
      evalApp fun' args'

  decrCallDepth
  --logDebug MaxDetail ("nbe-exit" <+> prettyVerbose result)
  return result

evalBinder :: MonadNorm m => Env -> CheckedBinder -> m NormBinder
evalBinder env = traverse (eval env)

evalApp :: MonadNorm m => NormExpr -> NonEmpty (GenericArg NormExpr) -> m NormExpr
evalApp fun (arg :| args) = case fun of
  VMeta p v  spine -> return $ VMeta p v (spine <> (arg : args))
  VVar  p v  spine -> return $ VVar  p v (spine <> (arg : args))
  VLVec p xs spine -> return $ VLVec  p xs (spine <> (arg : args))

  VLam _ _binder env body -> do
    body' <- eval (argExpr arg : env) body
    case args of
      []       -> return body'
      (a : as) -> evalApp body' (a :| as)

  VBuiltin p b spine -> evalBuiltin p b (spine <> (arg : args))

  VUniverse{} -> unexpectedExprError currentPass "VUniverse"
  VPi{}       -> unexpectedExprError currentPass "VPi"
  VLiteral{}  -> unexpectedExprError currentPass "VLiteral"

-- Separate builtins from syntactic sugar
--
-- Pass in the right number of arguments ensuring all literals

evalBuiltin :: MonadNorm m
            => Provenance
            -> Builtin
            -> [GenericArg NormExpr]
            -> m NormExpr
evalBuiltin p b args = case b of
  -- TODO rearrange builtin constructors so we don't have to do this.
  Constructor{} -> return $ VBuiltin p b args

  Not -> evalNot p args
  And -> evalAnd p args
  Or  -> evalOr  p args

  FromNat v dom -> case dom of
    FromNatToIndex -> evalFromNatToIndex v p args
    FromNatToNat   -> evalFromNatToNat   v p args
    FromNatToInt   -> evalFromNatToInt   v p args
    FromNatToRat   -> evalFromNatToRat   v p args

  FromRat dom -> case dom of
    FromRatToRat -> evalFromRatToRat p args

  Neg dom -> case dom of
    NegInt -> evalNegInt p args
    NegRat -> evalNegRat p args

  Add dom -> case dom of
    AddNat -> evalAddNat p args
    AddInt -> evalAddInt p args
    AddRat -> evalAddRat p args

  Sub dom -> case dom of
    SubInt -> evalSubInt p args
    SubRat -> evalSubRat p args

  Mul dom -> case dom of
    MulNat -> evalMulNat p args
    MulInt -> evalMulInt p args
    MulRat -> evalMulRat p args

  Div dom -> case dom of
    DivRat -> evalDivRat p args

  Equals dom op -> case dom of
    EqIndex -> evalEqualityIndex op p args
    EqNat   -> evalEqualityNat op p args
    EqInt   -> evalEqualityInt op p args
    EqRat   -> evalEqualityRat op p args

  Order  dom op -> case dom of
    OrderIndex -> evalOrderIndex op p args
    OrderNat   -> evalOrderNat op p args
    OrderInt   -> evalOrderInt op p args
    OrderRat   -> evalOrderRat op p args

  If -> evalIf p args
  At -> evalAt p args

  Fold dom -> case dom of
    FoldList   -> evalFoldList p args
    FoldVector -> evalFoldVector p args

  -- Derived
  TypeClassOp op -> evalTypeClassOp p op args

  Implies -> evalImplies p args
  Tensor  -> evalTensor p args

  Map dom -> case dom of
    MapList   -> evalMapList p args
    MapVector -> evalMapVec p args

  FromVec n dom -> case dom of
    FromVecToVec  -> evalFromVecToVec n p args
    FromVecToList -> evalFromVecToList n p args

  Foreach ->
    evalForeach p args



type EvalBuiltin = forall m . MonadNorm m => Provenance -> [GenericArg NormExpr] -> m NormExpr


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
evalNot p = \case
  [VBool x] -> return $ VLiteral p (LBool (not x))
  args      -> return $ VBuiltin p Not args

evalAnd :: EvalBuiltin
evalAnd p = \case
  [VBool x, VBool y] -> return $ VLiteral p (LBool (x && y))
  args               -> return $ VBuiltin p And args

evalOr :: EvalBuiltin
evalOr p = \case
  [VBool x, VBool y] -> return $ VLiteral p (LBool (x && y))
  args               -> return $ VBuiltin p Or args

evalNegInt :: EvalBuiltin
evalNegInt p = \case
  [VInt x] -> return $ VLiteral p (LInt (- x))
  args     -> return $ VBuiltin p (Neg NegInt) args

evalNegRat :: EvalBuiltin
evalNegRat p = \case
  [VRat x] -> return $ VLiteral p (LRat (- x))
  args     -> return $ VBuiltin p (Neg NegRat) args

evalAddNat :: EvalBuiltin
evalAddNat p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LNat (x + y))
  args             -> return $ VBuiltin p (Add AddNat) args

evalAddInt :: EvalBuiltin
evalAddInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x + y))
  args             -> return $ VBuiltin p (Add AddInt) args

evalAddRat :: EvalBuiltin
evalAddRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x + y))
  args             -> return $ VBuiltin p (Add AddRat) args

evalSubInt :: EvalBuiltin
evalSubInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x - y))
  args             -> return $ VBuiltin p (Sub SubInt) args

evalSubRat :: EvalBuiltin
evalSubRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x - y))
  args             -> return $ VBuiltin p (Sub SubRat) args

evalMulNat :: EvalBuiltin
evalMulNat p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LNat (x * y))
  args             -> return $ VBuiltin p (Mul MulNat) args

evalMulInt :: EvalBuiltin
evalMulInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x * y))
  args             -> return $ VBuiltin p (Mul MulInt) args

evalMulRat :: EvalBuiltin
evalMulRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x * y))
  args             -> return $ VBuiltin p (Mul MulRat) args

evalDivRat :: EvalBuiltin
evalDivRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x * y))
  args             -> return $ VBuiltin p (Div DivRat) args

evalOrderIndex :: OrderOp -> EvalBuiltin
evalOrderIndex op p = \case
  [VIndex x, VIndex y] -> return $ VLiteral p (LBool (orderOp op x y))
  args                 -> return $ VBuiltin p (Order OrderIndex op) args

evalOrderNat :: OrderOp -> EvalBuiltin
evalOrderNat op p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderNat op) args

evalOrderInt :: OrderOp -> EvalBuiltin
evalOrderInt op p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderInt op) args

evalOrderRat :: OrderOp -> EvalBuiltin
evalOrderRat op p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderRat op) args

evalEqualityIndex :: EqualityOp -> EvalBuiltin
evalEqualityIndex op p = \case
  [VIndex x, VIndex y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args                 -> return $ VBuiltin p (Equals EqIndex op) args

evalEqualityNat :: EqualityOp -> EvalBuiltin
evalEqualityNat op p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqNat op) args

evalEqualityInt :: EqualityOp -> EvalBuiltin
evalEqualityInt op p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqInt op) args

evalEqualityRat :: EqualityOp -> EvalBuiltin
evalEqualityRat op p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqRat op) args

evalFromNatToIndex :: Int -> EvalBuiltin
evalFromNatToIndex v p = \case
  [_, VNat n, VNat x] -> return $ VLiteral p $ LIndex n x
  args                -> return $ VBuiltin p (FromNat v FromNatToIndex) args

evalFromNatToNat :: Int -> EvalBuiltin
evalFromNatToNat v p = \case
  [_, ExplicitArg _ x] -> return x
  args                 -> return $ VBuiltin p (FromNat v FromNatToNat) args

evalFromNatToInt :: Int -> EvalBuiltin
evalFromNatToInt v p = \case
  [_, VNat x] -> return $ VLiteral p $ LInt x
  args        -> return $ VBuiltin p (FromNat v FromNatToInt) args

evalFromNatToRat :: Int -> EvalBuiltin
evalFromNatToRat v p = \case
  [_, VNat x] -> return $ VLiteral p $ LRat (fromIntegral x)
  args        -> return $ VBuiltin p (FromNat v FromNatToRat) args

evalFromRatToRat :: EvalBuiltin
evalFromRatToRat p = \case
  [ExplicitArg _ x] -> return x
  args              -> return $ VBuiltin p (FromRat FromRatToRat) args

evalIf :: EvalBuiltin
evalIf p = \case
  [_, VBool True,   e1, _e2] -> return $ argExpr e1
  [_, VBool False, _e1,  e2] -> return $ argExpr e2
  args                       -> return $ VBuiltin p If args

evalAt :: EvalBuiltin
evalAt p = \case
  [_, _, ExplicitArg _ (VLVec _ es _), VIndex i] -> return $ es !! fromIntegral i
  args                                           -> return $ VBuiltin p At args

evalFoldList :: EvalBuiltin
evalFoldList p = \case
  [_f, e, ExplicitArg _ (VConstructor _ Nil [])] ->
    return $ argExpr e

  [f, e, ExplicitArg _ (VConstructor _ Cons [x, xs'])] -> do
    r <- evalFoldList p [f, e, xs']
    evalApp (argExpr f) [x, ExplicitArg p r]

  args ->
    return $ VBuiltin p (Fold FoldList) args

evalFoldVector :: EvalBuiltin
evalFoldVector p = \case
  [f, e, ExplicitArg _ (VLVec _ v _)] ->
    foldrM f' (argExpr e) v
    where f' x r = evalApp (argExpr f) [ExplicitArg p x , ExplicitArg p r]

  args ->
    return $ VBuiltin p (Fold FoldVector) args

evalForeach :: EvalBuiltin
evalForeach p = \case
  [tRes, VNat n, ExplicitArg _ f] -> do
    let fn i = evalApp f [ExplicitArg p (VLiteral p (LIndex n i))]
    xs <- traverse fn [0 .. (n-1 :: Int)]
    return $ mkVLVec p xs tRes

  args -> return $ VBuiltin p Foreach args

evalTypeClassOp :: MonadNorm m
                => Provenance
                -> TypeClassOp
                -> [GenericArg NormExpr]
                -> m NormExpr
evalTypeClassOp p op args = do
  let (inst, remainingArgs) = findInstanceArg args
  if isMeta inst
    then return $ VBuiltin p (TypeClassOp op) args
    else case remainingArgs of
      v : vs -> evalApp inst (v :| vs)
      [] -> compilerDeveloperError $
        "Type class operation with no further arguments:" <+> pretty op

-----------------------------------------------------------------------------
-- Derived

-- TODO define in terms of language

evalImplies :: EvalBuiltin
evalImplies p = \case
  [e1, e2] -> do
    ne1 <- ExplicitArg p <$> evalNot p [e1]
    evalOr p [ne1, e2]

  args -> return $ VBuiltin p Implies args

{-
impliesDef :: Provenance -> Env -> NormExpr
impliesDef p env =
  VLam p (ExplicitBinder p Nothing (BoolType p)) env $
    Lam p (ExplicitBinder p Nothing (BoolType p)) $
      _
-}

evalTensor :: EvalBuiltin
evalTensor p = \case
  [ExplicitArg _ tElem, ExplicitArg _ (VConstructor _ Nil _)] -> return tElem
  [tElem, ExplicitArg _ (VConstructor _ Cons [_, n, ns])] -> do
    t' <- ExplicitArg p <$> evalTensor p [tElem, ns]
    return $ VConstructor p Vector [t', n]

  args -> return $ VBuiltin p Tensor args

{-
tensorDef :: Provenance -> Env -> NormExpr
tensorDef p env =
  VLam p (ExplicitBinder p Nothing _) env $
    Lam p (ExplicitBinder p Nothing _) $
      BuiltinExpr p (Fold FoldList) $ ExplicitArg p <$>
        [ Lam p _ _
        , Var p (Bound 1)
        , Var p (Bound 0)
        ]
-}

evalMapList :: EvalBuiltin
evalMapList p = \case
  [_, tTo, _f, ExplicitArg _ (VConstructor _ Nil _)] ->
    return $ VConstructor p Nil [tTo]

  [tFrom, tTo, f, ExplicitArg _ (VConstructor _ Cons [x, xs])] -> do
    x' <- ExplicitArg p <$> evalApp (argExpr f) [x]
    xs' <- ExplicitArg p <$> evalMapList p [tFrom, tTo, f, xs]
    return $ VConstructor p Cons [tTo, x', xs']

  args -> return $ VBuiltin p (Map MapList) args

{-
mapListDef :: Provenance -> Env -> NormExpr
mapListDef p env = _
-}

evalMapVec :: EvalBuiltin
evalMapVec p = \case
  [_n, _t1, t2, ExplicitArg _ f, ExplicitArg _ (VLVec _ xs _)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg p x]) xs
    return $ mkVLVec p xs' t2

  args -> return $ VBuiltin p (Map MapVector) args

{-
mapVecDef :: Provenance -> Env -> NormExpr
mapVecDef p env = _
-}

evalFromVecToList :: Int -> EvalBuiltin
evalFromVecToList n p args = return $ case args of
  [tElem, _f, ExplicitArg _ (VLVec _ xs _)] -> mkNList p (argExpr tElem) xs
  _ -> VBuiltin p (FromVec n FromVecToList) args
{-
fromVecToListDef :: Int -> Provenance -> Env -> NormExpr
fromVecToListDef n env p = _
-}

evalFromVecToVec :: Int -> EvalBuiltin
evalFromVecToVec n p = \case
  [ExplicitArg _ e] -> return e
  args              -> return $ VBuiltin p (FromVec n FromVecToVec) args

{-
fromVecToVecDef :: Int -> Provenance -> Env -> NormExpr
fromVecToVecDef n env p =
  VLam p (ExplicitBinder p Nothing (VectorType p _ _)) _ (BoundVar p 0)
-}

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"
