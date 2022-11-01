
module Vehicle.Compile.Normalise.NBE where

import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map qualified as Map (lookup)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude

-----------------------------------------------------------------------------
-- Data

type Env = [Value]

-- | A list of arguments for an application that cannot be normalised.
type Spine = [GenericArg Value]

-- TODO - make generic over WHNF.
data Value
  = VUniverse Provenance Universe
  | VLiteral  Provenance Literal
  | VLam      Provenance CheckedBinder Env CheckedExpr
  | VPi       Provenance CheckedBinder Env CheckedExpr
  | VLVec     Provenance [Value]
  | VMeta     Provenance Meta Spine
  | VVar      Provenance DBVar Spine
  | VBuiltin  Provenance Builtin Spine

type MonadNorm m =
  ( MonadCompile m
  , MonadReader (DeclCtx Value) m
  )

-----------------------------------------------------------------------------
-- Evaluation

-- WHNF - does And call eval on bodies.
-- NF - does call eval on bodies.

-- TODO change to return a tuple of NF and WHNF?
eval :: MonadNorm m => Env -> CheckedExpr -> m Value
eval env = \case
  Hole{} -> resolutionError currentPass "Hole"
  Meta{} -> resolutionError currentPass "Meta"

  Universe p u -> return $ VUniverse p u
  Builtin  p b -> return $ VBuiltin p b []
  Literal  p l -> return $ VLiteral p l

  Ann      _ e _   -> eval env e
  LVec     p xs    -> VLVec p <$> traverse (eval env) xs

  Lam p binder body -> return $ VLam p binder env body
  Pi  p binder body -> return $ VPi  p binder env body

  Var _ v -> case v of
    Bound i    -> return $ env !! i
    Free ident -> do
      declValue <- asks (Map.lookup ident)
      case declValue of
        Just x  -> return x
        Nothing -> internalScopingError currentPass ident

  Let _ bound _binder body -> do
    boundValue <- eval env bound
    eval (boundValue : env) body

  App _ fun args -> do
    fun'  <- eval env fun
    args' <- traverse (traverse (eval env)) args
    evalApp fun' args'

evalApp :: MonadNorm m => Value -> NonEmpty (GenericArg Value) -> m Value
evalApp fun (arg :| args) = case fun of
  VMeta p v [] -> return $ VMeta p v (arg : args)
  VVar p v []             -> return $ VVar p v (arg : args)

  VLam _ _binder env body -> do
    body' <- eval (argExpr arg : env) body
    case args of
      []       -> return body'
      (a : as) -> evalApp body' (a :| as)

  VBuiltin p b [] -> case b of
    Constructor{} -> return $ VBuiltin p b args
    _ -> do
      {-
      let nArgs = length (arg : args)
      let arity = builtinArity b
      if nArgs > arity
        then _ -- error
      else if nArgs < arity
        then return $ VBuiltin p b (arg : args)
      else
      -}
      evalBuiltin p b (arg :| args)



  VMeta{}    -> nestedAppError currentPass "VMeta ..."
  VBuiltin{} -> nestedAppError currentPass "VBuiltin ..."
  VVar{}     -> nestedAppError currentPass "VVar ..."

  VUniverse{} -> unexpectedExprError currentPass "VUniverse"
  VPi{}       -> unexpectedExprError currentPass "VPi"
  VLiteral{}  -> unexpectedExprError currentPass "VLiteral"
  VLVec{}     -> unexpectedExprError currentPass "VLVec"

pattern VBool :: Bool -> GenericArg Value
pattern VBool x <- ExplicitArg _ (VLiteral _ (LBool x))

pattern VIndex :: Int -> GenericArg Value
pattern VIndex x <- ExplicitArg _ (VLiteral _ (LIndex _ x))

pattern VNat :: Int -> GenericArg Value
pattern VNat x <- ExplicitArg _ (VLiteral _ (LNat x))

pattern VInt :: Int -> GenericArg Value
pattern VInt x <- ExplicitArg _ (VLiteral _ (LInt x))

pattern VRat :: Rational -> GenericArg Value
pattern VRat x <- ExplicitArg _ (VLiteral _ (LRat x))

pattern VConstructor :: Provenance -> BuiltinConstructor -> [GenericArg Value] -> Value
pattern VConstructor p c args = VBuiltin p (Constructor c) args

-- Separate builtins from syntactic sugar
--
-- Pass in the right number of arguments ensuring all literals

evalBuiltin :: MonadNorm m
            => Provenance
            -> Builtin
            -> NonEmpty (GenericArg Value)
            -> m Value
evalBuiltin p b args = case b of
  -- TODO rearrange builtin constructors so we don't have to do this.
  Constructor{} -> caseError currentPass "Constructor" ["evalApp"]

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



type EvalBuiltin = forall m . MonadNorm m => Provenance -> NonEmpty (GenericArg Value) -> m Value

-- TODO a lot of duplication in the below. Once we have separated out the
-- derived builtins we should be able to

evalNot :: EvalBuiltin
evalNot p = \case
  [VBool x] -> return $ VLiteral p (LBool (not x))
  args      -> return $ VBuiltin p Not $ toList args

evalAnd :: EvalBuiltin
evalAnd p = \case
  [VBool x, VBool y] -> return $ VLiteral p (LBool (x && y))
  args               -> return $ VBuiltin p And $ toList args

evalOr :: EvalBuiltin
evalOr p = \case
  [VBool x, VBool y] -> return $ VLiteral p (LBool (x && y))
  args               -> return $ VBuiltin p Or $ toList args

evalNegInt :: EvalBuiltin
evalNegInt p = \case
  [VInt x] -> return $ VLiteral p (LInt (- x))
  args     -> return $ VBuiltin p (Neg NegInt) $ toList args

evalNegRat :: EvalBuiltin
evalNegRat p = \case
  [VRat x] -> return $ VLiteral p (LRat (- x))
  args     -> return $ VBuiltin p (Neg NegRat) $ toList args

evalAddNat :: EvalBuiltin
evalAddNat p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LNat (x + y))
  args             -> return $ VBuiltin p (Add AddNat) $ toList args

evalAddInt :: EvalBuiltin
evalAddInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x + y))
  args             -> return $ VBuiltin p (Add AddInt) $ toList args

evalAddRat :: EvalBuiltin
evalAddRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x + y))
  args             -> return $ VBuiltin p (Add AddRat) $ toList args

evalSubInt :: EvalBuiltin
evalSubInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x - y))
  args             -> return $ VBuiltin p (Sub SubInt) $ toList args

evalSubRat :: EvalBuiltin
evalSubRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x - y))
  args             -> return $ VBuiltin p (Sub SubRat) $ toList args

evalMulNat :: EvalBuiltin
evalMulNat p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LNat (x * y))
  args             -> return $ VBuiltin p (Mul MulNat) $ toList args

evalMulInt :: EvalBuiltin
evalMulInt p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LInt (x * y))
  args             -> return $ VBuiltin p (Mul MulInt) $ toList args

evalMulRat :: EvalBuiltin
evalMulRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x * y))
  args             -> return $ VBuiltin p (Mul MulRat) $ toList args

evalDivRat :: EvalBuiltin
evalDivRat p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LRat (x * y))
  args             -> return $ VBuiltin p (Div DivRat) $ toList args

evalOrderIndex :: OrderOp -> EvalBuiltin
evalOrderIndex op p = \case
  [VIndex x, VIndex y] -> return $ VLiteral p (LBool (orderOp op x y))
  args                 -> return $ VBuiltin p (Order OrderIndex op) $ toList args

evalOrderNat :: OrderOp -> EvalBuiltin
evalOrderNat op p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderNat op) $ toList args

evalOrderInt :: OrderOp -> EvalBuiltin
evalOrderInt op p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderInt op) $ toList args

evalOrderRat :: OrderOp -> EvalBuiltin
evalOrderRat op p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LBool (orderOp op x y))
  args             -> return $ VBuiltin p (Order OrderRat op) $ toList args

evalEqualityIndex :: EqualityOp -> EvalBuiltin
evalEqualityIndex op p = \case
  [VIndex x, VIndex y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args                 -> return $ VBuiltin p (Equals EqIndex op) $ toList args

evalEqualityNat :: EqualityOp -> EvalBuiltin
evalEqualityNat op p = \case
  [VNat x, VNat y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqNat op) $ toList args

evalEqualityInt :: EqualityOp -> EvalBuiltin
evalEqualityInt op p = \case
  [VInt x, VInt y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqInt op) $ toList args

evalEqualityRat :: EqualityOp -> EvalBuiltin
evalEqualityRat op p = \case
  [VRat x, VRat y] -> return $ VLiteral p (LBool (equalityOp op x y))
  args             -> return $ VBuiltin p (Equals EqRat op) $ toList args

evalFromNatToIndex :: Int -> EvalBuiltin
evalFromNatToIndex v p = \case
  [VNat n, VNat x] -> return $ VLiteral p $ LIndex n x
  args             -> return $ VBuiltin p (FromNat v FromNatToIndex) $ toList args

evalFromNatToNat :: Int -> EvalBuiltin
evalFromNatToNat v p = \case
  [ExplicitArg _ x] -> return x
  args              -> return $ VBuiltin p (FromNat v FromNatToNat) $ toList args

evalFromNatToInt :: Int -> EvalBuiltin
evalFromNatToInt v p = \case
  [VNat x] -> return $ VLiteral p $ LInt x
  args     -> return $ VBuiltin p (FromNat v FromNatToInt) $ toList args

evalFromNatToRat :: Int -> EvalBuiltin
evalFromNatToRat v p = \case
  [VNat x] -> return $ VLiteral p $ LRat (fromIntegral x)
  args     -> return $ VBuiltin p (FromNat v FromNatToRat) $ toList args

evalFromRatToRat :: EvalBuiltin
evalFromRatToRat p = \case
  [ExplicitArg _ x] -> return x
  args              -> return $ VBuiltin p (FromRat FromRatToRat) $ toList args

evalIf :: EvalBuiltin
evalIf p = \case
  [_, VBool True,   e1, _e2] -> return $ argExpr e1
  [_, VBool False, _e1,  e2] -> return $ argExpr e2
  args                       -> return $ VBuiltin p If $ toList args

evalAt :: EvalBuiltin
evalAt p = \case
  [_, _, ExplicitArg _ (VLVec _ es), VIndex i] -> return $ es !! fromIntegral i
  args                                         -> return $ VBuiltin p At $ toList args

evalFoldList :: EvalBuiltin
evalFoldList p = \case
  [_f, e, ExplicitArg _ (VConstructor _ Nil [])] ->
    return $ argExpr e

  [f, e, ExplicitArg _ (VConstructor _ Cons [x, xs'])] -> do
    r <- evalFoldList p [f, e, xs']
    evalApp (argExpr f) [x, ExplicitArg p r]

  args ->
    return $ VBuiltin p (Fold FoldList) $ toList args

evalFoldVector :: EvalBuiltin
evalFoldVector p = \case
  [f, e, ExplicitArg _ (VLVec _ v)] ->
    foldrM f' (argExpr e) v
    where f' x r = evalApp (argExpr f) [ExplicitArg p x , ExplicitArg p r]

  args ->
    return $ VBuiltin p (Fold FoldVector) $ toList args

evalForeach :: EvalBuiltin
evalForeach p = \case
  [_tRes, VNat n, ExplicitArg _ f] -> do
    let fn i = evalApp f [ExplicitArg p (VLiteral p (LIndex n i))]
    VLVec p <$> traverse fn [0 .. (n-1 :: Int)]

  args -> return $ VBuiltin p Foreach $ toList args

evalTypeClassOp :: MonadNorm m
                => Provenance
                -> TypeClassOp
                -> NonEmpty (GenericArg Value)
                -> m Value
evalTypeClassOp _p op args = do
  let (inst, remainingArgs) = findInstanceArg (toList args) :: (Value, [GenericArg Value])
  case remainingArgs of
    -- (VMeta{}, _) -> Nothing
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

  args -> return $ VBuiltin p Implies $ toList args

{-
impliesDef :: Provenance -> Env -> Value
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

  args -> return $ VBuiltin p Tensor $ toList args

{-
tensorDef :: Provenance -> Env -> Value
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

  args       -> return $ VBuiltin p (Map MapList) $ toList args

{-
mapListDef :: Provenance -> Env -> Value
mapListDef p env = _
-}

evalMapVec :: EvalBuiltin
evalMapVec p = \case
  [_n, _t1, _t2, ExplicitArg _ f, ExplicitArg _ (VLVec _ xs)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg p x]) xs
    return $ VLVec p xs'

  args -> return $ VBuiltin p (Map MapVector) $ toList args

{-
mapVecDef :: Provenance -> Env -> Value
mapVecDef p env = _
-}

evalFromVecToList :: Int -> EvalBuiltin
evalFromVecToList n p = \case
  [tElem, _f, ExplicitArg _ (VLVec _ xs)] -> do
    let nil = VConstructor p Nil [tElem]
    let cons y ys = VConstructor p Cons [tElem, ExplicitArg p y, ExplicitArg p ys]
    return $ foldr cons nil xs

  args -> return $ VBuiltin p (FromVec n FromVecToList) $ toList args
{-
fromVecToListDef :: Int -> Provenance -> Env -> Value
fromVecToListDef n env p = _
-}

evalFromVecToVec :: Int -> EvalBuiltin
evalFromVecToVec n p = \case
  [ExplicitArg _ e] -> return e
  args              -> return $ VBuiltin p (FromVec n FromVecToVec) $ toList args

{-
fromVecToVecDef :: Int -> Provenance -> Env -> Value
fromVecToVecDef n env p =
  VLam p (ExplicitBinder p Nothing (VectorType p _ _)) _ (BoundVar p 0)
-}

-----------------------------------------------------------------------------
-- Reification

-- TODO ditch simply
{-
reify :: MonadCompile m => Value -> m CheckedExpr
reify = \case
  VUniverse p u -> return $ Universe p u
  VLiteral  p l -> return $ Literal p l

  VVar      p v spine -> normAppList p (Var p v) <$> traverse (traverse reify) spine
  VBuiltin  p b spine -> normAppList p (Builtin p b) <$> traverse (traverse reify) spine

  VLam      p binder env body -> Lam p binder <$> (reify =<< eval _ body)
  VPi       p binder env body -> Pi  p binder <$> _

  VLVec     p xs -> LVec p <$> traverse reify xs
-}
currentPass :: Doc ()
currentPass = "Normalisation"
