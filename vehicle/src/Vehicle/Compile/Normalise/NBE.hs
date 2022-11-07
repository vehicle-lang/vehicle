{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf
  , unnormalise
  , evalBuiltin
  ) where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Foldable (foldrM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map qualified as Map (lookup)

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NormExpr
import Vehicle.Compile.Prelude
import Control.Monad.Except (runExceptT)

whnf :: MonadCompile m => DeclCtx GluedExpr -> CheckedExpr -> m NormExpr
whnf declCtx e = runReaderT (eval mempty e) (fmap normalised declCtx)
{-
whnf :: MonadCompile m
     => DeclCtx CheckedExpr
     -> BoundCtx (Maybe CheckedExpr)
     -> CheckedExpr
     -> m NormExpr
whnf ctx expr = runReaderT (eval _ expr) ctx
-}

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
eval env = \case
  Hole{} -> resolutionError currentPass "Hole"
  Meta{} -> resolutionError currentPass "Meta"

  Universe p u -> return $ VUniverse p u
  Builtin  p b -> return $ VBuiltin p b []
  Literal  p l -> return $ VLiteral p l

  Ann      _ e _   -> eval env e
  LVec     p xs    -> VLVec p <$> traverse (eval env) xs

  Lam p binder body -> do
    binder' <- evalBinder env binder
    return $ VLam p binder' env body

  Pi  p binder body ->
    VPi p <$> evalBinder env binder <*> eval (liftEnvOverBinder p env) body

  Var _ v -> case v of
    Bound i    -> return $ env !! i
    Free ident -> do
      declExpr <- asks (Map.lookup ident)
      case declExpr of
        Just x  -> return x
        Nothing -> internalScopingError currentPass ident

  Let _ bound _binder body -> do
    boundNormExpr <- eval env bound
    eval (boundNormExpr : env) body

  App _ fun args -> do
    fun'  <- eval env fun
    args' <- traverse (traverse (eval env)) args
    evalApp fun' args'

evalBinder :: MonadNorm m => Env -> CheckedBinder -> m NormBinder
evalBinder env = traverse (eval env)

evalApp :: MonadNorm m => NormExpr -> NonEmpty (GenericArg NormExpr) -> m NormExpr
evalApp fun (arg :| args) = case fun of
  VMeta p v [] -> return $ VMeta p v (arg : args)
  VVar  p v [] -> return $ VVar  p v (arg : args)

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

-- Separate builtins from syntactic sugar
--
-- Pass in the right number of arguments ensuring all literals

evalBuiltin :: MonadNorm m
            => Provenance
            -> Builtin
            -> NonEmpty (GenericArg NormExpr)
            -> m NormExpr
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



type EvalBuiltin = forall m . MonadNorm m => Provenance -> NonEmpty (GenericArg NormExpr) -> m NormExpr


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
                -> NonEmpty (GenericArg NormExpr)
                -> m NormExpr
evalTypeClassOp _p op args = do
  let (inst, remainingArgs) = findInstanceArg (toList args) :: (NormExpr, [GenericArg NormExpr])
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

  args -> return $ VBuiltin p Tensor $ toList args

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

  args       -> return $ VBuiltin p (Map MapList) $ toList args

{-
mapListDef :: Provenance -> Env -> NormExpr
mapListDef p env = _
-}

evalMapVec :: EvalBuiltin
evalMapVec p = \case
  [_n, _t1, _t2, ExplicitArg _ f, ExplicitArg _ (VLVec _ xs)] -> do
    xs' <- traverse (\x -> evalApp f [ExplicitArg p x]) xs
    return $ VLVec p xs'

  args -> return $ VBuiltin p (Map MapVector) $ toList args

{-
mapVecDef :: Provenance -> Env -> NormExpr
mapVecDef p env = _
-}

evalFromVecToList :: Int -> EvalBuiltin
evalFromVecToList n p args = return $ case args of
  [tElem, _f, ExplicitArg _ (VLVec _ xs)] -> mkNList p (argExpr tElem) xs
  _ -> VBuiltin p (FromVec n FromVecToList) $ toList args
{-
fromVecToListDef :: Int -> Provenance -> Env -> NormExpr
fromVecToListDef n env p = _
-}

evalFromVecToVec :: Int -> EvalBuiltin
evalFromVecToVec n p = \case
  [ExplicitArg _ e] -> return e
  args              -> return $ VBuiltin p (FromVec n FromVecToVec) $ toList args

{-
fromVecToVecDef :: Int -> Provenance -> Env -> NormExpr
fromVecToVecDef n env p =
  VLam p (ExplicitBinder p Nothing (VectorType p _ _)) _ (BoundVar p 0)
-}

-----------------------------------------------------------------------------
-- Reification

class Quote a b where
  quote :: MonadCompile m => a -> m b

instance Quote NormExpr CheckedExpr where
  quote = \case
    VUniverse p u -> return $ Universe p u
    VLiteral  p l -> return $ Literal p l

    VMeta     p m spine -> normAppList p (Meta p m)    <$> traverse quote spine
    VVar      p v spine -> normAppList p (Var p v)     <$> traverse quote spine
    VBuiltin  p b spine -> normAppList p (Builtin p b) <$> traverse quote spine

    VLVec p xs -> LVec p <$> traverse quote xs
    VPi   p binder body -> Pi p <$> quote binder <*> quote body

    VLam  p binder env body -> do
      -- First quote the binder
      quotedBinder <- quote binder

      -- We normalise the body
      let newEnv = liftEnvOverBinder p env
      normBody <- runReaderT (eval newEnv body) mempty
      -- We then quote the body.
      quotedBody <- quote normBody

      return $ Lam p quotedBinder quotedBody

instance Quote NormBinder CheckedBinder where
  quote = traverse quote

instance Quote NormArg CheckedArg where
  quote = traverse quote

-- | Converts from a normalised representation to an unnormalised representation.
-- Do not call except for logging and debug purposes, very expensive with nested
-- lambdas.
unnormalise :: Quote a b => a -> b
unnormalise e = do
  let r = discardLogger $ runExceptT (quote e)
  case r of
    Left err -> developerError $ "Error thrown while unquoting" <+> pretty (show err)
    Right v  -> v

-----------------------------------------------------------------------------
-- Substitution
{-
instance Substitutable NormExpr NormExpr where
  subst = \case
    VUniverse p u -> return $ VUniverse p u
    VLiteral  p l -> return $ VLiteral p l

    VMeta p m args -> VMeta p m <$> traverse subst args
    VLVec p args   -> VLVec p <$> traverse subst args

    VVar p v@(Free _) args -> VVar p v <$> traverse subst args
    VPi  p binder body -> VPi p <$> traverse subst binder <*> subst body

    VVar p v@(Bound i) args -> do
      args' <- traverse subst args
      (d, sub) <- ask
      if i < d then
        -- If the variable was a bound variable at the start of the substitution
        return $ VVar p v args'
      else case sub p (i - d) of
        -- If no value was provided for the variable
        Nothing -> return $ VVar p (Bound i) args'
        Just e  -> do
          -- Otherwise lift the substitution as per the number of binders we've gone under.
          let e' = if d > 0 then liftFreeDBIndicesNorm d e else e
          case args' of
            [] -> return e'
            (a : as) -> do
              -- Normalise the resulting application to preserve the invariant that the
              -- expression is in normal form.
              let x = evalApp e' (a :| as)
              r <- discardLoggerT $ runExceptT x
              case r of
                Left err -> developerError $ "Error thrown while unquoting" <+> pretty (show err)
                Right t  -> return t

    VLam p binder env body ->
      VLam p <$> traverse subst binder <*> traverse subst env <*> subst body

    expr@(VBuiltin p b args) -> case args of
      [] -> return expr
      (a : as) -> do
        args' <- traverse subst (a :| as)
        runReaderT (evalBuiltin p b args') mempty
-}
liftFreeDBIndicesNorm :: Int -> NormExpr -> NormExpr
liftFreeDBIndicesNorm = developerError "Lifting not yet implemented" -- liftDBIndices (\p v -> VVar p v [])

liftEnvOverBinder :: Provenance -> Env -> Env
liftEnvOverBinder p env = VVar p (Bound 0) [] : fmap (liftFreeDBIndicesNorm 1) env

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"
