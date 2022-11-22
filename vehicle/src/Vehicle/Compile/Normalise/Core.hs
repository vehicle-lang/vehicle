module Vehicle.Compile.Normalise.Core where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Expr.AlphaEquivalence (alphaEq)
import Vehicle.Expr.DeBruijn (liftDBIndices)


--------------------------------------------------------------------------------
-- Normalising equality

nfEq :: Provenance
     -> EqualityDomain
     -> EqualityOp
     -> CheckedArg
     -> CheckedArg
     -> CheckedExpr
nfEq p dom eq e1 e2 = case (dom, argExpr e1, argExpr e2) of
  (EqIndex, IndexLiteral _ _ x, IndexLiteral _ _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqNat,   NatLiteral     _ x, NatLiteral     _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqInt,   IntLiteral     _ x, IntLiteral     _ y) -> BoolLiteral p (equalityOp eq x y)
  (EqRat,   RatLiteral     _ x, RatLiteral     _ y) -> BoolLiteral p (equalityOp eq x y)
  (_, e1', e2') | alphaEq e1' e2'                   -> BoolLiteral p (eq == Eq)
  _                                                 -> EqualityExpr p dom eq [e1, e2]

--------------------------------------------------------------------------------
-- Normalising tensor types

nfTensor :: Provenance
         -> CheckedType
         -> CheckedExpr
         -> CheckedType
nfTensor p tElem dims = case dims of
  NilExpr{}            -> tElem
  AppConsExpr _ _ d ds -> VectorType p (nfTensor p tElem ds) d
  _                    -> TensorType p tElem dims

--------------------------------------------------------------------------------
-- Normalising orders

nfOrder :: Provenance
        -> OrderDomain
        -> OrderOp
        -> CheckedArg
        -> CheckedArg
        -> CheckedExpr
nfOrder p dom ord arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (OrderNat,   NatLiteral     _ x, NatLiteral     _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderIndex, IndexLiteral _ _ x, IndexLiteral _ _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderInt,   IntLiteral     _ x, IntLiteral     _ y) -> BoolLiteral p (orderOp ord x y)
  (OrderRat,   RatLiteral     _ x, RatLiteral     _ y) -> BoolLiteral p (orderOp ord x y)
  (_, e1 , e2) | alphaEq e1 e2                         -> BoolLiteral p (not (isStrict ord))
  _                                                    -> OrderExpr p dom ord [arg1, arg2]


--------------------------------------------------------------------------------
-- Normalising boolean operations

normaliseNotArg :: CheckedArg -> CheckedArg
normaliseNotArg x = ExplicitArg (provenanceOf x) $ nfNot (provenanceOf x) x

nfNot :: Provenance -> CheckedArg -> CheckedExpr
nfNot p arg = case argExpr arg of
  BoolLiteral _ b -> BoolLiteral p (not b)
  _               -> NotExpr p [arg]

nfAnd :: Provenance -> CheckedArg -> CheckedArg -> CheckedExpr
nfAnd p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _)         -> argExpr arg2
  (FalseExpr _, _)         -> FalseExpr p
  (_, TrueExpr  _)         -> argExpr arg1
  (_, FalseExpr _)         -> FalseExpr p
  (e1, e2) | alphaEq e1 e2 -> e1
  _                        -> AndExpr p [arg1, arg2]

nfOr :: Provenance -> CheckedArg -> CheckedArg -> CheckedExpr
nfOr p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _)         -> TrueExpr p
  (FalseExpr _, _)         -> argExpr arg2
  (_, TrueExpr  _)         -> TrueExpr p
  (_, FalseExpr _)         -> argExpr arg1
  (e1, e2) | alphaEq e1 e2 -> e1
  _                        -> OrExpr p [arg1, arg2]

nfImplies :: Provenance -> CheckedArg -> CheckedArg -> CheckedExpr
nfImplies p arg1 arg2 = case (argExpr arg1, argExpr arg2) of
  (TrueExpr  _, _)         -> argExpr arg2
  (FalseExpr _, _)         -> TrueExpr p
  (_, TrueExpr  _)         -> TrueExpr p
  (_, FalseExpr _)         -> NotExpr p [arg2]
  (e1, e2) | alphaEq e1 e2 -> TrueExpr p
  _                        -> ImpliesExpr p [arg1, arg2]

nfIf :: Provenance -> CheckedExpr -> CheckedArg -> CheckedArg -> CheckedArg -> CheckedExpr
nfIf p t condition e1 e2 = case argExpr condition of
  TrueExpr  _ -> argExpr e1
  FalseExpr _ -> argExpr e2
  _           -> IfExpr p t [condition, e1, e2]

-----------------------------------------------------------------------------
-- Normalising conversion

nfFromRat :: MonadCompile m => FromRatDomain -> CheckedArg -> m CheckedExpr
nfFromRat dom (ExplicitArg _ rat@RatLiteral{}) = case dom of
  FromRatToRat -> return rat
nfFromRat _ _ = unexpectedExprError "conversion FromRat" "non-Rat"

nfFromVec :: MonadCompile m => FromVecDomain -> CheckedArg -> m CheckedExpr
nfFromVec dom (ExplicitArg _ vec@(AnnVecLiteral p tElem xs)) = case dom of
  FromVecToList -> return $ mkList p tElem xs
  FromVecToVec  -> return vec
nfFromVec _ _ = unexpectedExprError "conversion FromVec" "non-Vec"

-----------------------------------------------------------------------------
-- Normalising numeric operations

normaliseNegArg :: NegDomain -> CheckedArg -> CheckedArg
normaliseNegArg dom x = ExplicitArg (provenanceOf x) $ nfNeg (provenanceOf x) dom x

nfNeg :: Provenance -> NegDomain -> CheckedArg -> CheckedExpr
nfNeg p dom e = case (dom, argExpr e) of
  (NegInt, IntLiteral _ x) -> IntLiteral p (- x)
  (NegRat, RatLiteral _ x) -> RatLiteral p (- x)
  _                        -> NegExpr p dom [e]

nfAdd :: Provenance -> AddDomain -> CheckedArg -> CheckedArg -> CheckedExpr
nfAdd p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (AddNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x + y)
  (AddInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x + y)
  (AddRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x + y)
  _                                        -> AddExpr p dom [arg1, arg2]

nfSub :: Provenance -> SubDomain -> CheckedArg -> CheckedArg -> CheckedExpr
nfSub p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (SubInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x - y)
  (SubRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x - y)
  _                                        -> SubExpr p dom [arg1, arg2]

nfMul :: Provenance -> MulDomain -> CheckedArg -> CheckedArg -> CheckedExpr
nfMul p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (MulNat, NatLiteral _ x, NatLiteral _ y) -> NatLiteral p (x * y)
  (MulInt, IntLiteral _ x, IntLiteral _ y) -> IntLiteral p (x * y)
  (MulRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x * y)
  _                                        -> MulExpr p dom [arg1, arg2]

nfDiv :: Provenance -> DivDomain -> CheckedArg -> CheckedArg -> CheckedExpr
nfDiv p dom arg1 arg2 = case (dom, argExpr arg1, argExpr arg2) of
  (DivRat, RatLiteral _ x, RatLiteral _ y) -> RatLiteral p (x / y)
  _                                        -> DivExpr p dom [arg1, arg2]

-----------------------------------------------------------------------------
-- Normalising container operations

nfAt :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedArg -> CheckedArg -> CheckedExpr
nfAt p tElem tDim vector index = case (argExpr vector, argExpr index) of
  (AnnVecLiteral _ _ es, IndexLiteral _ _ i) -> es !! fromIntegral i
  _                                          -> AtExpr p tElem tDim [vector, index]
{-
nfFoldList :: MonadNorm m
           => Provenance
           -> CheckedArg
           -> CheckedArg
           -> CheckedArg
           -> Maybe (m CheckedExpr)
nfFoldList ann foldOp unit container = case argExpr container of
  NilExpr  _ _    -> Just $ return $ NilExpr _ _
  ConsExpr _ _ xs -> do
    let combine x body = normApp ann (argExpr foldOp) [ExplicitArg ann x, ExplicitArg ann body]
    Just $ nf $ foldr combine (argExpr unit) xs
  _ -> Nothing
-}


bigOp :: Provenance
      -> (Bool, Builtin)
      -> CheckedExpr
      -> CheckedExpr
      -> CheckedExpr
bigOp p (unit, op) size xs =
  FoldVectorExpr p (BoolType p) size (BoolType p) $ fmap (ExplicitArg p)
    [ Builtin p op
    , BoolLiteral p unit
    , xs
    ]

zipWithVector :: Provenance
              -> CheckedType
              -> CheckedType
              -> CheckedType
              -> CheckedExpr
              -> CheckedExpr
              -> CheckedExpr
              -> CheckedExpr
              -> CheckedExpr
zipWithVector p tElem1 tElem2 tRes size fn xs ys = do
  let xsLifted = fmap (liftDBIndices 1) (ExplicitArg p xs)
  let ysLifted = fmap (liftDBIndices 1) (ExplicitArg p ys)
  let index = ExplicitArg p (BoundVar p 0)

  let body = App p fn $ ExplicitArg p <$>
        [ AtExpr p tElem1 size [xsLifted, index]
        , AtExpr p tElem2 size [ysLifted, index]
        ]

  let lam = Lam p (ExplicitBinder p Nothing (IndexType p size)) body
  ForeachExpr p tRes size lam

{-
nfMapList :: MonadNorm m
          => CheckedType
          -> CheckedType
          -> CheckedExpr
          -> CheckedExpr
          -> Maybe (m CheckedExpr)
nfMapList tFrom tTo fun container = case container of
  NilExpr p _          -> Just $ return $ NilExpr p tTo
  AppConsExpr p _ x xs -> do
    let fx = nf $ normApp p fun [ExplicitArg p x]
    fxs <- nfMapList tFrom tTo fun xs
    return $ AppConsExpr p tTo <$> fx <*> fxs
  _ -> Nothing
-}
