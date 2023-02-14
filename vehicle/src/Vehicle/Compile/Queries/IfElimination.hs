{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Compile.Queries.IfElimination
  ( eliminateIfs,
    unfoldIf,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Primary function

-- | Lifts all `if`s in the provided expression `e`, and eliminates any which
-- live in which is assumed to
-- have been normalised and is of type `Bool`. It does this by recursively
-- lifting the `if` expression until it reaches a point where we know that it's
-- of type `Bool` in which case we then normalise it to an `or` statement.
eliminateIfs :: MonadCompile m => BoundDBCtx -> StandardNormExpr -> m StandardNormExpr
eliminateIfs ctx e =
  logCompilerPass MaxDetail currentPass $ do
    result <- elimIf <$> recLiftIf e
    logCompilerPassOutput (prettyFriendly (WithContext result ctx))
    return result

currentPass :: Doc a
currentPass = "if elimination"

--------------------------------------------------------------------------------
-- If operations

liftIf :: (StandardNormExpr -> StandardNormExpr) -> StandardNormExpr -> StandardNormExpr
liftIf f (VBuiltinFunction If [_t, cond, e1, e2]) =
  VBuiltinFunction
    If
    -- Can't reconstruct the result type of `f` here, so have to insert a hole.
    [ ImplicitArg mempty (VBuiltin (Constructor Bool) []),
      cond,
      fmap (liftIf f) e1,
      fmap (liftIf f) e2
    ]
liftIf f e = f e

recLiftIf :: MonadCompile m => StandardNormExpr -> m StandardNormExpr
recLiftIf expr = case expr of
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  -- Quantified lambdas should have been caught before now.
  VLam {} -> normalisationError currentPass "Non-quantified Lam"
  VUniverse {} -> return expr
  VLiteral {} -> return expr
  VMeta {} -> return expr
  VBoundVar {} -> return expr
  VFreeVar v spine -> liftArgs (VFreeVar v) <$> traverse (traverse recLiftIf) spine
  VBuiltin b spine -> liftArgs (VBuiltin b) <$> traverse (traverse recLiftIf) spine
  VLVec es spine -> do
    es' <- traverse recLiftIf es
    let result = liftSeq (\es'' -> VLVec es'' spine) es'
    return result

liftArg :: (StandardNormArg -> StandardNormExpr) -> StandardNormArg -> StandardNormExpr
liftArg f (Arg p v r e) = liftIf (f . Arg p v r) e

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftArgs ::
  (StandardSpine -> StandardNormExpr) ->
  StandardSpine ->
  StandardNormExpr
liftArgs f [] = f []
liftArgs f (x : xs) =
  if visibilityOf x == Explicit
    then liftArg (\a -> liftArgs (\as -> f (a : as)) xs) x
    else liftArgs (\as -> f (x : as)) xs

liftSeq :: ([StandardNormExpr] -> StandardNormExpr) -> [StandardNormExpr] -> StandardNormExpr
liftSeq f [] = f []
liftSeq f (x : xs) = liftIf (\v -> liftSeq (\ys -> f (v : ys)) xs) x

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: StandardNormExpr -> StandardNormExpr
elimIf (VBuiltinFunction If [_t, cond, e1, e2]) = unfoldIf cond (fmap elimIf e1) (fmap elimIf e2)
elimIf e = e

unfoldIf :: StandardNormArg -> StandardNormArg -> StandardNormArg -> StandardNormExpr
unfoldIf c x y =
  VBuiltinFunction Or $
    fmap
      (ExplicitArg mempty)
      [ VBuiltinFunction And [c, x],
        VBuiltinFunction And [ExplicitArg mempty (VBuiltinFunction Not [c]), y]
      ]
