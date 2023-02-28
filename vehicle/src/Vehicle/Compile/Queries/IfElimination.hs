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
liftIf f (VBuiltinFunction If [cond, e1, e2]) =
  VBuiltinFunction
    If
    [ cond,
      liftIf f e1,
      liftIf f e2
    ]
liftIf f e = f e

recLiftIf :: MonadCompile m => StandardNormExpr -> m StandardNormExpr
recLiftIf expr = case expr of
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  -- Quantified lambdas should have been caught before now.
  VLam {} -> normalisationError currentPass "Non-quantified Lam"
  VUniverse {} -> return expr
  VMeta {} -> return expr
  VBoundVar {} -> return expr
  VFreeVar v spine -> liftSpine (VFreeVar v) <$> traverse (traverse recLiftIf) spine
  VBuiltin b spine -> liftExplicitSpine (VBuiltin b) <$> traverse recLiftIf spine

liftArg :: (StandardNormArg -> StandardNormExpr) -> StandardNormArg -> StandardNormExpr
liftArg f (Arg p v r e) = liftIf (f . Arg p v r) e

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftSpine ::
  (StandardSpine -> StandardNormExpr) ->
  StandardSpine ->
  StandardNormExpr
liftSpine f [] = f []
liftSpine f (x : xs) =
  if visibilityOf x == Explicit
    then liftArg (\a -> liftSpine (\as -> f (a : as)) xs) x
    else liftSpine (\as -> f (x : as)) xs

liftExplicitSpine ::
  (StandardExplicitSpine -> StandardNormExpr) ->
  [StandardNormExpr] ->
  StandardNormExpr
liftExplicitSpine f [] = f []
liftExplicitSpine f (x : xs) = do
  liftIf (\a -> liftExplicitSpine (\as -> f (a : as)) xs) x

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: StandardNormExpr -> StandardNormExpr
elimIf (VBuiltinFunction If [cond, e1, e2]) = unfoldIf cond (elimIf e1) (elimIf e2)
elimIf e = e

unfoldIf :: StandardNormExpr -> StandardNormExpr -> StandardNormExpr -> StandardNormExpr
unfoldIf c x y =
  VBuiltinFunction
    Or
    [ VBuiltinFunction And [c, x],
      VBuiltinFunction And [VBuiltinFunction Not [c], y]
    ]
