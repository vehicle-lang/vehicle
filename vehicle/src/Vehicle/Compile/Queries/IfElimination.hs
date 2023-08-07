{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Compile.Queries.IfElimination
  ( eliminateIfs,
    unfoldIf,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Primary function

-- | Lifts all `if`s in the provided expression `e`, and eliminates any which
-- live in which is assumed to
-- have been normalised and is of type `Bool`. It does this by recursively
-- lifting the `if` expression until it reaches a point where we know that it's
-- of type `Bool` in which case we then normalise it to an `or` statement.
--
--   - `Nothing` indicates no `if`s present
--   - `Just Nothing` indicates `if`s present but couldn't be lifted
--   - `Just (Just x)` indicates `if`s present and `x` is the result of the
--     lifting and eliminating them
eliminateIfs ::
  (MonadCompile m) =>
  StandardNormExpr ->
  m (Maybe (Maybe StandardNormExpr))
eliminateIfs e = do
  ifLiftedExpr <- recLiftIf e
  case ifLiftedExpr of
    Nothing -> return $ Just Nothing
    Just liftedExpr@(VBuiltinFunction If _) ->
      return $ Just $ Just $ elimIf liftedExpr
    Just _ -> return Nothing

currentPass :: Doc a
currentPass = "if elimination"

--------------------------------------------------------------------------------
-- If operations

liftIf :: (StandardNormExpr -> StandardNormExpr) -> StandardNormExpr -> StandardNormExpr
liftIf f (VBuiltinFunction If [t, cond, e1, e2]) =
  VBuiltinFunction
    If
    [ t,
      cond,
      RelevantExplicitArg mempty (liftIf f $ argExpr e1),
      RelevantExplicitArg mempty (liftIf f $ argExpr e2)
    ]
liftIf f e = f e

recLiftIf :: (MonadCompile m) => StandardNormExpr -> m (Maybe StandardNormExpr)
recLiftIf expr = case expr of
  VPi {} -> unexpectedTypeInExprError currentPass "Pi"
  VUniverse {} -> unexpectedTypeInExprError currentPass "Universe"
  -- Can't lift over quantified lambda.
  VLam {} -> return Nothing
  VMeta {} -> return $ Just expr
  VBoundVar {} -> return $ Just expr
  VFreeVar v spine -> do
    maybeLiftedSpine <- sequence <$> (fmap sequence <$> traverse (traverse recLiftIf) spine)
    case maybeLiftedSpine of
      Nothing -> return Nothing
      Just xs -> return $ Just $ liftSpine (VFreeVar v) xs
  VBuiltin b spine -> do
    maybeLiftedSpine <- sequence <$> (fmap sequence <$> traverse (traverse recLiftIf) spine)
    case maybeLiftedSpine of
      Nothing -> return Nothing
      Just xs -> return $ Just $ liftSpine (VBuiltin b) xs

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

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: StandardNormExpr -> StandardNormExpr
elimIf (VBuiltinFunction If [_, cond, e1, e2]) = unfoldIf cond (elimIf (argExpr e1)) (elimIf (argExpr e2))
elimIf e = e

unfoldIf :: StandardNormArg -> StandardNormExpr -> StandardNormExpr -> StandardNormExpr
unfoldIf c x y =
  VBuiltinFunction
    Or
    $ RelevantExplicitArg mempty
      <$> [ VBuiltinFunction And [c, RelevantExplicitArg mempty x],
            VBuiltinFunction And [RelevantExplicitArg mempty (VBuiltinFunction Not [c]), RelevantExplicitArg mempty y]
          ]
