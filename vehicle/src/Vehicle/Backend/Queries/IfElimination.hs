{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Backend.Queries.IfElimination
  ( eliminateIfs,
    unfoldIf,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.BuiltinInterface
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
  Value Builtin ->
  m (Maybe (Maybe (Value Builtin)))
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

liftIf :: (Value Builtin -> Value Builtin) -> Value Builtin -> Value Builtin
liftIf f (VBuiltinFunction If [t, cond, e1, e2]) =
  VBuiltinFunction
    If
    [ t,
      cond,
      Arg mempty Explicit Relevant (liftIf f $ argExpr e1),
      Arg mempty Explicit Relevant (liftIf f $ argExpr e2)
    ]
liftIf f e = f e

recLiftIf :: (MonadCompile m) => Value Builtin -> m (Maybe (Value Builtin))
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

liftArg :: (VArg Builtin -> Value Builtin) -> VArg Builtin -> Value Builtin
liftArg f (Arg p v r e) = liftIf (f . Arg p v r) e

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftSpine ::
  (Spine Builtin -> Value Builtin) ->
  Spine Builtin ->
  Value Builtin
liftSpine f [] = f []
liftSpine f (x : xs) =
  if visibilityOf x == Explicit
    then liftArg (\a -> liftSpine (\as -> f (a : as)) xs) x
    else liftSpine (\as -> f (x : as)) xs

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: Value Builtin -> Value Builtin
elimIf (VBuiltinFunction If [_, cond, e1, e2]) = unfoldIf cond (elimIf (argExpr e1)) (elimIf (argExpr e2))
elimIf e = e

unfoldIf :: VArg Builtin -> Value Builtin -> Value Builtin -> Value Builtin
unfoldIf c x y =
  VBuiltinFunction
    Or
    $ Arg mempty Explicit Relevant
      <$> [ VBuiltinFunction And [c, Arg mempty Explicit Relevant x],
            VBuiltinFunction And [Arg mempty Explicit Relevant (VBuiltinFunction Not [c]), Arg mempty Explicit Relevant y]
          ]
