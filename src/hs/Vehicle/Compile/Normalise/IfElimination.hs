
module Vehicle.Compile.Normalise.IfElimination
  ( eliminateIfs
  ) where

import Data.List.NonEmpty as NonEmpty

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print
import Vehicle.Compile.Normalise.DNF (applyNotAndNormalise)

--------------------------------------------------------------------------------
-- Primary function

-- | Lifts all `if`s in the provided expression `e`, and eliminates any which
-- live in which is assumed to
-- have been normalised and is of type `Bool`. It does this by recursively
-- lifting the `if` expression until it reaches a point where we know that it's
-- of type `Bool` in which case we then normalise it to an `or` statement.
eliminateIfs :: MonadCompile m => CheckedExpr -> m CheckedExpr
eliminateIfs e = logCompilerPass MinDetail currentPass $ do
  result <- liftAndElimIf e
  logCompilerPassOutput (prettyFriendly result)
  return result

currentPass :: Doc a
currentPass = "if elimination"

--------------------------------------------------------------------------------
-- If operations

liftIf :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftIf f (IfExpr ann _t [cond, e1, e2]) = IfExpr ann
  -- Can't reconstruct the result type of `f` here, so have to insert a hole.
  (Hole ann "?")
  [ cond
  , mapArgExpr (liftIf f) e1
  , mapArgExpr (liftIf f) e2
  ]
liftIf f e = f e

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: CheckedExpr -> CheckedExpr
elimIf (IfExpr ann _ [cond, e1, e2]) = argExpr $
  op2 Or HasOr
    (op2 And HasAnd cond         (mapArgExpr elimIf e1))
    (op2 And HasAnd (notOp cond) (mapArgExpr elimIf e2))
  where
    op2 :: BooleanOp2 -> TypeClass -> CheckedArg -> CheckedArg -> CheckedArg
    op2 op tc arg1 arg2 = ExplicitArg ann (BooleanOp2Expr op tc ann [arg1, arg2])

    notOp :: CheckedArg -> CheckedArg
    notOp arg = ExplicitArg ann $ applyNotAndNormalise arg
elimIf e = e

liftAndElimIf :: MonadCompile m => CheckedExpr -> m CheckedExpr
liftAndElimIf expr =
  case expr of
    Universe{} -> return expr
    PrimDict{} -> return expr
    Builtin{}  -> return expr
    Literal{}  -> return expr
    Var{}      -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr

    Pi{}       -> typeError currentPass "Pi"

    QuantifierExpr q  ann binder body -> QuantifierExpr q  ann binder . elimIf <$> liftAndElimIf body

    NotExpr              ann args        -> NotExpr              ann   <$> traverse (traverseArgExpr (fmap elimIf . liftAndElimIf)) args
    BooleanOp2Expr op tc ann args        -> BooleanOp2Expr op tc ann   <$> traverse (traverseArgExpr (fmap elimIf . liftAndElimIf)) args
    IfExpr               ann t args      -> IfExpr               ann t <$> traverse (traverseArgExpr (fmap elimIf . liftAndElimIf)) args

    Let ann bound binder body ->
      Let ann <$> liftAndElimIf bound <*> pure binder <*> liftAndElimIf body

    App ann fun args -> do
      fun'  <- liftAndElimIf fun
      args' <- traverse (traverseArgExpr liftAndElimIf) args
      return $ liftIf (\v -> liftArgs (\vs -> App ann v vs) args') fun'

    Ann ann e t -> do
      e' <- liftAndElimIf e
      t' <- liftAndElimIf t
      return $ liftIf (\e'' -> liftIf (\t'' -> Ann ann e'' t'') t') e'

    LSeq ann es -> do
      es'   <- traverse liftAndElimIf es
      return $ liftSeq (\es'' -> LSeq ann es'') es'

    -- Quantified lambdas should have been caught before now.
    Lam{} -> normalisationError currentPass "Non-quantified Lam"

liftArg :: (CheckedArg -> CheckedExpr) -> CheckedArg -> CheckedExpr
liftArg f (Arg ann v e) = liftIf (f . Arg ann v) e

liftSeq :: ([CheckedExpr] -> CheckedExpr) -> [CheckedExpr] -> CheckedExpr
liftSeq f []       = f []
liftSeq f (x : xs) = liftIf (\v -> liftSeq (\ys -> f (v : ys)) xs) x

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftArgs :: (NonEmpty CheckedArg -> CheckedExpr)
         -> NonEmpty CheckedArg
         -> CheckedExpr
liftArgs f (x :| [])       = liftArg (\x' -> f (x' :| [])) x
liftArgs f (arg :| y : xs) = if visibilityOf arg == Explicit
  then liftArg (\arg' -> liftArgs (\as -> f (arg' <| as)) (y :| xs)) arg
  else                   liftArgs (\as -> f (arg  <| as)) (y :| xs)