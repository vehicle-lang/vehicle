
module Vehicle.Compile.Normalise.IfElimination
  ( liftAndEliminateIfs
  ) where

import Data.List.NonEmpty as NonEmpty

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error

--------------------------------------------------------------------------------
-- If operations

-- | Lifts all `if`s in the provided expression `e`, and eliminates any which
-- live in which is assumed to
-- have been normalised and is of type `Prop`. It does this by recursively
-- lifting the `if` expression until it reaches a point where we know that it's
-- of type `Prop` in which case we then normalise it to an `or` statement.
liftAndEliminateIfs :: MonadLogger m => CheckedExpr -> m CheckedExpr
liftAndEliminateIfs e = do
  logDebug "Beginning if lifting and elimination"
  incrCallDepth
  result <- liftAndElim liftIf elimIf e

  logDebug $ prettySimple result
  decrCallDepth
  logDebug "Finished if lifting elimination"
  return result

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
  op2 And
    (op2 Impl cond         (mapArgExpr elimIf e1))
    (op2 Impl (notOp cond) (mapArgExpr elimIf e2))
  where
    op2 :: BooleanOp2 -> CheckedArg -> CheckedArg -> CheckedArg
    op2 op arg1 arg2 = ExplicitArg ann (BooleanOp2Expr op ann Bool [arg1, arg2])

    notOp :: CheckedArg -> CheckedArg
    notOp arg = ExplicitArg ann (NotExpr ann Bool [arg])
elimIf e = e

--------------------------------------------------------------------------------
-- General operations

type LiftingOp = (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
type EliminationOp = CheckedExpr -> CheckedExpr

liftAndElim :: MonadLogger m => LiftingOp -> EliminationOp -> CheckedExpr -> m CheckedExpr
liftAndElim liftOp elimOp expr =
  let recCall = liftAndElim liftOp elimOp in
  case expr of
    Type{}     -> return expr
    PrimDict{} -> return expr
    Builtin{}  -> return expr
    Literal{}  -> return expr
    Var{}      -> return expr
    Hole{}     -> return expr
    Meta{}     -> return expr

    Pi{}       -> typeError currentPass "Pi"

    QuantifierExpr q  ann binder body -> QuantifierExpr q ann binder . elimOp <$> recCall body
    NotExpr           ann t args      -> NotExpr           ann t <$> traverse (traverseArgExpr (fmap elimOp . recCall)) args
    BooleanOp2Expr op ann t args      -> BooleanOp2Expr op ann t <$> traverse (traverseArgExpr (fmap elimOp . recCall)) args
    IfExpr            ann t args      -> IfExpr            ann t <$> traverse (traverseArgExpr (fmap elimOp . recCall)) args

    Let ann bound binder body ->
      Let ann <$> recCall bound <*> pure binder <*> recCall body

    App ann fun args -> do
      fun'  <- recCall fun
      args' <- traverse (traverseArgExpr recCall) args
      return $ liftOp (\v -> liftArgs liftOp (\vs -> App ann v vs) args') fun'

    Ann ann e t -> do
      e' <- recCall e
      t' <- recCall t
      return $ liftOp (\e'' -> liftOp (\t'' -> Ann ann e'' t'') t') e'

    LSeq ann dict es -> do
      dict' <- recCall dict
      es'   <- traverse recCall es
      return $ liftOp (\dict'' -> liftSeq liftOp (\es'' -> LSeq ann dict'' es'') es') dict'

    -- Quantified lambdas should have been caught before now.
    Lam{} -> normalisationError currentPass "Non-quantified Lam"

liftArg :: LiftingOp -> (CheckedArg -> CheckedExpr) -> CheckedArg -> CheckedExpr
liftArg liftOp f (Arg ann v e) = liftOp (f . Arg ann v) e

liftSeq :: LiftingOp -> ([CheckedExpr] -> CheckedExpr) -> [CheckedExpr] -> CheckedExpr
liftSeq _      f []       = f []
liftSeq liftOp f (x : xs) = liftOp (\v -> liftSeq liftOp (\ys -> f (v : ys)) xs) x

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftArgs :: LiftingOp -> (NonEmpty CheckedArg -> CheckedExpr) -> NonEmpty CheckedArg -> CheckedExpr
liftArgs liftOp f (x :| [])       = liftArg liftOp (\x' -> f (x' :| [])) x
liftArgs liftOp f (arg :| y : xs) = if visibilityOf arg == Explicit
  then liftArg liftOp (\arg' -> liftArgs liftOp (\as -> f (arg' <| as)) (y :| xs)) arg
  else                          liftArgs liftOp (\as -> f (arg  <| as)) (y :| xs)

currentPass :: Doc a
currentPass = "if elimination"