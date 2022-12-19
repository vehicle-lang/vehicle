module Vehicle.Compile.Queries.IfElimination
  ( eliminateIfs,
  )
where

import Data.List.NonEmpty as NonEmpty
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Core (normaliseNotArg)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print

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
  logCompilerPassOutput (prettyVerbose result)
  return result

currentPass :: Doc a
currentPass = "if elimination"

--------------------------------------------------------------------------------
-- If operations

liftIf :: (CheckedExpr -> CheckedExpr) -> CheckedExpr -> CheckedExpr
liftIf f (IfExpr ann _t [cond, e1, e2]) =
  IfExpr
    ann
    -- Can't reconstruct the result type of `f` here, so have to insert a hole.
    (BoolType ann)
    [ cond,
      fmap (liftIf f) e1,
      fmap (liftIf f) e2
    ]
liftIf f e = f e

-- | Recursively removes all top-level `if` statements in the current
-- provided expression.
elimIf :: CheckedExpr -> CheckedExpr
elimIf (IfExpr ann _ [cond, e1, e2]) =
  OrExpr ann $
    fmap
      (ExplicitArg ann)
      [ AndExpr ann [cond, fmap elimIf e1],
        AndExpr ann [normaliseNotArg cond, fmap elimIf e2]
      ]
elimIf e = e

liftAndElimIf :: MonadCompile m => CheckedExpr -> m CheckedExpr
liftAndElimIf expr = case expr of
  Universe {} -> return expr
  Builtin {} -> return expr
  Literal {} -> return expr
  Var {} -> return expr
  Hole {} -> return expr
  Meta {} -> return expr
  Pi {} -> unexpectedTypeInExprError currentPass "Pi"
  PostulatedQuantifierExpr ident p binder body ->
    PostulatedQuantifierExpr ident p binder . elimIf <$> liftAndElimIf body
  NotExpr p args -> NotExpr p <$> traverse (traverse (fmap elimIf . liftAndElimIf)) args
  AndExpr p args -> AndExpr p <$> traverse (traverse (fmap elimIf . liftAndElimIf)) args
  OrExpr p args -> OrExpr p <$> traverse (traverse (fmap elimIf . liftAndElimIf)) args
  ImpliesExpr p args -> ImpliesExpr p <$> traverse (traverse (fmap elimIf . liftAndElimIf)) args
  IfExpr p t args -> IfExpr p t <$> traverse (traverse (fmap elimIf . liftAndElimIf)) args
  Let p bound binder body ->
    Let p <$> liftAndElimIf bound <*> pure binder <*> liftAndElimIf body
  App p fun args -> do
    fun' <- liftAndElimIf fun
    args' <- traverse (traverse liftAndElimIf) args
    return $ liftIf (\v -> liftArgs (\vs -> App p v vs) args') fun'
  Ann p e t -> do
    e' <- liftAndElimIf e
    t' <- liftAndElimIf t
    return $ liftIf (\e'' -> liftIf (\t'' -> Ann p e'' t'') t') e'
  LVec p es -> do
    es' <- traverse liftAndElimIf es
    return $ liftSeq (\es'' -> LVec p es'') es'

  -- Quantified lambdas should have been caught before now.
  Lam {} -> normalisationError currentPass "Non-quantified Lam"

liftArg :: (CheckedArg -> CheckedExpr) -> CheckedArg -> CheckedExpr
liftArg f (Arg ann v r e) = liftIf (f . Arg ann v r) e

liftSeq :: ([CheckedExpr] -> CheckedExpr) -> [CheckedExpr] -> CheckedExpr
liftSeq f [] = f []
liftSeq f (x : xs) = liftIf (\v -> liftSeq (\ys -> f (v : ys)) xs) x

-- I feel this should be definable in terms of `liftIfs`, but I can't find it.
liftArgs ::
  (NonEmpty CheckedArg -> CheckedExpr) ->
  NonEmpty CheckedArg ->
  CheckedExpr
liftArgs f (x :| []) = liftArg (\x' -> f (x' :| [])) x
liftArgs f (arg :| y : xs) =
  if visibilityOf arg == Explicit
    then liftArg (\arg' -> liftArgs (\as -> f (arg' <| as)) (y :| xs)) arg
    else liftArgs (\as -> f (arg <| as)) (y :| xs)
