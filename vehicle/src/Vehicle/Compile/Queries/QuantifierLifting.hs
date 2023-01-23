module Vehicle.Compile.Queries.QuantifierLifting
  ( liftAndRemoveQuantifiers,
    liftQuantifiers,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Queries.Variable (UserVariable (..))
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Public interface

-- | Lifts all quantifiers in the provided expression `e`, of type `Bool` to the
-- top-level and then removes them.
liftAndRemoveQuantifiers :: MonadCompile m => CheckedExpr -> m (CheckedExpr, [UserVariable])
liftAndRemoveQuantifiers expr = logCompilerPass MinDetail currentPass $ do
  liftedExpr <- liftQuantifiers expr
  result@(removedExpr, variables) <- removeQuantifiers liftedExpr
  logCompilerPassOutput $
    pretty variables
      <> line
      <> prettyFriendly (WithContext removedExpr (fmap (Just . userVarName) variables))
  return result

currentPass :: Doc a
currentPass = "quantifier lifting and removal"

--------------------------------------------------------------------------------
-- Implementation

liftQuantifiers :: MonadCompile m => CheckedExpr -> m CheckedExpr
liftQuantifiers expr =
  case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta {} -> resolutionError currentPass "Meta"
    Universe {} -> unexpectedTypeInExprError currentPass "Universe"
    Pi {} -> unexpectedTypeInExprError currentPass "Pi"
    Ann {} -> normalisationError currentPass "Ann"
    Let {} -> normalisationError currentPass "Let"
    LVec {} -> unexpectedExprError currentPass "Seq"
    -- \^ Should never recurse down this far, though this may change if we add support
    -- for boolean valued networks.

    IfExpr {} -> normalisationError currentPass "If"
    OrExpr {} -> normalisationError currentPass "Or"
    NotExpr {} -> normalisationError currentPass "Not"
    ImpliesExpr {} -> normalisationError currentPass "Implies"
    -- \^ Problem becomes much harder if these two are allowed as then quantifiers have
    -- to be negated as well.

    ExistsRatExpr p binder body -> do
      ExistsRatExpr p binder <$> liftQuantifiers body
    AndExpr p [ExplicitArg ann1 e1, ExplicitArg ann2 e2] -> do
      e1' <- liftQuantifiers e1
      e2' <- liftQuantifiers e2
      return $
        liftQuant e1' $ \e1'' _d1 ->
          liftQuant e2' $ \e2'' d2 ->
            AndExpr
              p
              [ ExplicitArg ann1 (liftDBIndices d2 e1''),
                ExplicitArg ann2 (liftDBIndices 0 e2'')
              ]
    App {} -> return expr
    Builtin {} -> return expr
    Literal {} -> return expr
    Var {} -> return expr
    -- Quantified lambdas should have been caught before now.
    Lam {} -> caseError currentPass "Lam" ["QuantifierExpr"]

liftQuant :: CheckedExpr -> (CheckedExpr -> DBLevel -> CheckedExpr) -> CheckedExpr
liftQuant (ExistsRatExpr p binder body) f =
  ExistsRatExpr p binder (liftQuant body (\e d -> f e (d + 1)))
liftQuant e f = f e 0

-- | Strip off user quantifiers
removeQuantifiers :: MonadCompile m => CheckedExpr -> m (CheckedExpr, [UserVariable])
removeQuantifiers (ExistsRatExpr _ binder body) = do
  let n = getBinderName binder
  (result, binders) <- removeQuantifiers body
  return (result, UserVariable n : binders)
removeQuantifiers e = return (e, [])
