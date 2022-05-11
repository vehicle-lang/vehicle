
module Vehicle.Compile.Normalise.QuantifierLifting
  ( liftQuantifiers
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Language.Print

--------------------------------------------------------------------------------
-- Public interface

-- | Lifts all quantifiers in the provided expression `e`, of type `Bool` to the
-- top-level.
liftQuantifiers :: MonadCompile m => CheckedExpr -> m CheckedExpr
liftQuantifiers e = logCompilerPass currentPass $ do
  result <- recLift e
  logCompilerPassOutput (prettyFriendly result)
  return result

currentPass :: Doc a
currentPass = "quantifier lifting"

--------------------------------------------------------------------------------
-- Implementation

recLift :: MonadCompile m => CheckedExpr -> m CheckedExpr
recLift expr =
  case expr of
    Hole{}     -> resolutionError currentPass "Hole"
    Meta{}     -> resolutionError currentPass "Meta"
    Type{}     -> typeError currentPass "Type"
    Pi{}       -> typeError currentPass "Pi"
    PrimDict{} -> typeError currentPass "PrimDict"
    Ann{}      -> normalisationError currentPass "Ann"
    Let{}      -> normalisationError currentPass "Let"

    LSeq{}     -> unexpectedExprError currentPass "Seq"
    -- ^ Should never recurse down this far, though this may change if we add support
    -- for boolean valued networks.

    IfExpr{}   -> normalisationError currentPass "If"
    OrExpr{}   -> normalisationError currentPass "Or"
    NotExpr{}  -> normalisationError currentPass "Not"
    ImplExpr{} -> normalisationError currentPass "Impl"
    -- ^ Problem becomes much harder if these two are allowed as then quantifiers have
    -- to be negated as well.

    QuantifierExpr q ann binder body -> do
      QuantifierExpr q ann binder <$> recLift body

    AndExpr ann [ExplicitArg ann1 e1, ExplicitArg ann2 e2] -> do
      e1' <- recLift e1
      e2' <- recLift e2
      return $
        liftQuant e1' $ \e1'' d1 ->
          liftQuant e2' $ \e2'' d2 ->
            AndExpr ann
              [ ExplicitArg ann1 (liftFreeDBIndices d2 e1'')
              , ExplicitArg ann2 (liftFreeDBIndices 0 e2'')
              ]

    App{}      -> return expr
    Builtin{}  -> return expr
    Literal{}  -> return expr
    Var{}      -> return expr

    -- Quantified lambdas should have been caught before now.
    Lam{} -> caseError currentPass "Lam" ["QuantifierExpr"]

liftQuant :: CheckedExpr -> (CheckedExpr -> BindingDepth -> CheckedExpr) -> CheckedExpr
liftQuant (QuantifierExpr ann q binder body) f =
  QuantifierExpr ann q binder (liftQuant body (\e d -> f e (d + 1)))
liftQuant e f = f e 0