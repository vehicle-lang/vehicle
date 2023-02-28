module Vehicle.Compile.Simplify
  ( Simplify (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (filter)
import Vehicle.Compile.Prelude

class Simplify a where
  -- | Simplifies the code according to the options provided.
  --   Note that this can be seen as undoing parts of the type-checking,
  --   and therefore the resulting code is not guaranteed to be well-typed.
  simplify :: a -> a

instance Simplify InputProg where
  simplify = fmap simplify

instance Simplify InputDecl where
  simplify = fmap simplify

instance Simplify InputExpr where
  simplify expr = case expr of
    Universe {} -> expr
    Hole {} -> expr
    Meta {} -> expr
    Builtin {} -> expr
    BoundVar {} -> expr
    FreeVar {} -> expr
    Ann p e t -> Ann p (simplify e) (simplify t)
    Pi p binder result -> Pi p (simplify binder) (simplify result)
    Let p bound binder body -> Let p (simplify bound) (simplify binder) (simplify body)
    Lam p binder body -> Lam p (simplify binder) (simplify body)
    App p fun args -> do
      let fun' = simplify fun
      let args' = simplifyArgs args
      -- Remove automatically inserted cast functions
      if isLiteralCast fun' && not (null args')
        then argExpr $ last args'
        else normAppList p fun' args'

instance Simplify InputBinder where
  simplify = fmap simplify

instance Simplify InputArg where
  simplify = fmap simplify

simplifyArgs :: NonEmpty InputArg -> [InputArg]
simplifyArgs = fmap simplify . NonEmpty.filter (not . wasInserted)

wasInserted :: Arg binder var builtin -> Bool
wasInserted arg = case visibilityOf arg of
  Implicit True -> True
  Instance True -> True
  _ -> False

isLiteralCast :: Expr binder var Builtin -> Bool
isLiteralCast = \case
  Builtin _ (BuiltinFunction FromNat {}) -> True
  Builtin _ (BuiltinFunction FromRat {}) -> True
  Builtin _ (TypeClassOp FromNatTC {}) -> True
  Builtin _ (TypeClassOp FromRatTC {}) -> True
  Builtin _ (TypeClassOp FromVecTC {}) -> True
  _ -> False
