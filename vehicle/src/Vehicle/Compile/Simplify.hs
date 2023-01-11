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

instance Simplify expr => Simplify (GenericProg expr) where
  simplify = fmap simplify

instance Simplify expr => Simplify (GenericDecl expr) where
  simplify = fmap simplify

instance Simplify (Expr binder var) where
  simplify expr = case expr of
    Universe {} -> expr
    Hole {} -> expr
    Meta {} -> expr
    Builtin {} -> expr
    Literal {} -> expr
    Var {} -> expr
    App p fun args -> normAppList p (simplify fun) (simplifyArgs args)
    LVec p xs -> LVec p (fmap simplify xs)
    Ann p e t -> Ann p (simplify e) (simplify t)
    Pi p binder result -> Pi p (simplify binder) (simplify result)
    Let p bound binder body -> Let p (simplify bound) (simplify binder) (simplify body)
    Lam p binder body -> Lam p (simplify binder) (simplify body)

instance Simplify (Binder binder var) where
  simplify = fmap simplify

instance Simplify (Arg binder var) where
  simplify = fmap simplify

simplifyArgs :: NonEmpty (Arg binder var) -> [Arg binder var]
simplifyArgs = NonEmpty.filter (not . wasInserted)

wasInserted :: Arg binder var -> Bool
wasInserted arg = case visibilityOf arg of
  Implicit True -> True
  Instance True -> True
  _ -> False
