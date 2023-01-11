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
    App ann fun args -> normAppList ann (simplify fun) (simplifyArgs args)
    LVec ann xs -> LVec ann (fmap simplify xs)
    Ann ann e t -> Ann ann (simplify e) (simplify t)
    Pi ann binder result -> Pi ann (simplify binder) (simplify result)
    Let ann bound binder body -> Let ann (simplify bound) (simplify binder) (simplify body)
    Lam ann binder body -> Lam ann (simplify binder) (simplify body)

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
