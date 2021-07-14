
module Vehicle.Core.AST.DeBruijn
  ( Name(..)
  , Index(..)
  , DeBruijnExpr
  , DeBruijnDecl
  , DeBruijnProg
  , DeBruijnBinder
  , BindingDepth
  , Liftable(..)
  , Substitutable(..)
  ) where

import Vehicle.Prelude (Symbol)
import Vehicle.Core.AST.Core

--------------------------------------------------------------------------------
-- Definitions

-- |The type of data DeBruijn indices store at binding sites
data Name
  = User Symbol
  | Machine
  deriving (Eq, Ord, Show)

-- |The type of data DeBruijn indices store at name sites
newtype Index = Index Int
  deriving (Eq, Ord, Show)

-- An expression that uses DeBruijn index scheme for both binders and names.
type DeBruijnBinder ann = Binder       Name ann
type DeBruijnArg    ann = Arg    Index Name ann
type DeBruijnExpr   ann = Expr   Index Name ann
type DeBruijnDecl   ann = Decl   Index Name ann
type DeBruijnProg   ann = Prog   Index Name ann

--------------------------------------------------------------------------------
-- DeBruijn operations

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

class Liftable a where
  liftAcc :: BindingDepth -> a -> a

  -- |Lift all deBruin indices that refer to environment variables by 1.
  -- Code loosely based off of:
  -- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
  lift :: a -> a
  lift = liftAcc 0

instance Liftable ann => Liftable (DeBruijnExpr ann) where
  liftAcc d = \case
    App     ann fun arg        -> App     (liftAcc d ann) (liftAcc d fun) (liftAcc d arg)
    Pi      ann binder dom cod -> Pi      (liftAcc d ann) (liftAcc d binder) (liftAcc d dom) (liftAcc (d + 1) cod)
    Builtin ann op             -> Builtin (liftAcc d ann) op
    Meta    ann m              -> Meta    (liftAcc d ann) m
    Let     ann binder e1 e2   -> Let     (liftAcc d ann) (liftAcc d binder) (liftAcc d e1) (liftAcc (d + 1) e2)
    Lam     ann binder e       -> Lam     (liftAcc d ann) (liftAcc d binder) (liftAcc (d + 1) e)
    Literal ann l              -> Literal (liftAcc d ann) l
    Seq     ann es             -> Seq     (liftAcc d ann) (fmap (liftAcc d) es)
    Free    ann ident          -> Free    (liftAcc d ann) ident
    Bound   ann (Index i)      -> Bound   (liftAcc d ann) (Index i')
      where
        i' | d <= i    = i + 1 -- Index is referencing the environment so increment it
           | otherwise = i     -- Index is locally bound so no need to increment it

instance Liftable ann => Liftable (DeBruijnArg ann) where
  liftAcc d (Arg ann vis e) = Arg (liftAcc d ann) vis (liftAcc d e)

instance Liftable ann => Liftable (DeBruijnBinder ann) where
  liftAcc d (Binder ann b vis) = Binder (liftAcc d ann) b vis



class Substitutable a where
  substAcc
    :: Liftable ann
    => BindingDepth      -- ^ current binding depth
    -> DeBruijnExpr ann  -- ^ expression to substitute
    -> a                 -- ^ expression to substitute into
    -> a                 -- ^ the result of the substitution

  -- |Substitute the provided expression for all variables at the current binding depth.
  -- Code loosely based off of:
  -- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
  subst :: Liftable ann => DeBruijnExpr ann -> a -> a
  subst = substAcc 0

instance Substitutable ann => Substitutable (DeBruijnExpr ann) where
  substAcc d sub = \case
    App     ann fun arg        -> App (substAcc d sub ann) (substAcc d sub fun) (substAcc d sub arg)
    Pi      ann binder dom cod -> Pi  (substAcc d sub ann) (substAcc d sub binder) (substAcc d sub dom) (substAcc (d + 1) (lift sub) cod)
    Builtin ann op            -> Builtin (substAcc d sub ann) op
    Meta    ann m             -> Meta (substAcc d sub ann) m
    Let     ann binder e1 e2  -> Let (substAcc d sub ann) binder (substAcc d sub e1) (substAcc (d + 1) (lift sub) e2)
    Lam     ann binder e      -> Lam (substAcc d sub ann) binder (substAcc (d + 1) (lift sub) e)
    Literal ann l             -> Literal (substAcc d sub ann) l
    Seq     ann es            -> Seq (substAcc d sub ann) (fmap (substAcc d sub) es)
    Free    ann ident         -> Free (substAcc d sub ann) ident
    Bound   ann (Index i)     ->
      case compare i d of
        -- Index matches the expression we're substituting for
        EQ -> sub
        -- Index was bound in the original expression
        LT -> Bound ann (Index i)
        -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
        GT -> Bound ann (Index (i - 1))

instance Substitutable ann => Substitutable (DeBruijnArg ann) where
  substAcc d sub (Arg ann vis e) = Arg (substAcc d sub ann) vis (substAcc d sub e)

instance Substitutable ann => Substitutable (DeBruijnBinder ann) where
  substAcc d sub (Binder ann b vis) = Binder (substAcc d sub ann) b vis