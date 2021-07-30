
module Vehicle.Core.AST.DeBruijn
  ( Name(..)
  , Var(..)
  , Index
  , DeBruijnAnn
  , DeBruijnExpr
  , DeBruijnDecl
  , DeBruijnProg
  , DeBruijnArg
  , DeBruijnBinder
  , BindingDepth
  , Liftable(..)
  , Substitutable(..)
  ) where

import Vehicle.Prelude
import Vehicle.Core.AST.Core

--------------------------------------------------------------------------------
-- Definitions

-- |The type of data DeBruijn indices store at binding sites
data Name
  = User Symbol
  | Machine
  deriving (Eq, Ord, Show)

type Index = Int

-- |The type of data DeBruijn indices store at name sites
data Var
  = Free Identifier
  | Bound Index
  deriving (Eq, Ord, Show)

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DeBruijnBinder    ann = Binder Name Var ann
type DeBruijnArg       ann = Arg    Name Var ann
type DeBruijnExpr      ann = Expr   Name Var ann
type DeBruijnDecl      ann = Decl   Name Var ann
type DeBruijnProg      ann = Prog   Name Var ann
type DeBruijnAnn       ann = RecAnn Name Var ann

--------------------------------------------------------------------------------
-- * DeBruijn operations

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

-- ** Liftable

class Liftable a where
  liftAcc :: BindingDepth -> a -> a

  -- |Lift all deBruin indices that refer to environment variables by 1.
  -- Code loosely based off of:
  -- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
  lift :: a -> a
  lift = liftAcc 0

instance Liftable ann => Liftable (DeBruijnExpr ann) where
  liftAcc d = \case
    Type l                   -> Type l
    Constraint               -> Constraint
    Meta    p m              -> Meta    p m
    Hole    p name           -> Hole    p name
    Ann     ann term typ     -> Ann     (liftAcc d ann) (liftAcc d term) (liftAcc d typ)
    App     ann fun arg      -> App     (liftAcc d ann) (liftAcc d fun) (liftAcc d arg)
    Pi      ann binder res   -> Pi      (liftAcc d ann) (liftAcc d binder) (liftAcc (d + 1) res)
    Builtin ann op           -> Builtin (liftAcc d ann) op
    Let     ann binder e1 e2 -> Let     (liftAcc d ann) (liftAcc d binder) (liftAcc d e1) (liftAcc (d + 1) e2)
    Lam     ann binder e     -> Lam     (liftAcc d ann) (liftAcc d binder) (liftAcc (d + 1) e)
    Literal ann l            -> Literal (liftAcc d ann) l
    Seq     ann es           -> Seq     (liftAcc d ann) (fmap (liftAcc d) es)
    Var     ann (Free i)     -> Var     (liftAcc d ann) (Free i)
    Var     ann (Bound i)    -> Var     (liftAcc d ann) (Bound i')
      where
        i' | d <= i    = i + 1 -- Index is referencing the environment so increment it
           | otherwise = i     -- Index is locally bound so no need to increment it

instance Liftable ann => Liftable (DeBruijnArg ann) where
  liftAcc d (Arg p vis e) = Arg p vis (liftAcc d e)

instance Liftable ann => Liftable (DeBruijnBinder ann) where
  liftAcc d (Binder p vis binder expr) = Binder p vis binder (fmap (liftAcc d) expr)

-- ** Concrete liftable instances

instance Liftable (DeBruijnAnn ann) where
  liftAcc d (RecAnn expr ann) = RecAnn (liftAcc d expr) ann


-- ** Substitution

class SubstitutableAnn ann where
  substAnn
    :: BindingDepth
    -> DeBruijnExpr ann
    -> ann
    -> ann

class Substitutable ann (a :: * -> *) where
  substAcc
    :: (Liftable ann, SubstitutableAnn ann)
    => BindingDepth      -- ^ current binding depth
    -> DeBruijnExpr ann  -- ^ expression to substitute
    -> a ann             -- ^ term to substitute into
    -> a ann             -- ^ the result of the substitution

  -- |Substitute the provided expression for all variables at the current binding depth.
  -- Code loosely based off of:
  -- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
  subst :: (SubstitutableAnn ann, Liftable ann) => DeBruijnExpr ann -> a ann -> a ann
  subst = substAcc 0

instance Substitutable ann (Expr Name Var) where
  substAcc d sub = \case
    Type l                   -> Type l
    Constraint               -> Constraint
    Meta    p m              -> Meta     p m
    Hole    p name           -> Hole     p name
    Ann     ann term typ     -> Ann     (substAnn d sub ann) (substAcc d sub term) (substAcc d sub typ)
    App     ann fun arg      -> App     (substAnn d sub ann) (substAcc d sub fun)  (substAcc d sub arg)
    Pi      ann binder res   -> Pi      (substAnn d sub ann) (substAcc d sub binder) (substAcc (d + 1) (lift sub) res)
    Builtin ann op           -> Builtin (substAnn d sub ann) op
    Let     ann binder e1 e2 -> Let     (substAnn d sub ann) (substAcc d sub binder) (substAcc d sub e1) (substAcc (d + 1) (lift sub) e2)
    Lam     ann binder e     -> Lam     (substAnn d sub ann) (substAcc d sub binder) (substAcc (d + 1) (lift sub) e)
    Literal ann l            -> Literal (substAnn d sub ann) l
    Seq     ann es           -> Seq     (substAnn d sub ann) (fmap (substAcc d sub) es)
    Var     ann (Free i)     -> Var     (substAnn d sub ann) (Free i)
    Var     ann (Bound i)    ->
      case compare i d of
        -- Index matches the expression we're substituting for
        EQ -> sub
        -- Index was bound in the original expression
        LT -> Var (substAnn d sub ann) (Bound i)
        -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
        GT -> Var (substAnn d sub ann) (Bound (i - 1))

instance Substitutable ann (Arg Name Var) where
  substAcc d sub (Arg p vis e) = Arg p vis (substAcc d sub e)

instance Substitutable ann (Binder Name Var) where
  substAcc d sub (Binder p vis binder expr) = Binder p vis binder (substAcc d sub expr)

-- ** Concrete substitution instances

instance SubstitutableAnn (DeBruijnAnn ann) where
  substAnn d sub (RecAnn expr ann) = RecAnn (substAcc d sub expr) ann