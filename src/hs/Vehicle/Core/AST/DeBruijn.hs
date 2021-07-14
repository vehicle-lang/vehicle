
module Vehicle.Core.AST.DeBruijn
  ( DeBruijnBinder(..)
  , DeBruijnIndex(..)
  , DeBruijnExpr
  , DeBruijnDecl
  , DeBruijnProg
  , toSymbol
  , BindingDepth
  , liftDeBruijn
  , substDeBruijn
  ) where

import Vehicle.Prelude (Symbol)
import Vehicle.Core.AST.Core (Expr(..), Decl, Prog)

--------------------------------------------------------------------------------
-- Definitions

-- |The type of data DeBruijn indices store at binding sites
data DeBruijnBinder
  = User Symbol
  | Machine
  deriving (Eq, Ord, Show)

-- |The type of data DeBruijn indices store at name sites
newtype DeBruijnIndex = Index Int
  deriving (Eq, Ord, Show)

-- |Returns the |Symbol| of a DeBruijn binder.
toSymbol :: DeBruijnBinder -> Maybe Symbol
toSymbol (User symbol) = Just symbol
toSymbol Machine       = Nothing

-- An expression that uses DeBruijn index scheme for both binders and names.
type DeBruijnExpr ann = Expr DeBruijnIndex DeBruijnBinder ann
type DeBruijnDecl ann = Decl DeBruijnIndex DeBruijnBinder ann
type DeBruijnProg ann = Prog DeBruijnIndex DeBruijnBinder ann

--------------------------------------------------------------------------------
-- DeBruijn operations

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

-- | lift all deBruin indices that refer to environment variables by 1.
-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
liftDeBruijn
  :: BindingDepth      -- ^ current binding depth
  -> DeBruijnExpr ann  -- ^ expression to lift
  -> DeBruijnExpr ann  -- ^ the result of the lifting
liftDeBruijn d = \case
  Star    ann               -> Star ann
  App     ann e1 e2         -> App ann (liftDeBruijn d e1) (liftDeBruijn d e2)
  Fun     ann e1 e2         -> Fun ann (liftDeBruijn d e1) (liftDeBruijn d e2)
  Builtin ann op            -> Builtin ann op
  Meta    ann m             -> Meta ann m
  Forall  ann binder cons e -> Forall ann binder cons (liftDeBruijn (d + 1) e)
  Let     ann binder e1 e2  -> Let ann binder (liftDeBruijn d e1) (liftDeBruijn (d + 1) e2)
  Lam     ann binder e      -> Lam ann binder (liftDeBruijn (d + 1) e)
  Literal ann l             -> Literal ann l
  Seq     ann es            -> Seq ann (fmap (liftDeBruijn d) es)
  Free    ann ident         -> Free ann ident
  Bound   ann (Index i)     -> Bound ann (Index i')
    where
      i' | d <= i    = i + 1 -- Index is referencing the environment so increment it
         | otherwise = i     -- Index is locally bound so no need to increment it

-- |Substitute the provided expression for all variables at the current binding depth.
-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html
substDeBruijn
  :: BindingDepth     -- ^ current binding depth
  -> DeBruijnExpr ann -- ^ expression to substitute
  -> DeBruijnExpr ann -- ^ expression to substitute into
  -> DeBruijnExpr ann -- ^ the result of the substitution
substDeBruijn d sub = \case
  Star    ann               -> Star ann
  App     ann e1 e2         -> App ann (substDeBruijn d sub e1) (substDeBruijn d sub e2)
  Fun     ann e1 e2         -> Fun ann (substDeBruijn d sub e1) (substDeBruijn d sub e2)
  Builtin ann op            -> Builtin ann op
  Meta    ann m             -> Meta ann m
  Forall  ann binder cons e -> Forall ann binder cons (substDeBruijn (d + 1) (liftDeBruijn 0 sub) e)
  Let     ann binder e1 e2  -> Let ann binder (substDeBruijn d sub e1) (substDeBruijn (d + 1) (liftDeBruijn 0 sub) e2)
  Lam     ann binder e      -> Lam ann binder (substDeBruijn (d + 1) (liftDeBruijn 0 sub) e)
  Literal ann l             -> Literal ann l
  Seq     ann es            -> Seq ann (fmap (substDeBruijn d sub) es)
  Free    ann ident         -> Free ann ident
  Bound   ann (Index i)     ->
    case compare i d of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original expression
      LT -> Bound ann (Index i)
      -- Index was free in the original expression, and we've removed a binder so decrease it by 1.
      GT -> Bound ann (Index (i - 1))