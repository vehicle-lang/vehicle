
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
  , cleanDBIndices
  , substInto
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.State (MonadState, modify, get, evalState)

import Vehicle.Prelude
import Vehicle.Core.AST.Core

--------------------------------------------------------------------------------
-- Definitions

-- |The type of data DeBruijn indices store at binding sites
data Name
  = User Symbol
  | Machine
  deriving (Eq, Ord, Show, Generic, NFData)

type Index = Int

-- |The type of data DeBruijn indices store at name sites
data Var
  = Free Identifier
  | Bound Index
  deriving (Eq, Ord, Show, Generic, NFData)

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DeBruijnBinder    ann = Binder Name Var ann
type DeBruijnArg       ann = Arg    Name Var ann
type DeBruijnExpr      ann = Expr   Name Var ann
type DeBruijnDecl      ann = Decl   Name Var ann
type DeBruijnProg      ann = Prog   Name Var ann
type DeBruijnAnn       ann = RecAnn Name Var ann

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

-- | A type-synonym for the function that is used to update a bound variable
type UpdateVariable state ann
  =  Index                 -- The old deBruijn index of the variable
  -> ann                   -- The annotation of the variable
  -> (BindingDepth, state) -- How many binders the variable is under & the current state
  -> DeBruijnExpr ann      -- The resulting expression

-- | A type-synonym for the function that is used to update the operation's state
-- when traversing across a binder.
type TraverseBinder state ann = state -> state

class MutableAnn ann where
  alterAnn
    :: (MonadState (BindingDepth, state) m)
    => TraverseBinder state ann
    -> UpdateVariable state ann
    -> ann
    -> m ann

class Mutable ann (a :: * -> *) where
  alter
    :: (MutableAnn ann, MonadState (BindingDepth, state) m)
    => TraverseBinder state ann
    -> UpdateVariable state ann
    -> a ann
    -> m (a ann)

instance MutableAnn ann => Mutable ann (Expr Name Var) where
  alter body var =
    let
      altAnn    = alterAnn body var
      altBinder = alter    body var
      altArg    = alter    body var
      altExpr   = alter    body var
    in \case
      Type l                   -> return (Type l)
      Constraint               -> return Constraint
      Meta p m                 -> return (Meta p m)
      Hole p name              -> return (Hole p name)
      Builtin ann op           -> Builtin <$> altAnn ann <*> pure op
      Literal ann l            -> Literal <$> altAnn ann <*> pure l
      Seq     ann es           -> Seq     <$> altAnn ann <*> traverse altExpr es
      Ann     ann term typ     -> Ann     <$> altAnn ann <*> altExpr   term   <*> altExpr typ
      App     ann fun arg      -> App     <$> altAnn ann <*> altExpr   fun    <*> altArg arg
      Pi      ann binder res   -> Pi      <$> altAnn ann <*> altBinder binder <*> altExpr res
      Let     ann e1 binder e2 -> Let     <$> altAnn ann <*> altExpr e1 <*> altBinder binder <*> altExpr e2
      Lam     ann binder e     -> Lam     <$> altAnn ann <*> altBinder binder <*> altExpr e
      Var     ann (Free i)     -> Var     <$> altAnn ann <*> pure (Free i)
      Var     ann (Bound i)    -> var i   <$> altAnn ann <*> get

instance Mutable ann (Arg Name Var) where
  alter body var (Arg p vis e) = Arg p vis <$> alter body var e

instance Mutable ann (Binder Name Var) where
  alter body var (Binder p v b e) = do
    e' <- alter body var e
    modify (\(d , s) -> (d+1, body s))
    return $ Binder p v b e'

instance MutableAnn (DeBruijnAnn ann) where
  alterAnn body var (RecAnn expr ann) = RecAnn <$> alter body var expr <*> pure ann

--------------------------------------------------------------------------------
-- Concrete operations

-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html

-- | Lift all deBruin indices that refer to environment variables by 1.
liftDBIndices :: MutableAnn ann
              => DeBruijnExpr ann  -- ^ expression to lift
              -> DeBruijnExpr ann  -- ^ the result of the lifting
liftDBIndices e = evalState (alter id alterVar e) (0 , ())
  where
    alterVar :: UpdateVariable () ann
    alterVar i ann (d, _) = Var ann (Bound i')
      where
        i' | d <= i    = i + 1 -- Index is referencing the environment so increment
           | otherwise = i     -- Index is locally bound so no need to increment

-- | Finds all de Bruijn indices set to the provided value and
-- updates them to their correct value. Used for instantiating Pi types
-- in the `Vehicle.Core.Compile.DSL`.
cleanDBIndices :: MutableAnn ann
               => BindingDepth
               -> DeBruijnExpr ann
               -> DeBruijnExpr ann
cleanDBIndices target e = evalState (alter id alterVar e) (0, ())
  where
    alterVar :: UpdateVariable () ann
    alterVar i ann (d, _)
      | i == target = Var ann (Bound d)
      | otherwise   = Var ann (Bound i)

-- | De Bruijn aware substitution of one expression into another
substInto :: MutableAnn ann
          => DeBruijnExpr ann -- ^ expression to substitute
          -> DeBruijnExpr ann -- ^ term to substitute into
          -> DeBruijnExpr ann -- ^ the result of the substitution
substInto sub e = evalState (alter binderUpdate alterVar e) (0 , sub)
  where
    alterVar :: UpdateVariable (DeBruijnExpr ann) ann
    alterVar i ann (d, sub) = case compare i d of
      -- Index matches the expression we're substituting for
      EQ -> sub
      -- Index was bound in the original expression
      LT -> Var ann (Bound i)
      -- Index was free in the original expression,
      -- and we've removed a binder so decrease it by 1.
      GT -> Var ann (Bound (i - 1))

    -- Whenever we go underneath the binder we must lift
    -- all the indices in the substituted expression
    binderUpdate sub = liftDBIndices sub
