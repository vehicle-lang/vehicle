{-# LANGUAGE PartialTypeSignatures #-}

module Vehicle.Core.AST.DeBruijn
  ( Name(..)
  , Var(..)
  , Index
  , DeBruijnExpr
  , DeBruijnDecl
  , DeBruijnProg
  , DeBruijnArg
  , DeBruijnBinder
  , BindingDepth
  , liftDBIndices
  , substInto
  , patternOfArgs
  , substAll
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, Reader, ask, runReader, runReaderT, local)
import Control.Monad.Trans (lift)

import Vehicle.Prelude
import Vehicle.Core.AST.Core

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------
-- Definitions

type Index = Int

-- |The type of data DeBruijn indices store at name sites
data Var
  = Free Identifier
  | Bound Index
  deriving (Eq, Ord, Show, Generic)

instance NFData Var

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DeBruijnBinder ann = Binder Var ann
type DeBruijnArg    ann = Arg    Var ann
type DeBruijnExpr   ann = Expr   Var ann
type DeBruijnDecl   ann = Decl   Var ann
type DeBruijnProg   ann = Prog   Var ann

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

-- | A type-synonym for the function that is used to update a bound variable
type UpdateVariable m state ann
  =  Index                 -- The old deBruijn index of the variable
  -> ann                   -- The annotation of the variable
  -> (BindingDepth, state) -- How many binders the variable is under & the current state
  -> m (DeBruijnExpr ann)  -- The resulting expression

-- | A type-synonym for the function that is used to update the operation's state
-- when traversing across a binder.
type TraverseBinder state ann = state -> state

class DeBruijnFunctor ann (a :: * -> *) where
  alter
    :: (Semigroup ann, MonadReader (BindingDepth, state) m)
    => TraverseBinder state ann
    -> UpdateVariable m state ann
    -> a ann
    -> m (a ann)

instance DeBruijnFunctor ann (Expr Var) where
  alter body var =
    let
      altPiBinder  = alter    body var
      altLamBinder = alter    body var
      altArg       = alter    body var
      altExpr      = alter    body var
      underB       = underBinder body
    in \case
      Type l                   -> return (Type l)
      Meta p m                 -> return (Meta p m)
      Hole p name              -> return (Hole p name)
      Builtin ann op           -> return (Builtin ann op)
      Literal ann l            -> return (Literal ann l)
      PrimDict e               -> return (PrimDict e)
      Seq     ann es           -> Seq     ann <$> traverse altExpr es
      Ann     ann term typ     -> Ann     ann <$> altExpr   term   <*> altExpr typ
      App     ann fun args     -> normApp ann <$> altExpr   fun    <*> traverse altArg args
      Pi      ann binder res   -> Pi      ann <$> altPiBinder binder <*> underB (altExpr res)
      Let     ann e1 binder e2 -> Let     ann <$> altExpr e1 <*> altLamBinder binder <*> underB (altExpr e2)
      Lam     ann binder e     -> Lam     ann <$> altLamBinder binder <*> underB (altExpr e)
      Var     ann (Free i)     -> return (Var ann (Free i))
      Var     ann (Bound i)    -> var i ann =<< ask

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, state) m =>
               TraverseBinder state ann -> m a -> m a
underBinder body = local (\(d, s) -> (d+1, body s))

instance DeBruijnFunctor ann (Arg Var) where
  alter body var (Arg p v e) = Arg p v <$> alter body var e

instance DeBruijnFunctor ann (Binder Var) where
  alter body var (Binder p v b e) = Binder p v b <$> alter body var e

--------------------------------------------------------------------------------
-- Concrete operations

-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html

-- | Lift all deBruin indices that refer to environment variables by the provided depth.
liftDBIndices :: Semigroup ann
              => BindingDepth
              -> DeBruijnExpr ann  -- ^ expression to lift
              -> DeBruijnExpr ann  -- ^ the result of the lifting
liftDBIndices j e = runReader (alter id alterVar e) (0 , ())
  where
    alterVar :: UpdateVariable (Reader (BindingDepth, ())) () ann
    alterVar i ann (d, _) = return (Var ann (Bound i'))
      where
        i' | d <= i    = i + j -- Index is referencing the environment so increment
           | otherwise = i     -- Index is locally bound so no need to increment

-- | De Bruijn aware substitution of one expression into another
substInto :: Semigroup ann
          => DeBruijnExpr ann -- ^ expression to substitute
          -> DeBruijnExpr ann -- ^ term to substitute into
          -> DeBruijnExpr ann -- ^ the result of the substitution
substInto sub e = runReader (alter binderUpdate alterVar e) (0 , sub)
  where
    alterVar i ann (d, subExpr) = case compare i d of
      -- Index matches the expression we're substituting for
      EQ -> return subExpr
      -- Index was bound in the original expression
      LT -> return $ Var ann (Bound i)
      -- Index was free in the original expression,
      -- and we've removed a binder so decrease it by 1.
      GT -> return $ Var ann (Bound (i - 1))

    -- Whenever we go underneath the binder we must lift
    -- all the indices in the substituted expression
    binderUpdate = liftDBIndices 1

type Substitution ann = IntMap (DeBruijnExpr ann)

-- TODO: explain what this means:
--   ?X i2 i4 i1 --> [i2 -> 2, i4 -> 1, i1 -> 0]

patternOfArgs :: [DeBruijnArg ann] -> Maybe (Substitution ann)
patternOfArgs args = go (length args - 1) IM.empty args
  where
    go :: Int -> IntMap (DeBruijnExpr ann) -> [DeBruijnArg ann] -> Maybe (Substitution ann)
    go _ revMap [] = Just revMap
    -- TODO: we could eta-reduce arguments too, if possible
    go i revMap (Arg _ _ (Var ann (Bound j)) : restArgs) =
      if IM.member j revMap then
        Nothing -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning; but then we should make sure the solution is well-typed
      else
        go (i-1) (IM.insert j (Var ann (Bound i)) revMap) restArgs
    go _ _ _ = Nothing

substAll :: Semigroup ann
         =>  Substitution ann
         -> DeBruijnExpr ann
         -> Maybe (DeBruijnExpr ann)
substAll sub e = runReaderT (alter binderUpdate alterVar e) (0, sub)
  where
    alterVar i ann (d, subst) =
      if i >= d then
        lift (IM.lookup (i - d) subst)
      else
        return $ Var ann (Bound i)

    binderUpdate = IM.map (liftDBIndices 1)
