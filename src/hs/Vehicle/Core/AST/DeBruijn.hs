{-# LANGUAGE PartialTypeSignatures #-}

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
type UpdateVariable m state ann
  =  Index                 -- The old deBruijn index of the variable
  -> ann                   -- The annotation of the variable
  -> (BindingDepth, state) -- How many binders the variable is under & the current state
  -> m (DeBruijnExpr ann)  -- The resulting expression

-- | A type-synonym for the function that is used to update the operation's state
-- when traversing across a binder.
type TraverseBinder state ann = state -> state

class MutableAnn ann where
  alterAnn
    :: (MonadReader (BindingDepth, state) m)
    => TraverseBinder state ann
    -> UpdateVariable m state ann
    -> ann
    -> m ann

class Mutable ann (a :: * -> *) where
  alter
    :: (MutableAnn ann, MonadReader (BindingDepth, state) m)
    => TraverseBinder state ann
    -> UpdateVariable m state ann
    -> a ann
    -> m (a ann)

instance MutableAnn ann => Mutable ann (Expr Name Var) where
  alter body var =
    let
      altAnn    = alterAnn body var
      altBinder = alter    body var
      altArg    = alter    body var
      altExpr   = alter    body var
      underB    = underBinder body
    in \case
      Type l                   -> return (Type l)
      Meta p m                 -> return (Meta p m)
      Hole p name              -> return (Hole p name)
      Builtin ann op           -> Builtin <$> altAnn ann <*> pure op
      Literal ann l            -> Literal <$> altAnn ann <*> pure l
      Seq     ann es           -> Seq     <$> altAnn ann <*> traverse altExpr es
      Ann     ann term typ     -> Ann     <$> altAnn ann <*> altExpr   term   <*> altExpr typ
      App     ann fun arg      -> App     <$> altAnn ann <*> altExpr   fun    <*> altArg arg
      Pi      ann binder res   -> Pi      <$> altAnn ann <*> altBinder binder <*> underB (altExpr res)
      Let     ann e1 binder e2 -> Let     <$> altAnn ann <*> altExpr e1 <*> altBinder binder <*> underB (altExpr e2)
      Lam     ann binder e     -> Lam     <$> altAnn ann <*> altBinder binder <*> underB (altExpr e)
      Var     ann (Free i)     -> Var     <$> altAnn ann <*> pure (Free i)
      Var     ann (Bound i)    -> do ann' <- altAnn ann
                                     st   <- ask
                                     var i ann' st

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, state) m =>
               TraverseBinder state ann -> m a -> m a
underBinder body = local (\(d, s) -> (d+1, body s))


instance Mutable ann (Arg Name Var) where
  alter body var (Arg p vis e) = Arg p vis <$> alter body var e

instance Mutable ann (Binder Name Var) where
  alter body var (Binder p v b e) = Binder p v b <$> alter body var e

instance MutableAnn (DeBruijnAnn ann) where
  alterAnn body var (RecAnn expr ann) = RecAnn <$> alter body var expr <*> pure ann

instance MutableAnn Provenance where
  alterAnn _ _ ann = return ann

--------------------------------------------------------------------------------
-- Concrete operations

-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html

-- | Lift all deBruin indices that refer to environment variables by the provided depth.
liftDBIndices :: MutableAnn ann
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
substInto :: MutableAnn ann
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
    binderUpdate sub = liftDBIndices 1 sub

type Substitution ann = IntMap (DeBruijnExpr ann)

-- TODO: explain what this means:
--   ?X i2 i4 i1 --> [i2 -> 2, i4 -> 1, i1 -> 0]

patternOfArgs :: [DeBruijnArg ann] -> Maybe (Substitution ann)
patternOfArgs args = go (length args - 1) IM.empty args
  where go _ revMap [] = Just revMap
        -- TODO: we could eta-reduce arguments too, if possible
        go i revMap (Arg _ _ (Var ann (Bound j)) : restArgs) =
          if IM.member j revMap then
            Nothing -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning; but then we should make sure the solution is well-typed
          else
            go (i-1) (IM.insert j (Var ann (Bound i)) revMap) restArgs
        go _ _ _ = Nothing

substAll :: MutableAnn ann
         => Substitution ann
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
