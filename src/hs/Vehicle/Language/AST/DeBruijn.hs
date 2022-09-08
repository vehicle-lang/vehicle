{-# LANGUAGE PartialTypeSignatures #-}

module Vehicle.Language.AST.DeBruijn
  ( DBVar(..)
  , DBBinding
  , DBIndex
  , DBExpr
  , DBDecl
  , DBProg
  , DBArg
  , DBBinder
  , BindingDepth
  , liftFreeDBIndices
  , substInto
  , substIntoAtLevel
  , substAll
  , getArgPattern
  , reverseArgPattern
  , patternToSubst
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, Reader, ask, runReader, runReaderT, local, lift)
import Data.Hashable (Hashable(..))
import Data.Tuple (swap)

import Vehicle.Prelude
import Vehicle.Language.AST.Core

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Vehicle.Language.AST.Provenance

--------------------------------------------------------------------------------
-- Definitions

-- | A DeBruijn index pointing to the binder that the variable refers to.
type DBIndex = Int

-- |The type of data DeBruijn indices store at name sites
data DBVar
  = Free Identifier
  | Bound DBIndex
  deriving (Eq, Ord, Show, Generic)

instance NFData DBVar

instance Hashable DBVar

-- |The type of the data DeBruijn notation stores at binding sites.
type DBBinding = Maybe Symbol

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DBBinder = Binder DBBinding DBVar
type DBArg    = Arg    DBBinding DBVar
type DBExpr   = Expr   DBBinding DBVar
type DBDecl   = Decl   DBBinding DBVar
type DBProg   = Prog   DBBinding DBVar

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

-- | A type-synonym for the function that is used to update a bound variable
type UpdateVariable m state
  =  DBIndex               -- The old deBruijn index of the variable
  -> Provenance            -- The annotation of the variable
  -> (BindingDepth, state) -- How many binders the variable is under & the current state
  -> m DBExpr              -- The resulting expression

-- | A type-synonym for the function that is used to update the operation's state
-- when traversing across a binder.
type TraverseBinder state = state -> state

class DeBruijnFunctor a where
  alter
    :: (MonadReader (BindingDepth, state) m)
    => TraverseBinder state
    -> UpdateVariable m state
    -> a
    -> m a

instance DeBruijnFunctor DBExpr where
  alter body var =
    let
      altPiBinder  = alter    body var
      altLamBinder = alter    body var
      altArg       = alter    body var
      altExpr      = alter    body var
      underB       = underBinder body
    in \case
      Universe ann l            -> return (Universe ann l)
      Meta     ann m            -> return (Meta ann m)
      Hole     ann name         -> return (Hole ann name)
      Builtin  ann op           -> return (Builtin ann op)
      Literal  ann l            -> return (Literal ann l)
      LVec     ann es           -> LVec    ann <$> traverse altExpr es
      Ann      ann term typ     -> Ann     ann <$> altExpr   term   <*> altExpr typ
      App      ann fun args     -> normApp ann <$> altExpr   fun    <*> traverse altArg args
      Pi       ann binder res   -> Pi      ann <$> altPiBinder binder <*> underB (altExpr res)
      Let      ann e1 binder e2 -> Let     ann <$> altExpr e1 <*> altLamBinder binder <*> underB (altExpr e2)
      Lam      ann binder e     -> Lam     ann <$> altLamBinder binder <*> underB (altExpr e)
      Var      ann (Free i)     -> return (Var ann (Free i))
      Var      ann (Bound i)    -> var i ann =<< ask

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, state) m =>
               TraverseBinder state -> m a -> m a
underBinder body = local (\(d, s) -> (d+1, body s))

instance DeBruijnFunctor DBArg where
  alter body var = traverseArgExpr (alter body var)

instance DeBruijnFunctor DBBinder where
  alter body var = traverseBinderType (alter body var)

--------------------------------------------------------------------------------
-- Concrete operations

-- Code loosely based off of:
-- http://blog.discus-lang.org/2011/08/how-i-learned-to-stop-worrying-and-love.html

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftFreeDBIndices :: BindingDepth -- ^ amount to lift by
                  -> DBExpr       -- ^ expression to lift
                  -> DBExpr       -- ^ the result of the lifting
liftFreeDBIndices 0 e = e
liftFreeDBIndices j e = runReader (alter id alterVar e) (0 , ())
  where
    alterVar :: UpdateVariable (Reader (BindingDepth, ())) ()
    alterVar i ann (d, _) = return (Var ann (Bound i'))
      where
        i' | d <= i    = i + j -- Index is referencing the environment so increment
           | otherwise = i     -- Index is locally bound so no need to increment

-- | De Bruijn aware substitution of one expression into another
substIntoAtLevel :: BindingDepth -- ^ The current binding depth
                 -> DBExpr       -- ^ expression to substitute
                 -> DBExpr       -- ^ term to substitute into
                 -> DBExpr       -- ^ the result of the substitution
substIntoAtLevel level sub e = runReader (alter binderUpdate alterVar e) (level , sub)
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
    binderUpdate = liftFreeDBIndices 1

-- | De Bruijn aware substitution of one expression into another
substInto :: DBExpr -- ^ expression to substitute
          -> DBExpr -- ^ term to substitute into
          -> DBExpr -- ^ the result of the substitution
substInto = substIntoAtLevel 0

type Substitution = IntMap DBExpr

substAll :: Substitution
         -> DBExpr
         -> Maybe DBExpr
substAll sub e = runReaderT (alter binderUpdate alterVar e) (0, sub)
  where
    alterVar i ann (d, subst) =
      if i >= d then
        lift (IM.lookup (i - d) subst)
      else
        return $ Var ann (Bound i)

    binderUpdate = IM.map (liftFreeDBIndices 1)


--------------------------------------------------------------------------------
-- Argument patterns

newtype ArgPattern = ArgPattern (IntMap Int)

-- | TODO: explain what this means:
-- [i2 i4 i1] --> [2 -> 2, 4 -> 1, 1 -> 0]
getArgPattern :: [DBArg] -> Maybe ArgPattern
getArgPattern args = ArgPattern <$> go (length args - 1) IM.empty args
  where
    go :: Int -> IntMap Int -> [DBArg] -> Maybe (IntMap Int)
    go _ revMap [] = Just revMap
    -- TODO: we could eta-reduce arguments too, if possible
    go i revMap (arg : restArgs) =
      case argExpr arg of
        Var _ (Bound j) ->
           if IM.member j revMap then
            -- TODO: mark 'j' as ambiguous, and remove ambiguous entries before returning;
            -- but then we should make sure the solution is well-typed
            Nothing
          else
            go (i-1) (IM.insert j i revMap) restArgs
        _ -> Nothing

reverseArgPattern :: ArgPattern -> ArgPattern
reverseArgPattern (ArgPattern xs) = ArgPattern $ IM.fromList (fmap swap (IM.toList xs))

patternToSubst :: ArgPattern -> Substitution
patternToSubst (ArgPattern xs) = fmap (Var mempty . Bound) xs