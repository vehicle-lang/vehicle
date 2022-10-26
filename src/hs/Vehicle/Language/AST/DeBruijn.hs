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
  , traverseBoundVars
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, ask, runReader, runReaderT, local, lift, ReaderT)
import Control.Monad.Identity
import Data.Hashable (Hashable(..))
import Data.Tuple (swap)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Vehicle.Prelude
import Vehicle.Language.AST.Core
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
type DBBinding = Maybe Name

-- An expression that uses DeBruijn index scheme for both binders and variables.
type DBBinder = Binder DBBinding DBVar
type DBArg    = Arg    DBBinding DBVar
type DBExpr   = Expr   DBBinding DBVar
type DBDecl   = Decl   DBBinding DBVar
type DBProg   = Prog   DBBinding DBVar

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

traverseBoundVars :: Monad m
                  => TraverseBinder state
                  -> UpdateVariable m state
                  -> BindingDepth
                  -> state
                  -> DBExpr
                  -> m DBExpr
traverseBoundVars traverseBinder updateVar depth state e =
  runReaderT (alter traverseBinder updateVar e) (depth, state)

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
  alter :: Monad m
        => TraverseBinder state
        -> UpdateVariable m state
        -> a
        -> ReaderT (BindingDepth, state) m a

instance DeBruijnFunctor DBExpr where
  alter body var =
    let
      altPiBinder  = traverse (alter body var)
      altLamBinder = traverse (alter body var)
      altArg       = traverse (alter body var)
      altExpr      = alter body var
      underB       = underBinder body
    in \case
      Universe    p l            -> return (Universe p l)
      Meta        p m            -> return (Meta p m)
      Hole        p name         -> return (Hole p name)
      Builtin     p op           -> return (Builtin p op)
      Constructor p c            -> return (Constructor p c)
      Literal     p l            -> return (Literal p l)
      LVec        p es           -> LVec    p <$> traverse altExpr es
      Ann         p term typ     -> Ann     p <$> altExpr   term   <*> altExpr typ
      App         p fun args     -> normApp p <$> altExpr   fun    <*> traverse altArg args
      Pi          p binder res   -> Pi      p <$> altPiBinder binder <*> underB (altExpr res)
      Let         p e1 binder e2 -> Let     p <$> altExpr e1 <*> altLamBinder binder <*> underB (altExpr e2)
      Lam         p binder e     -> Lam     p <$> altLamBinder binder <*> underB (altExpr e)
      Var         p (Free i)     -> return (Var p (Free i))
      Var         p (Bound i)    -> lift <$> var i p =<< ask

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, state) m =>
               TraverseBinder state -> m a -> m a
underBinder body = local (\(d, s) -> (d+1, body s))

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
liftFreeDBIndices j e = runIdentity $ traverseBoundVars id alterVar 0 () e
  where
    alterVar :: UpdateVariable Identity ()
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
substAll = traverseBoundVars binderUpdate alterVar 0
  where
    alterVar :: DBIndex -> Provenance -> (BindingDepth, Substitution) -> Maybe DBExpr
    alterVar i ann (d, subst) =
      if i >= d then
        IM.lookup (i - d) subst
      else
        return $ Var ann (Bound i)

    binderUpdate :: Substitution -> Substitution
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