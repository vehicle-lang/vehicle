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
  , Substitution
  , liftFreeDBIndices
  , substInto
  , substIntoAtLevel
  , substAll
  , getArgPattern
  , reverseArgPattern
  , patternToSubst
  , substitute
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, ask, local, runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IM
import Data.Tuple (swap)
import GHC.Generics (Generic)

import Vehicle.Language.AST.Core
import Vehicle.Language.AST.Provenance
import Vehicle.Prelude

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

type Substitution = DBIndex -> Maybe DBExpr

class Substitute a where
  subst :: MonadReader (BindingDepth, Substitution) m => a -> m a

  substitute :: Substitution -> a -> a
  substitute su e = runReader (subst e) (0, su)

instance Substitute DBExpr where
  subst = \case
    Var p (Bound i)    -> do
      (d, s) <- ask
      return $ if i < d then
        Var p (Bound i)
      else case s (i - d) of
        Nothing -> Var p (Bound i)
        Just v  -> if d > 0 then liftFreeDBIndices d v else v

    Universe p l            -> return $ Universe p l
    Meta     p m            -> return $ Meta p m
    Hole     p name         -> return $ Hole p name
    Builtin  p op           -> return $ Builtin p op
    Literal  p l            -> return $ Literal p l
    Var      p (Free i)     -> return $ Var p (Free i)

    LVec     p es           -> LVec    p <$> traverse subst es
    Ann      p term typ     -> Ann     p <$> subst   term   <*> subst typ
    App      p fun args     -> normApp p <$> subst   fun    <*> traverse subst args
    Pi       p binder res   -> Pi      p <$> traverse subst binder <*> underBinder (subst res)
    Let      p e1 binder e2 -> Let     p <$> subst e1 <*> traverse subst binder <*> underBinder (subst e2)
    Lam      p binder e     -> Lam     p <$> traverse subst binder <*> underBinder (subst e)

instance Substitute a => Substitute (GenericArg a) where
  subst = traverse subst

instance Substitute a => Substitute (GenericBinder binder a) where
  subst = traverse subst

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, Substitution) m =>
               m a -> m a
underBinder = local (first (+1))

--------------------------------------------------------------------------------
-- Concrete operations

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftFreeDBIndices :: Int          -- ^ amount to lift by
                  -> DBExpr       -- ^ expression to lift
                  -> DBExpr       -- ^ the result of the lifting
liftFreeDBIndices d e = substitute s e
  where
    p = provenanceOf e
    s v = Just $ Var p (Bound (v+d))

-- | De Bruijn aware substitution of one expression into another
substIntoAtLevel :: DBIndex      -- ^ The index of the variable of which to substitute
                 -> DBExpr       -- ^ expression to substitute
                 -> DBExpr       -- ^ term to substitute into
                 -> DBExpr       -- ^ the result of the substitution
substIntoAtLevel level value =
  substitute (\v -> if v == level then Just value else Nothing)

-- | De Bruijn aware substitution of one expression into another
substInto :: DBExpr -- ^ expression to substitute
          -> DBExpr -- ^ term to substitute into
          -> DBExpr -- ^ the result of the substitution
substInto = substIntoAtLevel 0

substAll :: IntMap DBExpr -> DBExpr -> DBExpr
substAll su = substitute (`IM.lookup` su)

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

patternToSubst :: ArgPattern -> IntMap DBExpr
patternToSubst (ArgPattern xs) = fmap (Var mempty . Bound) xs
