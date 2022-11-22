{-# LANGUAGE PartialTypeSignatures #-}

module Vehicle.Language.AST.DeBruijn
  ( DBVar(..)
  , DBBinding
  , DBIndex
  , BindingDepth
  , Substitution
  , Substitutable(..)
  , substInto
  , substIntoAtLevel
  , substAll
  , liftDBIndices
  , underDBBinder
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, local, runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import GHC.Generics (Generic)
import Vehicle.Language.AST.Arg (GenericArg (..))
import Vehicle.Language.AST.Binder (GenericBinder)
import Vehicle.Language.AST.Name
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

-- | Used to track the number of binders we're underneath during a traversal of
-- an expression
type BindingDepth = Int

--------------------------------------------------------------------------------
-- A framework for writing generic operations on DeBruijn variables

type Substitution value = DBIndex -> Either DBIndex value

class Substitutable value target | target -> value where
  subst :: MonadReader (BindingDepth, Substitution value) m => target -> m target

  substitute :: BindingDepth -> Substitution value -> target -> target
  substitute depth sub e = runReader (subst e) (depth, sub)

instance Substitutable expr expr => Substitutable expr (GenericArg expr)  where
  subst = traverse subst

instance Substitutable expr expr => Substitutable expr (GenericBinder binder expr) where
  subst = traverse subst

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underDBBinder :: MonadReader (BindingDepth, c) m => m a -> m a
underDBBinder = local (first (+1))

--------------------------------------------------------------------------------
-- Concrete operations

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices :: Substitutable value target
              => Int                            -- ^ amount to lift by
              -> target                         -- ^ target term to lift
              -> target                         -- ^ lifted term
liftDBIndices d = substitute 0 (\v -> Left (v+d))

-- | De Bruijn aware substitution of one expression into another
substIntoAtLevel :: forall value target . Substitutable value target
                 => DBIndex      -- ^ The index of the variable of which to substitute
                 -> value        -- ^ expression to substitute
                 -> target       -- ^ term to substitute into
                 -> target       -- ^ the result of the substitution
substIntoAtLevel level value = substitute 0 substVar
  where
    substVar :: DBIndex -> Either DBIndex value
    substVar v
      | v == level = Right value
      | v > level  = Left (v - 1)
      | otherwise  = Left v

-- | De Bruijn aware substitution of one expression into another
substInto :: Substitutable value target
          => value  -- ^ expression to substitute
          -> target -- ^ term to substitute into
          -> target -- ^ the result of the substitution
substInto = substIntoAtLevel 0

substAll :: Substitutable value target
         => BindingDepth
         -> (DBIndex -> Maybe DBIndex)
         -> target
         -> target
substAll depth sub = substitute depth (\v -> maybe (Left v) Left (sub v))
