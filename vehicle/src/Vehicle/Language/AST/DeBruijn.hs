{-# LANGUAGE PartialTypeSignatures #-}

module Vehicle.Language.AST.DeBruijn
  ( DBVar(..)
  , DBBinding
  , DBIndex
  , BindingDepth
  , Substitution
  , Substitutable(..)
  , HasDBVariables(..)
  , substInto
  , substIntoAtLevel
  , substAll
  , liftDBIndices
  , underBinder
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader, local, runReader)
import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable (..))
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IM
import GHC.Generics (Generic)

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance
import Vehicle.Language.AST.Binder (GenericBinder)
import Vehicle.Language.AST.Arg (GenericArg(..))
import Vehicle.Language.AST.Name

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

class HasDBVariables a where
  mkVar :: Provenance -> DBIndex -> a


type Substitution value = Provenance -> DBIndex -> Maybe value

class Substitutable value target | target -> value where
  subst :: MonadReader (BindingDepth, Substitution value) m => target -> m target

  substitute :: Substitution value -> target -> target
  substitute su e = runReader (subst e) (0, su)

instance Substitutable expr expr => Substitutable expr (GenericArg expr)  where
  subst = traverse subst

instance Substitutable expr expr => Substitutable expr (GenericBinder binder expr) where
  subst = traverse subst

-- Temporarily go under a binder, increasing the binding depth by one
-- and shifting the current state.
underBinder :: MonadReader (BindingDepth, c) m => m a -> m a
underBinder = local (first (+1))

--------------------------------------------------------------------------------
-- Concrete operations

-- | Lift all DeBruijn indices that refer to environment variables by the
-- provided depth.
liftDBIndices :: (Substitutable value target, HasDBVariables value)
              => Int                            -- ^ amount to lift by
              -> target                         -- ^ target term to lift
              -> target                         -- ^ lifted term
liftDBIndices d = substitute (\p v -> Just $ mkVar p (v+d))

-- | De Bruijn aware substitution of one expression into another
substIntoAtLevel :: forall value target . (Substitutable value target, HasDBVariables value)
                 => DBIndex      -- ^ The index of the variable of which to substitute
                 -> value        -- ^ expression to substitute
                 -> target       -- ^ term to substitute into
                 -> target       -- ^ the result of the substitution
substIntoAtLevel level value = substitute substVar
  where
    substVar :: Provenance -> DBIndex -> Maybe value
    substVar p v
      | v > level  = Just (mkVar p (v - 1))
      | v == level = Just value
      | otherwise  = Nothing


-- | De Bruijn aware substitution of one expression into another
substInto :: (HasDBVariables value, Substitutable value target)
          => value  -- ^ expression to substitute
          -> target -- ^ term to substitute into
          -> target -- ^ the result of the substitution
substInto = substIntoAtLevel 0

substAll :: Substitutable value target => IntMap value -> target -> target
substAll su = substitute (\_p v -> v `IM.lookup` su)
