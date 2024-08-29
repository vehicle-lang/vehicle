{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.DeBruijn
  ( Ix (..),
    Lv (..),
    Substitution,
    Substitutable (..),
    dbLevelToIndex,
    dbIndexToLevel,
    shiftDBIndex,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Reader (MonadReader (..))
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Indices

-- | A DeBruijn index pointing to the binder that the variable refers to,
-- counting from the variable position upwards.
newtype Ix = Ix
  { unIx :: Int
  }
  deriving (Eq, Ord, Num, Enum, Show, Generic)

instance NFData Ix

instance Hashable Ix

instance Serialize Ix

instance Pretty Ix where
  pretty i = "𝓲" <> pretty (unIx i)

--------------------------------------------------------------------------------
-- Levels

-- | DeBruijn level - represents how many binders deep we currently are.
-- (e.g. \f . f (\x . x)) the variable `f` is at level 0 and the variable `x`
-- is at level 1.
-- When used as a variable refers to the binder at that level.
newtype Lv = Lv
  { unLv :: Int
  }
  deriving (Eq, Ord, Num, Enum, Show, Generic)

instance NFData Lv

instance Hashable Lv

instance Serialize Lv

instance Pretty Lv where
  pretty l = "𝓵" <> pretty (unLv l)

-- | Converts a `Lv` x to a `Ix` given that we're currently at
-- level `l`.
dbLevelToIndex :: Lv -> Lv -> Ix
dbLevelToIndex l x = Ix (unLv l - unLv x - 1)

-- | Converts a `Lv` x to a `Ix` given that we're currently at
-- level `l`.
dbIndexToLevel :: Lv -> Ix -> Lv
dbIndexToLevel l x = Lv (unLv l - unIx x - 1)

shiftDBIndex :: Ix -> Lv -> Ix
shiftDBIndex i l = Ix (unIx i + unLv l)

--------------------------------------------------------------------------------
-- Substitution

type Substitution value = Ix -> Either Ix value

class Substitutable value target | target -> value where
  subst :: (MonadReader (Lv, Substitution value) m) => target -> m target

instance (Substitutable expr expr) => Substitutable expr (GenericArg expr) where
  subst = traverse subst

instance (Substitutable expr expr) => Substitutable expr (GenericBinder expr) where
  subst = traverse subst
