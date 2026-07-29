{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Level.Core where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics (Generic)
import Vehicle.Data.Variable.Bound.Index
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Generic interface

-- | A variable.
class (Eq variable, Ord variable) => VariableLike variable where
  toLv :: variable -> Lv

instance VariableLike Lv where
  toLv = id

--------------------------------------------------------------------------------
-- Levels

-- | DeBruijn level - represents how many binders deep we currently are.
-- (e.g. \f . f (\x . x)) the variable `f` is at level 0 and the variable `x`
-- is at level 1.
-- When used as a variable refers to the binder at that level.
newtype Lv = Lv
  { unLv :: Int
  }
  deriving (Eq, Ord, Num, Enum, Show, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance NFData Lv

instance Hashable Lv

instance Serialize Lv

instance Pretty Lv where
  pretty l = "𝓵" <> pretty (unLv l)

-- | Converts a `Lv` x to a `Ix` given that we're currently at
-- level `l`.
dbLevelToIndex :: (HasCallStack) => Lv -> Lv -> Ix
dbLevelToIndex l x = Ix (unLv l - unLv x - 1)

-- | Converts a `Lv` x to a `Ix` given that we're currently at
-- level `l`.
dbIndexToLevel :: Lv -> Ix -> Lv
dbIndexToLevel l x = Lv (unLv l - unIx x - 1)

shiftDBIndex :: Ix -> Lv -> Ix
shiftDBIndex i l = Ix (unIx i + unLv l)
