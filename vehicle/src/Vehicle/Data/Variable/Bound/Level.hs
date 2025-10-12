{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Level
  ( VariableLike (..),
    Lv (..),
    dbLevelToIndex,
    dbIndexToLevel,
    shiftDBIndex,
    TensorVariableLike (..),
    TensorVariable (..),
    UserTensorVariable (..),
    NetworkInputTensorVariable (..),
    NetworkOutputTensorVariable (..),
    SliceVariable (..),
    UserSliceVariable (..),
    NetworkIOVariable (..),
    NetworkIOElementVariable (..),
    SliceVariableLike (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (coerce)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
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
dbLevelToIndex :: Lv -> Lv -> Ix
dbLevelToIndex l x = Ix (unLv l - unLv x - 1)

-- | Converts a `Lv` x to a `Ix` given that we're currently at
-- level `l`.
dbIndexToLevel :: Lv -> Ix -> Lv
dbIndexToLevel l x = Lv (unLv l - unIx x - 1)

shiftDBIndex :: Ix -> Lv -> Ix
shiftDBIndex i l = Ix (unIx i + unLv l)

--------------------------------------------------------------------------------
-- Slice variables

-- | Variables that represent a particular row-wise slice of a tensor.
-- e.g. given a 2x2 tensor = [[a,b],[c,d]] then there are seven possible
-- slice variables:
--
--   * v1 = [[a,b],[c,d]]
--   * v2 = [a,b]
--   * v3 = a
--   * v4 = b
--   * v5 = [c,d]
--   * v6 = c
--   * v7 = d
newtype SliceVariable = SliceVariable Lv
  deriving (Show, Eq, Ord, Enum, Generic)

class (VariableLike variable) => SliceVariableLike variable where
  toSliceVar :: variable -> SliceVariable

instance SliceVariableLike SliceVariable where
  toSliceVar = coerce

instance VariableLike SliceVariable where
  toLv = coerce

instance NFData SliceVariable

instance ToJSON SliceVariable

instance ToJSONKey SliceVariable

instance FromJSON SliceVariable

instance FromJSONKey SliceVariable

--------------------------------------------------------------------------------
-- Tensor variables

-- | Variables that represent a whole tensor, rather than an individual slice.
newtype TensorVariable = TensorVariable SliceVariable
  deriving (Eq, Ord)

instance VariableLike TensorVariable where
  toLv = coerce

instance SliceVariableLike TensorVariable where
  toSliceVar = coerce

class (SliceVariableLike variable) => TensorVariableLike variable where
  toTensorVar :: variable -> TensorVariable

instance TensorVariableLike TensorVariable where
  toTensorVar = id

--------------------------------------------------------------------------------
-- UserSliceVariable

-- | SliceVariables introduced by the user via a quantifier.
newtype UserSliceVariable = UserSliceVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike UserSliceVariable where
  toLv = coerce

instance NFData UserSliceVariable

instance ToJSON UserSliceVariable

instance FromJSON UserSliceVariable

instance SliceVariableLike UserSliceVariable where
  toSliceVar = coerce

--------------------------------------------------------------------------------
-- UserTensorVariables

-- | TensorVariables introduced by the user via a quantifier.
newtype UserTensorVariable = UserTensorVariable TensorVariable
  deriving (Eq, Ord)

instance VariableLike UserTensorVariable where
  toLv = coerce

instance SliceVariableLike UserTensorVariable where
  toSliceVar = coerce

instance TensorVariableLike UserTensorVariable where
  toTensorVar = coerce

--------------------------------------------------------------------------------
-- NetworkInputTensorVariable

-- | Variables introduced by the compiler that represent a tensor which is used
-- to represent the input to a neural network.
newtype NetworkInputTensorVariable = NetworkInputTensorVariable TensorVariable
  deriving (Eq, Ord)

instance VariableLike NetworkInputTensorVariable where
  toLv = coerce

instance TensorVariableLike NetworkInputTensorVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkInputTensorVariable where
  toSliceVar = coerce

--------------------------------------------------------------------------------
-- NetworkOutputTensorVariable

-- | Variables introduced by the compiler that represent a tensor which is used
-- to represent the output of a neural network.
newtype NetworkOutputTensorVariable = NetworkOutputTensorVariable Lv
  deriving (Eq, Ord)

instance VariableLike NetworkOutputTensorVariable where
  toLv = coerce

instance TensorVariableLike NetworkOutputTensorVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkOutputTensorVariable where
  toSliceVar = coerce

-- | Tensor variables that represent quantities used as the direct
-- inputs and outputs of a network application.
-- They are introduced by the compiler.
-- For example,
--
--   @network f : Tensor Rat [1] -> Tensor Rat [2]
--
--   ... f <e> ...
--
-- gets mapped to the five variables
--
--   x = [x_0]
--   y = [y_0, y_1]
newtype NetworkIOVariable = NetworkIOVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike NetworkIOVariable where
  toLv = coerce

instance NFData NetworkIOVariable

instance ToJSON NetworkIOVariable

instance FromJSON NetworkIOVariable

instance SliceVariableLike NetworkIOVariable where
  toSliceVar = coerce

--------------------------------------------------------------------------------
-- Element variables

newtype NetworkIOElementVariable = NetworkIOElementVariable Lv
  deriving (Ord, Eq, Generic)

instance NFData NetworkIOElementVariable

instance ToJSON NetworkIOElementVariable

instance FromJSON NetworkIOElementVariable

instance VariableLike NetworkIOElementVariable where
  toLv = coerce

instance SliceVariableLike NetworkIOElementVariable where
  toSliceVar = coerce
