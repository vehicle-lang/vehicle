{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Level
  ( module Core,
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
    -- Nested variables
    NestedSliceVariable (..),
    childVariablesOf,
    elementVariablesOf,
    numberOfSliceVariablesIn,
    findSliceShape,
    findSliceIndices,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import Vehicle.Compile.Resource (NetworkModality (..))
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level.Core as Core
import Vehicle.Prelude

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
  deriving (Eq, Ord, Show)

instance VariableLike TensorVariable where
  toLv = coerce

instance SliceVariableLike TensorVariable where
  toSliceVar = coerce

class (SliceVariableLike variable) => TensorVariableLike variable where
  toTensorVar :: variable -> TensorVariable

instance TensorVariableLike TensorVariable where
  toTensorVar = id

--------------------------------------------------------------------------------
-- User Variables

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
  deriving (Eq, Ord, Show)

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

--------------------------------------------------------------------------------
-- NestedSliceVariable

-- | This represents a pyramid, e.g. for a tensor shape of [2,2,3] the pyramid
-- will represent 19 variables:
--  - 1 variable x representing the tensor
--  - 2 variables x!0 and x!1 representing the first dimensions
--  - 4 variables x!0!0, x!0!1, x!1!0, x!1!1 represnting the second dimensions
--  - 12 variables ... representing the element dimensions
--
-- We store it like this in order to maximise space efficiency.
data NestedSliceVariable = NestedSliceVariable
  { nestedTensorShape :: NetworkModality TensorShape,
    nestedStartingVariable :: SliceVariable
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData NestedSliceVariable

instance ToJSON NestedSliceVariable

instance FromJSON NestedSliceVariable

instance Pretty NestedSliceVariable where
  pretty (NestedSliceVariable shape l) = pretty (toLv l) <> ":" <+> pretty shape

instance HasShape NestedSliceVariable where
  shapeOf (NestedSliceVariable shapeOrShapes _) = case shapeOrShapes of
    UniModal shape -> shape
    MultiModal _shapes -> error "MultiModal IO is not implemented yet"

instance VariableLike NestedSliceVariable where
  toLv = toLv . nestedStartingVariable

instance SliceVariableLike NestedSliceVariable where
  toSliceVar = nestedStartingVariable

numberOfSliceVariablesIn :: TensorShape -> Int
numberOfSliceVariablesIn shape = sum $ NonEmpty.scanl (*) 1 shape

childVariablesOf :: NestedSliceVariable -> Maybe [NestedSliceVariable]
childVariablesOf (NestedSliceVariable shapeOrShapes startingVar) = case shapeOrShapes of
  UniModal shape -> getChildrenOf shape
  MultiModal _shapes -> error "MultiModal IO is not implemented yet"
  where
    getChildrenOf :: TensorShape -> Maybe [NestedSliceVariable]
    getChildrenOf s = case s of
      [] -> Nothing
      d : ds -> Just $ do
        let subSize = numberOfSliceVariablesIn ds
        let calculateChildStartingVar i = SliceVariable $ toLv startingVar + Lv (1 + subSize * i)
        fmap (NestedSliceVariable (UniModal ds) . calculateChildStartingVar) [0 .. d - 1]

elementVariablesOf :: NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
elementVariablesOf = go mempty
  where
    go :: TensorIndices -> NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
    go indices var = case childVariablesOf var of
      Nothing -> [(coerce (nestedStartingVariable var), reverse indices)]
      Just childVars -> concatMap (\(v, index) -> go (index : indices) v) $ zip childVars [0 ..]

-- | Returns the shape of the provided slice variable
findSliceShape :: NestedSliceVariable -> SliceVariable -> NetworkModality TensorShape
findSliceShape (NestedSliceVariable shapeOrShapes lv) var = case shapeOrShapes of
  UniModal shape -> UniModal (go startIndex shape)
  MultiModal _shapes -> error "MultiModal IO is not implemented yet"
  where
    startIndex :: Int
    startIndex = unLv $ toLv var - toLv lv

    go :: Int -> TensorShape -> TensorShape
    go 0 ds = ds
    go _flatIndex [] = developerError "Malformed shape and index"
    go flatIndex (_d : ds) = do
      let newFlatIndex = (flatIndex - 1) `rem` numberOfSliceVariablesIn ds
      go newFlatIndex ds

-- | Returns the indices into the provided parent variable of the provided slice variable
findSliceIndices :: (SliceVariableLike variable) => NestedSliceVariable -> variable -> TensorIndices
findSliceIndices (NestedSliceVariable shapeOrShapes lv) var = case shapeOrShapes of
  UniModal shape -> go mempty startIndex shape
  MultiModal _shapes -> error "MultiModal IO is not implemented yet"
  where
    startIndex :: Int
    startIndex = unLv $ toLv var - toLv lv

    go :: TensorIndices -> Int -> TensorShape -> TensorIndices
    go indices 0 _ = reverse indices
    go _indices _flatIndex [] = developerError "Malformed shape and index"
    go indices flatIndex (_d : ds) = do
      let newIndex = (flatIndex - 1) `div` numberOfSliceVariablesIn ds
      let newFlatIndex = (flatIndex - 1) `rem` numberOfSliceVariablesIn ds
      go (newIndex : indices) newFlatIndex ds
