{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Level
  ( module Core,
    TensorVariableLike (..),
    TensorVariable (..),
    UserTensorVariable (..),
    NetworkInputVariable (..),
    NetworkOutputVariable (..),
    SliceVariable (..),
    UserSliceVariable (..),
    NetworkIOVariable (..),
    NetworkIOElementVariable (..),
    SliceVariableLike (..),
    SlicePath (..),
    -- Nested variables
    NestedSliceVariable (..),
    childVariablesOf,
    equivalentShapeOf,
    elementVariablesOf,
    numberOfSliceVariablesIn,
    findSliceShape,
    findSliceIndices,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Generics (Generic)
import Vehicle.Compile.Resource (AbstractNetworkModality (..), NetworkModality, NetworkTensorModality (..))
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
-- NetworkInputVariable

-- | Variables introduced by the compiler that represent a tensor or record which
-- is used to represent the input to a neural network.
newtype NetworkInputVariable = NetworkInputVariable TensorVariable
  deriving (Eq, Ord, Show)

instance VariableLike NetworkInputVariable where
  toLv = coerce

instance TensorVariableLike NetworkInputVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkInputVariable where
  toSliceVar = coerce

---------------------------------------------------------------------------------
-- NetworkOutputVariable

-- | Variables introduced by the compiler that represent a tensor or record which
-- is used to represent the output of a neural network.
newtype NetworkOutputVariable = NetworkOutputVariable Lv
  deriving (Eq, Ord)

instance VariableLike NetworkOutputVariable where
  toLv = coerce

instance TensorVariableLike NetworkOutputVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkOutputVariable where
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

instance VariableLike NestedSliceVariable where
  toLv = toLv . nestedStartingVariable

instance SliceVariableLike NestedSliceVariable where
  toSliceVar = nestedStartingVariable

numberOfSliceVariablesIn :: TensorShape -> Int
numberOfSliceVariablesIn shape = sum $ NonEmpty.scanl (*) 1 shape

equivalentShapeOf :: NetworkTensorModality TensorShape -> TensorShape
equivalentShapeOf (TensorModality ds) = ds
equivalentShapeOf (RecordModality _ fields@(a :| _)) = length fields : equivalentShapeOf (snd a)

childVariablesOf :: NestedSliceVariable -> Maybe [NestedSliceVariable]
childVariablesOf (NestedSliceVariable shapeOrShapes startingVar) = case shapeOrShapes of
  UniModal tensorOrRecord -> getUniModalChildren (toLv startingVar) tensorOrRecord
  MultiModal _typ shapes -> Just $ getMultiModalChildren (toLv startingVar) shapes
  where
    getUniModalChildren :: Lv -> NetworkTensorModality TensorShape -> Maybe [NestedSliceVariable]
    getUniModalChildren start shape = case shape of
      TensorModality [] -> Nothing
      TensorModality (d : ds) -> Just $ do
        let subSize = numberOfSliceVariablesIn ds
        let calculateChildStartingVar i = SliceVariable $ toLv start + Lv (1 + subSize * i)
        fmap (NestedSliceVariable (UniModal . TensorModality $ ds) . calculateChildStartingVar) [0 .. d - 1]
      RecordModality _ fields -> Just $ do
        let numberedFields = NonEmpty.zip [0 .. (length fields - 1)] fields
        let subSize = numberOfSliceVariablesIn (equivalentShapeOf $ snd $ NonEmpty.head fields)
        let calculateChildStartingVar i = SliceVariable $ toLv start + Lv (1 + subSize * i)
        fmap (\(i, (_, fieldShape)) -> NestedSliceVariable (UniModal fieldShape) (calculateChildStartingVar i)) (NonEmpty.toList numberedFields)

    getMultiModalChildren :: Lv -> [(Name, NetworkTensorModality TensorShape)] -> [NestedSliceVariable]
    getMultiModalChildren _ [] = []
    getMultiModalChildren start ((_, shape) : rest) = do
      let first = NestedSliceVariable (UniModal shape) (SliceVariable start)
      let next = start + Lv (numberOfSliceVariablesIn $ equivalentShapeOf shape)
      first : getMultiModalChildren next rest

elementVariablesOf :: NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
elementVariablesOf = go mempty
  where
    go :: TensorIndices -> NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
    go indices var = case childVariablesOf var of
      Nothing -> [(coerce (nestedStartingVariable var), reverse indices)]
      Just childVars -> concatMap (\(v, index) -> go (index : indices) v) $ zip childVars [0 ..]

-- | Returns the shape of the provided slice variable given its parent variable
findSliceShape :: NestedSliceVariable -> SliceVariable -> NetworkModality TensorShape
findSliceShape (NestedSliceVariable shapeOrShapes lv) var =
  if startIndex == 0
    then shapeOrShapes
    else case shapeOrShapes of
      UniModal shape -> goUniModal startIndex shape
      MultiModal _ shapes -> goMultiModal startIndex (map snd shapes)
  where
    startIndex :: Int
    startIndex = unLv $ toLv var - toLv lv

    goUniModal :: Int -> NetworkTensorModality TensorShape -> NetworkModality TensorShape
    goUniModal 0 shape = UniModal shape
    goUniModal flatIndex (TensorModality shape) = UniModal (TensorModality $ goTensor flatIndex shape)
    goUniModal flatIndex (RecordModality _ ((_, shape) :| _)) = do
      let newFlatIndex = (flatIndex - 1) `rem` numberOfSliceVariablesIn (equivalentShapeOf shape)
      goUniModal newFlatIndex shape

    goTensor :: Int -> TensorShape -> TensorShape
    goTensor 0 ds = ds
    goTensor _flatIndex [] = developerError "Malformed shape and index"
    goTensor flatIndex (_d : ds) = do
      let newFlatIndex = (flatIndex - 1) `rem` numberOfSliceVariablesIn ds
      goTensor newFlatIndex ds

    goMultiModal :: Int -> [NetworkTensorModality TensorShape] -> NetworkModality TensorShape
    goMultiModal _ [] = developerError "Malformed shape and index"
    goMultiModal index (shape : shapes)
      | index < numberOfSliceVariablesIn (equivalentShapeOf shape) = goUniModal index shape
      | otherwise = goMultiModal (index - numberOfSliceVariablesIn (equivalentShapeOf shape)) shapes

-- | The correct representation of an empty path is NoPath, rather than
-- a tensor path with no indices
data SlicePath
  = TensorPath Int SlicePath
  | RecordPath Name SlicePath
  | EndPath
  deriving (Show)

-- | Returns the indices into the provided parent variable of the provided slice variable
findSliceIndices :: (SliceVariableLike variable) => NestedSliceVariable -> variable -> SlicePath
findSliceIndices (NestedSliceVariable shapeOrShapes lv) var = case shapeOrShapes of
  UniModal shape -> findSliceInTensor startIndex shape
  MultiModal _ shapes -> findSliceInRecord startIndex shapes
  where
    startIndex :: Int
    startIndex = unLv $ toLv var - toLv lv

    findSliceInTensor :: Int -> NetworkTensorModality TensorShape -> SlicePath
    findSliceInTensor index = \case
      TensorModality shape -> goTensor index shape
      RecordModality _ fields -> findSliceInRecord index (NonEmpty.toList fields)

    findSliceInRecord :: Int -> [(Name, NetworkTensorModality TensorShape)] -> SlicePath
    findSliceInRecord _ [] = developerError "Malformed shape and index"
    findSliceInRecord index ((name, shape) : rest)
      | index < numberOfSliceVariablesIn (equivalentShapeOf shape) = RecordPath name (findSliceInTensor index shape)
      | otherwise = findSliceInRecord (index - numberOfSliceVariablesIn (equivalentShapeOf shape)) rest

    goTensor :: Int -> TensorShape -> SlicePath
    goTensor 0 _ = EndPath
    goTensor _flatIndex [] = developerError "Malformed shape and index"
    goTensor flatIndex (_d : ds) = do
      let newIndex = (flatIndex - 1) `div` numberOfSliceVariablesIn ds
      let newFlatIndex = (flatIndex - 1) `rem` numberOfSliceVariablesIn ds
      TensorPath newIndex (goTensor newFlatIndex ds)
