{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.QuantifiedVariable
  ( TensorVariableLike (..),
    TensorVariable (..),
    UserTensorVariable (..),
    NetworkInputTensorVariable (..),
    NetworkOutputTensorVariable (..),
    SliceVariable (..),
    UserVariable (..),
    NetworkIOVariable (..),
    NetworkIOElementVariable (..),
    prettyRationalAsFloat,
    UserVariableAssignment (..),
    SliceVariableLike (..),
    variableValue,
    NestedSliceVariable,
    childVariablesOf,
    elementVariablesOf,
    nestedChildVariables,
    NestedTensorVariableCtx,
    lookupTensorVariable,
    lookupTensorVariableAndName,
    appendTensorVariableToNestedCtx,
    nestedCtxToNameCtx,
    emptyNestedCtx,
    findCorrespondingSliceVariable,
    findCorrespondingTensorVariable,
    findCorrespondingTensorVariables,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (coerce)
import Data.List (find, sortBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Compile.Context.Bound.Core (CompleteNamedBoundCtx, GenericBoundCtx)
import Vehicle.Data.Code.LinearExpr (VariableLike (..))
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

-- | Variables that represent a particular row-wise slice of a tensor.
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

-- | Variables that represent a whole tensor.
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
-- User Tensor variables

newtype UserTensorVariable = UserTensorVariable Lv
  deriving (Eq, Ord)

instance VariableLike UserTensorVariable where
  toLv = coerce

instance SliceVariableLike UserTensorVariable where
  toSliceVar = coerce

instance TensorVariableLike UserTensorVariable where
  toTensorVar = coerce

--------------------------------------------------------------------------------
-- Network Input Tensor variables

newtype NetworkInputTensorVariable = NetworkInputTensorVariable Lv
  deriving (Eq, Ord)

instance VariableLike NetworkInputTensorVariable where
  toLv = coerce

instance TensorVariableLike NetworkInputTensorVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkInputTensorVariable where
  toSliceVar = coerce

--------------------------------------------------------------------------------
-- Network Output Tensor variables

newtype NetworkOutputTensorVariable = NetworkOutputTensorVariable Lv
  deriving (Eq, Ord)

instance VariableLike NetworkOutputTensorVariable where
  toLv = coerce

instance TensorVariableLike NetworkOutputTensorVariable where
  toTensorVar = coerce

instance SliceVariableLike NetworkOutputTensorVariable where
  toSliceVar = coerce

-- | Tensor variables represent quantities that are directly bound by the user
-- in their original program via `forall`/`exists` quantifiers, e.g.
--
--   `forall (v : Tensor Rat 2)`
--
-- will get mapped to 3 variables
--
--   v = [v_0, v_1]
newtype UserVariable = UserVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike UserVariable where
  toLv = coerce

instance NFData UserVariable

instance ToJSON UserVariable

instance FromJSON UserVariable

variableValue :: (VariableLike variable) => variable -> Value builtin
variableValue var = VBoundVar (toLv var) []

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

instance SliceVariableLike UserVariable where
  toSliceVar = coerce

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
-- Represents all tensor variables of a given shape

-- This represents a pyramid, e.g. for a tensor shape of [2,2,3] the pyramid
-- will represent 19 variables:
--  - 1 variable x representing the tensor
--  - 2 variables x!0 and x!1 representing the first dimensions
--  - 4 variables x!0!0, x!0!1, x!1!0, x!1!1 represnting the second dimensions
--  - 12 variables ... representing the element dimensions
--
-- We store it like this in order to maximise space efficiency.
data NestedSliceVariable = NestedSliceVariable
  { nestedTensorShape :: TensorShape,
    nestedStartingVariable :: SliceVariable
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData NestedSliceVariable

instance ToJSON NestedSliceVariable

instance FromJSON NestedSliceVariable

instance HasShape NestedSliceVariable where
  shapeOf = nestedTensorShape

instance VariableLike NestedSliceVariable where
  toLv = toLv . nestedStartingVariable

instance SliceVariableLike NestedSliceVariable where
  toSliceVar = nestedStartingVariable

nestedVariablesSize :: TensorShape -> Int
nestedVariablesSize shape = sum $ NonEmpty.scanl (*) 1 shape

childVariablesOf :: NestedSliceVariable -> Maybe [NestedSliceVariable]
childVariablesOf (NestedSliceVariable shape startingVar) = case shape of
  [] -> Nothing
  d : ds -> Just $ do
    let subSize = nestedVariablesSize ds
    let calculateChildStartingVar i = SliceVariable $ toLv startingVar + Lv (1 + subSize * i)
    fmap (NestedSliceVariable ds . calculateChildStartingVar) [0 .. d - 1]

nestedChildVariables :: NestedSliceVariable -> Maybe [SliceVariable]
nestedChildVariables = fmap (fmap nestedStartingVariable) . childVariablesOf

elementVariablesOf :: NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
elementVariablesOf = go mempty
  where
    go :: TensorIndices -> NestedSliceVariable -> [(NetworkIOElementVariable, TensorIndices)]
    go indices var = case childVariablesOf var of
      Nothing -> [(coerce (nestedStartingVariable var), reverse indices)]
      Just childVars -> concatMap (\(v, index) -> go (index : indices) v) $ zip childVars [0 ..]

--------------------------------------------------------------------------------
-- Tensor variables

data NestedTensorVariableCtx = NestedTensorVariableCtx
  { _ctx :: GenericBoundCtx (NestedSliceVariable, Name),
    _name :: CompleteNamedBoundCtx,
    _totalSize :: Int
  }

instance Pretty NestedTensorVariableCtx where
  pretty (NestedTensorVariableCtx _ n s) = pretty n <+> pretty s <+> pretty (length n)

emptyNestedCtx :: NestedTensorVariableCtx
emptyNestedCtx = NestedTensorVariableCtx mempty mempty 0

nestedCtxToNameCtx :: NestedTensorVariableCtx -> CompleteNamedBoundCtx
nestedCtxToNameCtx (NestedTensorVariableCtx _ nameCtx _) = nameCtx

lookupTensorVariableAndName ::
  NestedTensorVariableCtx ->
  TensorVariable ->
  (NestedSliceVariable, Name)
lookupTensorVariableAndName (NestedTensorVariableCtx ctx _ _) var = do
  case find (\(v, _) -> nestedStartingVariable v == toSliceVar var) ctx of
    Nothing -> developerError $ "Missing nested tensor variable" <+> pretty (toLv var)
    Just result -> result

lookupTensorVariable ::
  (TensorVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
lookupTensorVariable ctx var =
  fst $ lookupTensorVariableAndName ctx (toTensorVar var)

findCorrespondingSliceVariable ::
  (HasCallStack, SliceVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
findCorrespondingSliceVariable ctx var = do
  let parentVar = findCorrespondingTensorVariable ctx var
  let shape = findShape (shapeOf parentVar) (unLv $ toLv var - toLv parentVar)
  NestedSliceVariable shape (toSliceVar var)
  where
    findShape :: TensorShape -> Int -> TensorShape
    findShape shape 0 = shape
    findShape [] _index = developerError "Malformed shape and index"
    findShape (_d : ds) index = findShape ds ((index - 1) `rem` nestedVariablesSize ds)

findCorrespondingTensorVariable ::
  (SliceVariableLike variable) =>
  NestedTensorVariableCtx ->
  variable ->
  NestedSliceVariable
findCorrespondingTensorVariable ctx var =
  -- TODO turn this into a binary search for added efficiency?
  case findCorrespondingTensorSliceVariables ctx (Set.singleton (toSliceVar var)) of
    [v] -> v
    _ -> developerError "Missing variable"

-- | Given a set of variables representing the slices of a given set of tensors
-- returns the set of tensor variables that those slices are taken from.
findCorrespondingTensorVariables ::
  NestedTensorVariableCtx ->
  Set SliceVariable ->
  Set TensorVariable
findCorrespondingTensorVariables ctx sliceVars = do
  let result = findCorrespondingTensorSliceVariables ctx sliceVars
  Set.fromList $ fmap (coerce . nestedStartingVariable) result

findCorrespondingTensorSliceVariables ::
  NestedTensorVariableCtx ->
  Set SliceVariable ->
  [NestedSliceVariable]
findCorrespondingTensorSliceVariables (NestedTensorVariableCtx wholeCtx _ _) vars = do
  let sortedVarList = sortBy (comparing Down) (Set.toList vars)
  go wholeCtx sortedVarList
  where
    go :: GenericBoundCtx (NestedSliceVariable, Name) -> [SliceVariable] -> [NestedSliceVariable]
    go [] _ = []
    go _ [] = []
    go ((tensorVar, _) : ctx) (v : vs) = do
      let startPoint = toLv tensorVar
      let endPoint = startPoint + Lv (nestedVariablesSize $ shapeOf tensorVar)
      if toLv v >= endPoint
        then developerError "Incorrectly sorted slice variables"
        else
          if toLv v < startPoint
            then go ctx (v : vs)
            else do
              let newVars = dropWhile (\u -> toLv u >= startPoint) vs
              tensorVar : go ctx newVars

appendTensorVariableToNestedCtx ::
  NestedTensorVariableCtx ->
  (Name, TensorShape) ->
  (SliceVariable, NestedTensorVariableCtx)
appendTensorVariableToNestedCtx (NestedTensorVariableCtx ctx nameCtx totalSize) (name, shape) = do
  let var = NestedSliceVariable shape (SliceVariable $ Lv totalSize)
  let newCtx = (var, name) : ctx
  let newNameCtx = reverse (fmap mkName (allIndices shape [])) <> nameCtx
  let newTotalSize = totalSize + nestedVariablesSize shape
  let newNestedCtx = NestedTensorVariableCtx newCtx newNameCtx newTotalSize
  (toSliceVar var, newNestedCtx)
  where
    mkName :: TensorIndices -> Name
    mkName indices = name <> Text.pack (showTensorIndices indices)

    allIndices :: TensorShape -> TensorIndices -> [TensorIndices]
    allIndices dims is =
      is : case dims of
        [] -> []
        d : ds -> concatMap (\i -> allIndices ds (i : is)) ([0 .. d - 1] :: [Int])

--------------------------------------------------------------------------------
-- Constants

prettyRationalAsFloat :: Rational -> Doc a
prettyRationalAsFloat p = do
  let f = realToFrac p :: Double
  pretty $ showFFloat Nothing f ""

--------------------------------------------------------------------------------
-- User variable assignments

-- | A (satisfying) assignment to a set of user-level variables.
newtype UserVariableAssignment
  = UserVariableAssignment [(Name, RatTensor)]
  deriving (Generic)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

instance Pretty UserVariableAssignment where
  pretty (UserVariableAssignment assignment) =
    vsep (fmap pretty assignment)
