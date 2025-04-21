module Vehicle.Data.QuantifiedVariable
  ( TensorVariable (..),
    reduceTensorVariable,
    TensorVariableInfo (..),
    UserVariable (..),
    mkUserVariable,
    NetworkIOVariable (..),
    mkNetworkTensorVariable,
    NetworkIOElementVariable (..),
    prettyRationalAsFloat,
    UserVariableAssignment (..),
    TensorVariableLike (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (coerce)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface (StackTensorArgs (..), accessStackTensor, mkDims, pattern INatLiteral, pattern INatType)
import Vehicle.Data.Code.LinearExpr (VariableLike (..))
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

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
  fromLv = coerce

instance NFData UserVariable

instance ToJSON UserVariable

instance FromJSON UserVariable

mkUserVariable :: Lv -> UserVariable
mkUserVariable = UserVariable

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
  fromLv = coerce

instance NFData NetworkIOVariable

instance ToJSON NetworkIOVariable

instance FromJSON NetworkIOVariable

mkNetworkTensorVariable :: Lv -> NetworkIOVariable
mkNetworkTensorVariable = NetworkIOVariable

-- | Variables that may be either be a `NetworkIOVariable` or
-- a `UserVariable`, or variables that represent sub-tensors
-- within those variables.
newtype TensorVariable = TensorVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike TensorVariable where
  toLv = coerce
  fromLv = coerce

instance NFData TensorVariable

instance ToJSON TensorVariable

instance ToJSONKey TensorVariable

instance FromJSON TensorVariable

instance FromJSONKey TensorVariable

class (VariableLike variable) => TensorVariableLike variable where
  toTensorVar :: variable -> TensorVariable

instance TensorVariableLike TensorVariable where
  toTensorVar = coerce

instance TensorVariableLike UserVariable where
  toTensorVar = coerce

instance TensorVariableLike NetworkIOVariable where
  toTensorVar = coerce

--------------------------------------------------------------------------------
-- Element variables

data TensorVariableInfo = TensorVariableInfo
  { variableName :: Name,
    -- | If this variable represents a sub-tensor of a variable tensor
    -- then this stores the reference to that variable, and the index.
    parentVariable :: Maybe (TensorVariable, TensorIndices),
    -- | Variables for each of it's elements
    childrenVariables :: Maybe (Tensor TensorVariable, Value Builtin)
  }

reduceTensorVariable ::
  forall variable.
  (TensorVariableLike variable) =>
  variable ->
  Name ->
  TensorShape ->
  [TensorVariableInfo]
reduceTensorVariable var varName shape = do
  let (reducedVariablesInfo, reducedVariables) = case shape of
        [] -> (mempty, Nothing)
        _ -> do
          let (reducedVarsInfo, tensors, value) = runSupply [toLv var + 1 ..] $ go shape []
          (reducedVarsInfo, Just (tensors, value))
  let variableInfo =
        TensorVariableInfo
          { variableName = varName,
            parentVariable = Nothing,
            childrenVariables = reducedVariables
          }
  variableInfo : reducedVariablesInfo
  where
    elementVariable ::
      TensorIndices ->
      Lv ->
      ([TensorVariableInfo], Tensor TensorVariable, Value Builtin)
    elementVariable indices currentLv = do
      let name = varName <> Text.pack (showTensorIndices indices)
      let tensorVariableInfo = TensorVariableInfo name (Just (toTensorVar var, indices)) Nothing
      ([tensorVariableInfo], ZeroDimTensor $ fromLv currentLv, VBoundVar currentLv [])

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([TensorVariableInfo], Tensor TensorVariable, Value Builtin)
    go dims indices = case dims of
      [] -> elementVariable (reverse indices) <$> demand
      d : ds -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. d - 1]

        -- Generate the corresponding names from the indices
        (elementVarNames, elementVars, elementExprs) <- unzip3 <$> traverse (\i -> go ds (i : indices)) allIndices
        let varsNames = concat elementVarNames
        let vars = stack ds elementVars
        let args = StackTensorArgs (implicit INatType) (INatLiteral d) (implicit $ mkDims ds) elementExprs
        let varsExpr = mkExpr accessStackTensor args
        return (varsNames, vars, varsExpr)

newtype NetworkIOElementVariable = NetworkIOElementVariable Lv
  deriving (Ord, Eq)

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
