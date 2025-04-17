module Vehicle.Data.QuantifiedVariable
  ( TensorVariable (..),
    reduceTensorVariable,
    TensorVariableInfo (..),
    UserOrNetworkTensorVariable,
    UserVariable,
    mkUserVariable,
    NetworkTensorVariable,
    mkNetworkTensorVariable,
    NetworkElementVariable,
    prettyRationalAsFloat,
    UserVariableAssignment (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (coerce)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Standard ()
import Vehicle.Data.Code.Interface (StackTensorArgs (..), mkDims, pattern INatLiteral, pattern INatType)
import Vehicle.Data.Code.LinearExpr (VariableLike (..))
import Vehicle.Data.Code.TypedView (RatTensorValue (..), fromRatTensorValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

-- | Tensor variables that are directly bound by the user in their original
-- program via `forall`/`exists` quantifiers. May have any shape.
newtype UserVariable = UserVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike UserVariable where
  toLv = coerce

instance NFData UserVariable

instance ToJSON UserVariable

instance FromJSON UserVariable

mkUserVariable :: Lv -> UserVariable
mkUserVariable = UserVariable

-- | Tensor variables that represent the top-level inputs and outputs of a network
-- application and are introduced by the compiler. May have any shape.
newtype NetworkIOVariable = NetworkIOVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike NetworkIOVariable where
  toLv = coerce

instance NFData NetworkIOVariable

instance ToJSON NetworkIOVariable

instance FromJSON NetworkIOVariable

mkNetworkTensorVariable :: Lv -> NetworkIOVariable
mkNetworkTensorVariable = NetworkTensorVariable

-- | Variables that may be either tensor variables or user variables.
newtype TensorVariable = TensorVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike TensorVariable where
  toLv = coerce

instance NFData TensorVariable

instance ToJSON TensorVariable

instance ToJSONKey TensorVariable

instance FromJSON TensorVariable

instance FromJSONKey TensorVariable

class (VariableLike variable) => TensorVariableLike variable where
  toTensorVar :: variable -> TensorVariable

instance TensorVariableLike UserVariable where
  toTensorVar = coerce

instance TensorVariableLike NetworkIOVariable where
  toTensorVar = coerce

--------------------------------------------------------------------------------
-- Element variables

newtype NetworkIOElementVariable = NetworkIOElementVariable Lv
  deriving (Ord, Eq)

reduceTensorVariable ::
  Lv ->
  Name ->
  TensorShape ->
  ([Name], Tensor TensorVariable, Value Builtin)
reduceTensorVariable lv varName shape = runSupply [lv + 1 ..] $ go shape []
  where
    elementVariable :: TensorIndices -> Lv -> ([Name], Tensor TensorVariable, Value Builtin)
    elementVariable indices currentLv = do
      let name = varName <> Text.pack (showTensorIndices indices)
      ([name], ZeroDimTensor $ TensorVariable currentLv, VBoundVar currentLv [])

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([Name], Tensor TensorVariable, Value Builtin)
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
        let varsExpr = fromRatTensorValue $ VRatStackTensor args
        return (varsNames, vars, varsExpr)

data TensorVariableInfo = TensorVariableInfo
  { -- | Variables for each of it's elements
    elementVariables :: Tensor NetworkElementVariable,
    -- | The tensor literal expression containing the element variables above.
    reducedVarExpr :: Value Builtin,
    -- | `Nothing` = user variable, `Input` = network input variable, `Output` = network output variable
    tensorVariableType :: Maybe InputOrOutput
  }

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
