module Vehicle.Data.QuantifiedVariable
  ( TensorVariable (..),
    reduceTensorVariable,
    TensorVariableInfo (..),
    UserOrNetworkTensorVariable,
    UserTensorVariable,
    mkUserTensorVariable,
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

-- | Tensor variables that are bound by the user in their original program via
-- either `forall` or `exists` quantifiers. May have any shape.
newtype UserTensorVariable = UserTensorVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike UserTensorVariable where
  toLv = coerce

instance NFData UserTensorVariable

instance ToJSON UserTensorVariable

instance FromJSON UserTensorVariable

mkUserTensorVariable :: Lv -> UserTensorVariable
mkUserTensorVariable = UserTensorVariable

-- | Tensor variables that represent the inputs and outputs of a network
-- application and are introduced by the compiler. May have any shape.
newtype NetworkTensorVariable = NetworkTensorVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike NetworkTensorVariable where
  toLv = coerce

instance NFData NetworkTensorVariable

instance ToJSON NetworkTensorVariable

instance FromJSON NetworkTensorVariable

mkNetworkTensorVariable :: Lv -> NetworkTensorVariable
mkNetworkTensorVariable = NetworkTensorVariable

-- | Variables that may be either tensor variables or user variables.
newtype UserOrNetworkTensorVariable = UserOrNetworkTensorVariable Lv
  deriving (Show, Eq, Ord, Generic)

instance VariableLike UserOrNetworkTensorVariable where
  toLv = coerce

instance NFData UserOrNetworkTensorVariable

instance ToJSON UserOrNetworkTensorVariable

instance ToJSONKey UserOrNetworkTensorVariable

instance FromJSON UserOrNetworkTensorVariable

instance FromJSONKey UserOrNetworkTensorVariable

class (VariableLike variable) => TensorVariable variable where
  toTensorVar :: variable -> UserOrNetworkTensorVariable

instance TensorVariable UserTensorVariable where
  toTensorVar = coerce

instance TensorVariable NetworkTensorVariable where
  toTensorVar = coerce

instance TensorVariable UserOrNetworkTensorVariable where
  toTensorVar = coerce

--------------------------------------------------------------------------------
-- Element variables

newtype NetworkElementVariable = NetworkElementVariable Lv
  deriving (Ord, Eq)

reduceTensorVariable ::
  Lv ->
  Name ->
  TensorShape ->
  ([Name], Tensor NetworkElementVariable, Value Builtin)
reduceTensorVariable lv varName shape = runSupply [lv + 1 ..] $ go shape []
  where
    elementVariable :: TensorIndices -> Lv -> ([Name], Tensor NetworkElementVariable, Value Builtin)
    elementVariable indices currentLv = do
      let name = varName <> Text.pack (showTensorIndices indices)
      ([name], ZeroDimTensor $ NetworkElementVariable currentLv, VBoundVar currentLv [])

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([Name], Tensor NetworkElementVariable, Value Builtin)
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
