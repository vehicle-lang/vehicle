module Vehicle.Data.QuantifiedVariable where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Tensor variables

-- | Both user and network variables
data TensorVariable = TensorVariable
  { tensorVarName :: Name,
    tensorVarDimensions :: TensorShape
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData TensorVariable

instance Hashable TensorVariable

instance ToJSON TensorVariable

instance FromJSON TensorVariable

instance ToJSONKey TensorVariable

instance FromJSONKey TensorVariable

instance Pretty TensorVariable where
  pretty = pretty . tensorVarName

--------------------------------------------------------------------------------
-- Network tensor variables

data NetworkVariableInfo = NetworkVariableInfo
  { -- | Variables for each of it's elements
    elementVariables :: [NetworkElementVariable],
    -- | The tensor literal expression containing the element variables above.
    reducedNetworkVarExpr :: WHNFValue Builtin
  }

--------------------------------------------------------------------------------
-- Reduced variables

data ElementVariable = ElementVariable
  { originalVar :: TensorVariable,
    tensorIndices :: TensorIndices
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData ElementVariable

instance Pretty ElementVariable where
  pretty ElementVariable {..} =
    pretty originalVar <> pretty (showTensorIndices tensorIndices)

instance FromJSON ElementVariable

instance FromJSONKey ElementVariable

instance ToJSON ElementVariable

instance ToJSONKey ElementVariable

instance Hashable ElementVariable

reduceVariable ::
  Lv ->
  TensorVariable ->
  ([(Lv, ElementVariable)], WHNFValue Builtin)
reduceVariable dbLevel var
  | null (tensorVarDimensions var) = createRatVar [] dbLevel
  | otherwise = runSupply (go (tensorVarDimensions var) []) [dbLevel ..]
  where
    createRatVar :: TensorIndices -> Lv -> ([(Lv, ElementVariable)], WHNFValue Builtin)
    createRatVar indices lv = ([(lv, ElementVariable var indices)], VBoundVar lv [])

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([(Lv, ElementVariable)], WHNFValue Builtin)
    go dims indices = case dims of
      [] -> createRatVar (reverse indices) <$> demand
      d : ds -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. d - 1]

        -- Generate the corresponding names from the indices
        (elementUserVars, subexprs) <- unzip <$> traverse (\i -> go ds (i : indices)) allIndices
        let userVars = concat elementUserVars
        return (userVars, mkVecExpr subexprs)

-- | Variables entered by the user
type UserElementVariable = ElementVariable

type NetworkElementVariable = ElementVariable

--------------------------------------------------------------------------------
-- All variables

-- | Both tensor and element variables
data Variable
  = RationalVar ElementVariable
  | TensorVar TensorVariable
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Variable

instance FromJSON Variable

instance ToJSONKey Variable

instance FromJSONKey Variable

instance Pretty Variable where
  pretty = \case
    RationalVar v -> pretty v
    TensorVar v -> pretty v

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
  = UserVariableAssignment [(TensorVariable, RationalTensor)]
  deriving (Generic)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

instance Pretty UserVariableAssignment where
  pretty (UserVariableAssignment assignment) =
    vsep (fmap pretty assignment)
