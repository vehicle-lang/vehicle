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
    elementVariables :: [NetworkRationalVariable],
    -- | The tensor literal expression containing the element variables above.
    reducedNetworkVarExpr :: WHNFValue Builtin
  }

--------------------------------------------------------------------------------
-- Reduced variables

data ReducedVariable variable = ReducedVariable
  { originalVar :: variable,
    tensorIndices :: TensorIndices
  }
  deriving (Show, Eq, Ord, Generic)

instance (NFData variable) => NFData (ReducedVariable variable)

instance (Pretty variable) => Pretty (ReducedVariable variable) where
  pretty ReducedVariable {..} =
    pretty originalVar <> pretty (showTensorIndices tensorIndices)

instance (FromJSON variable) => FromJSON (ReducedVariable variable)

instance (FromJSON variable) => FromJSONKey (ReducedVariable variable)

instance (ToJSON variable) => ToJSON (ReducedVariable variable)

instance (ToJSON variable) => ToJSONKey (ReducedVariable variable)

instance (Hashable variable) => Hashable (ReducedVariable variable)

reduceVariable ::
  forall variable.
  (variable -> TensorShape) ->
  Lv ->
  variable ->
  ([(Lv, ReducedVariable variable)], WHNFValue Builtin)
reduceVariable varDims dbLevel var
  | null (varDims var) = createRatVar [] dbLevel
  | otherwise = runSupply (go (varDims var) []) [dbLevel ..]
  where
    createRatVar :: TensorIndices -> Lv -> ([(Lv, ReducedVariable variable)], WHNFValue Builtin)
    createRatVar indices lv = ([(lv, ReducedVariable var indices)], VBoundVar lv [])

    go ::
      TensorShape ->
      TensorIndices ->
      Supply Lv ([(Lv, ReducedVariable variable)], WHNFValue Builtin)
    go dims indices = case dims of
      [] -> createRatVar (reverse indices) <$> demand
      d : ds -> do
        -- Use the list monad to create a nested list of all possible indices into the tensor
        let allIndices = [0 .. d - 1]

        -- Generate the corresponding names from the indices
        (elementUserVars, subexprs) <- unzip <$> traverse (\i -> go ds (i : indices)) allIndices
        let userVars = concat elementUserVars
        return (userVars, mkVecExpr subexprs)

--------------------------------------------------------------------------------
-- Reduced user variables

-- | Variables entered by the user
type UserRationalVariable = ReducedVariable TensorVariable

type NetworkRationalVariable = ReducedVariable TensorVariable

--------------------------------------------------------------------------------
-- All variables

-- | Both user and network variables
data RationalVariable
  = UserRationalVar UserRationalVariable
  | NetworkRationalVar NetworkRationalVariable
  deriving (Show, Eq, Ord, Generic)

instance NFData RationalVariable

instance ToJSON RationalVariable

instance FromJSON RationalVariable

instance ToJSONKey RationalVariable

instance FromJSONKey RationalVariable

instance Pretty RationalVariable where
  pretty = \case
    UserRationalVar v -> pretty v
    NetworkRationalVar v -> pretty v

--------------------------------------------------------------------------------
-- Tensor variables

-- | Both user and network variables
data Variable
  = RationalVar RationalVariable
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
