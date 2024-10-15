module Vehicle.Data.QuantifiedVariable
  ( Variable,
    makeTensorVariable,
    reduceTensorVariable,
    TensorVariableInfo (..),
    TensorVariable,
    UserElementVariable,
    ElementVariable,
    NetworkElementVariable,
    prettyRationalAsFloat,
    UserVariableAssignment (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.DeBruijn
import Vehicle.Data.Tensor
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Variables

-- | A variable. Empty indices means that is a tensor variable, otherwise
-- its a variable representing an element of a tensor.
type Variable = Name

makeTensorVariable :: Name -> Variable
makeTensorVariable = id

reduceTensorVariable ::
  Lv ->
  TensorVariable ->
  TensorShape ->
  ([(Lv, ElementVariable)], WHNFValue Builtin)
reduceTensorVariable dbLevel var shape = runSupply (go shape []) [dbLevel ..]
  where
    createRatVar :: TensorIndices -> Lv -> ([(Lv, ElementVariable)], WHNFValue Builtin)
    createRatVar indices lv = do
      let name = var <> Text.pack (showTensorIndices indices)
      ([(lv, name)], VBoundVar lv [])

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

type TensorVariable = Variable

type ElementVariable = Variable

-- | Variables entered by the user
type UserElementVariable = ElementVariable

type NetworkElementVariable = ElementVariable

data TensorVariableInfo = TensorVariableInfo
  { -- | Variables for each of it's elements
    elementVariables :: [NetworkElementVariable],
    -- | The tensor literal expression containing the element variables above.
    reducedVarExpr :: WHNFValue Builtin,
    -- The shape of the tensor
    tensorVariableShape :: TensorShape
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
  = UserVariableAssignment [(TensorVariable, RationalTensor)]
  deriving (Generic)

instance ToJSON UserVariableAssignment

instance FromJSON UserVariableAssignment

instance Pretty UserVariableAssignment where
  pretty (UserVariableAssignment assignment) =
    vsep (fmap pretty assignment)
