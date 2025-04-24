module Vehicle.Data.QuantifiedVariable
  ( TensorVariable (..),
    TensorVariableInfo (..),
    UserVariable (..),
    NetworkIOVariable (..),
    NetworkIOElementVariable (..),
    prettyRationalAsFloat,
    UserVariableAssignment (..),
    TensorVariableLike (..),
    variableValue,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Vehicle.Data.Builtin.Core
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
  fromLv = coerce

instance NFData NetworkIOVariable

instance ToJSON NetworkIOVariable

instance FromJSON NetworkIOVariable

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

newtype NetworkIOElementVariable = NetworkIOElementVariable Lv
  deriving (Ord, Eq, Generic)

instance NFData NetworkIOElementVariable

instance ToJSON NetworkIOElementVariable

instance FromJSON NetworkIOElementVariable

instance VariableLike NetworkIOElementVariable where
  toLv = coerce
  fromLv = coerce

instance TensorVariableLike NetworkIOElementVariable where
  toTensorVar = coerce

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
