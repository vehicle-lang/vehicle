module Vehicle.Compile.Queries.Variable where

import Prettyprinter (brackets)
import Data.Text (Text)
import Data.Text qualified as Text (pack)

import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Variable class

class Pretty variable => IsVariable variable

--------------------------------------------------------------------------------
-- User variables

-- | Variables entered by the user
newtype UserVariable = UserVariable Name

instance Pretty UserVariable where
  pretty (UserVariable name) = "user" <> brackets (pretty name)

instance IsVariable UserVariable

--------------------------------------------------------------------------------
-- Network variables

-- | Network input and output variables
data NetworkVariable = NetworkVariable
  -- | The name of the network this variable belongs to.
  { networkName   :: Name
  -- | If there are multiple applications of this network, this records which
  -- application it is.
  , application   :: Maybe Int
  -- | Whether its an input or an output variable
  , inputOrOutput :: InputOrOutput
  -- | The index of the input or output variable.
  , position      :: Int
  }

instance Pretty NetworkVariable where
  pretty NetworkVariable{..} =
    pretty networkName <>
    maybe "" (brackets . pretty) application <>
    brackets (pretty inputOrOutput <> pretty position)

instance IsVariable NetworkVariable

sequentialIONetworkVariableNaming :: Text -> Text -> [NetworkVariable] -> [Name]
sequentialIONetworkVariableNaming inputPrefix outputPrefix variables = do
  let (_, _, result) = foldl forNetwork (0, 0, []) variables
  reverse result
  where
  forNetwork :: (Int, Int, [Name]) -> NetworkVariable -> (Int, Int, [Name])
  forNetwork (inputIndex, outputIndex, result) NetworkVariable{..}=
    case inputOrOutput of
      Input -> do
        let name = inputPrefix <> Text.pack (show inputIndex)
        (inputIndex + 1, outputIndex, name : result)
      Output -> do
        let name = outputPrefix <> Text.pack (show outputIndex)
        (inputIndex, outputIndex + 1, name : result)

--------------------------------------------------------------------------------
-- All variables

-- | Both user and network variables
data Variable
  = UserVar UserVariable
  | NetworkVar NetworkVariable

instance Pretty Variable where
  pretty = \case
    UserVar    v -> pretty v
    NetworkVar v -> pretty v

instance IsVariable Variable

getNetworkVariable :: Variable -> Maybe NetworkVariable
getNetworkVariable = \case
  UserVar{}      -> Nothing
  NetworkVar var -> Just var
