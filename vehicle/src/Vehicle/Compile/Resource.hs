module Vehicle.Compile.Resource where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics
import Prettyprinter
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Networks

type NetworkName = Name

data NetworkType = NetworkType
  { inputTensor :: NetworkIOType,
    outputTensor :: NetworkIOType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkType

instance ToJSON NetworkType

instance FromJSON NetworkType

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

networkSize :: NetworkType -> Int
networkSize network = tensorSize (inputTensor network) + tensorSize (outputTensor network)

data NetworkIOType
  = NetworkTensorType
      { baseType :: NetworkBaseType,
        dimensions :: TensorShape
      }
  | NetworkRecordType
      { baseRecordType :: NetworkBaseType,
        recordTypeIdent :: Identifier,
        recordDimensions :: TensorShape,
        recordFields :: GenericRecordFieldNames
      }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkIOType

instance ToJSON NetworkIOType

instance FromJSON NetworkIOType

type GenericRecordFieldNames = [FieldName]

tensorSize :: NetworkIOType -> Int
tensorSize typ = case typ of
  NetworkTensorType _ dims -> product dims
  NetworkRecordType _ _ dims _ -> product dims

instance Pretty NetworkIOType where
  pretty = \case
    NetworkTensorType t dims -> "Tensor" <+> pretty t <+> pretty dims
    NetworkRecordType t ident dims fields ->
      "Record" <+> pretty ident <+> ":" <+> encloseSep ("{" <> space) (space <> "}") ("," <> space) (map prettyField fields)
      where
        prettyField field = pretty (nameOf field) <+> ":" <+> fieldType dims
        fieldType ds = case ds of
          [] -> pretty t
          [_x] -> pretty t
          (_x : xs) -> "Tensor" <+> pretty t <+> pretty xs

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
