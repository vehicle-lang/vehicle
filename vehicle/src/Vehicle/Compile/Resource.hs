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
  { networkInputType :: NetworkIOType,
    networkOutputType :: NetworkIOType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkType

instance ToJSON NetworkType

instance FromJSON NetworkType

instance Pretty NetworkType where
  pretty (NetworkType input output) =
    pretty input <+> "->" <+> pretty output

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorShape
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkTensorType

instance ToJSON NetworkTensorType

instance FromJSON NetworkTensorType

instance Pretty NetworkTensorType where
  pretty (NetworkTensorType t dims) = "Tensor" <+> pretty t <+> pretty dims

data NetworkRecordType = NetworkRecordType
  { baseRecordType :: NetworkBaseType,
    recordTypeIdent :: Identifier,
    recordDims :: TensorShape, -- The dimensions of the tensor equivalent of the record
    recordFields :: [Name]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkRecordType

instance ToJSON NetworkRecordType

instance FromJSON NetworkRecordType

instance Pretty NetworkRecordType where
  pretty (NetworkRecordType t ident dims fields) =
    "Record" <+> pretty ident <+> ":" <+> encloseSep ("{" <> space) (space <> "}") ("," <> space) (map prettyField fields)
    where
      prettyField field = pretty (nameOf field) <+> ":" <+> fieldType dims
      fieldType ds = case ds of
        [] -> pretty t
        [_x] -> pretty t
        (_x : xs) -> "Tensor" <+> pretty t <+> pretty xs

data NetworkIOType
  = TensorIOType NetworkTensorType
  | RecordIOType NetworkRecordType
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkIOType

instance ToJSON NetworkIOType

instance FromJSON NetworkIOType

instance Pretty NetworkIOType where
  pretty = pretty

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
