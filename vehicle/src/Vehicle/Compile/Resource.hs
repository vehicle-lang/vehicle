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

type NetworkIOType = NetworkModality NetworkIOBase

data NetworkModality a
  = UniModal a
  | MultiModal [(Name, a)]
  deriving (Eq, Ord, Show, Generic)

instance (NFData a) => NFData (NetworkModality a)

instance (ToJSON a) => ToJSON (NetworkModality a)

instance (FromJSON a) => FromJSON (NetworkModality a)

instance (Pretty a) => Pretty (NetworkModality a) where
  pretty (UniModal e) = pretty e
  pretty (MultiModal es) = pretty es

instance Functor NetworkModality where
  fmap f (UniModal a) = UniModal (f a)
  fmap f (MultiModal as) = MultiModal (fmap applyRight as)
    where
      applyRight (a, b) = (a, f b)

data NetworkTensorType = NetworkTensorType
  { baseType :: NetworkBaseType,
    dimensions :: TensorShape
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkTensorType

instance ToJSON NetworkTensorType

instance FromJSON NetworkTensorType

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

data NetworkIOBase
  = TensorIOType NetworkTensorType
  | RecordIOType NetworkRecordType
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkIOBase

instance ToJSON NetworkIOBase

instance FromJSON NetworkIOBase

instance Pretty NetworkIOBase where
  pretty = \case
    (TensorIOType (NetworkTensorType t dims)) -> "Tensor" <+> pretty t <+> pretty dims
    (RecordIOType (NetworkRecordType t ident dims fields)) ->
      "Record"
        <+> pretty ident
        <+> ":"
        <> line
        <> prettyMapEntries ((,typ) <$> map pretty fields)
      where
        typ = case dims of
          [] -> pretty t
          [_x] -> pretty t
          (_x : xs) -> "Tensor" <+> pretty t <+> pretty xs

getIODims :: NetworkIOBase -> TensorShape
getIODims (TensorIOType (NetworkTensorType _ dims)) = dims
getIODims (RecordIOType (NetworkRecordType _ _ dims _)) = dims

data NetworkBaseType
  = NetworkRatType
  deriving (Eq, Ord, Enum, Show, Generic)

instance NFData NetworkBaseType

instance ToJSON NetworkBaseType

instance FromJSON NetworkBaseType

instance Pretty NetworkBaseType where
  pretty = \case
    NetworkRatType -> pretty RatType
