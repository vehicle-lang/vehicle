{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Vehicle.Verify.Core where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char.SScript (subscript)
import Data.Map (Map)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Prettyprinter (brackets)
import System.FilePath ((<.>))
import Vehicle.Compile.Resource
import Vehicle.Data.Builtin.Core
import Vehicle.Data.QuantifiedVariable (NetworkElementVariable)
import Vehicle.Data.Tensor (TensorIndices, showTensorIndices)
import Vehicle.Prelude
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Meta-network

data NetworkContextInfo = NetworkContextInfo
  { networkFilepath :: FilePath,
    networkType :: NetworkType
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData NetworkContextInfo

instance ToJSON NetworkContextInfo

instance FromJSON NetworkContextInfo

data MetaNetworkEntry = MetaNetworkEntry
  { metaNetworkEntryName :: Name,
    metaNetworkEntryInfo :: NetworkContextInfo
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData MetaNetworkEntry

instance ToJSON MetaNetworkEntry

instance FromJSON MetaNetworkEntry

instance Pretty MetaNetworkEntry where
  pretty MetaNetworkEntry {..} =
    pretty metaNetworkEntryName
      <> ":"
      <> softline
      <> pretty (networkType metaNetworkEntryInfo)

-- | A mapping from query variables (i.e. those used by the verifier)
-- and the internal variable representation (i.e. those used by Vehicle)
type QueryVariableMapping = [(QueryVariable, NetworkElementVariable)]

-- | A list of neural networks used in a given query.
type MetaNetwork = [MetaNetworkEntry]

-- | A (satisfying) assignment to a set of reduced network-level variables.
newtype NetworkVariableAssignment
  = NetworkVariableAssignment (Map NetworkElementVariable Rational)

--------------------------------------------------------------------------------
-- Queries misc

-- | Location of a verifier query file.
type QueryFile = FilePath

type QueryText = Text

-- | Tracks whether or not the result of the query set should be negated.
-- Not that at first glance this might seem like it can be lifted to whether
-- or not the property is negated, but recall a property can have multiple
-- query sets. e.g. prop = (forall x . P x) and (exists x . Q y).
type QuerySetNegationStatus = Bool

--------------------------------------------------------------------------------
-- Query results

data QueryResult witness
  = SAT (Maybe witness)
  | UnSAT
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance (FromJSON witness) => FromJSON (QueryResult witness)

instance (ToJSON witness) => ToJSON (QueryResult witness)

instance (Pretty witness) => Pretty (QueryResult witness) where
  pretty = \case
    SAT w -> "SAT:" <+> pretty w
    UnSAT -> "UNSAT"

--------------------------------------------------------------------------------
-- Property addresses

-- | The number of an individual property within a specification.
type PropertyID = Int

-- | A name of a property in the specification.
type PropertyName = Name

-- | A set of properties in the specification.
type PropertyNames = [PropertyName]

-- | A unique identifier for every individual property that needs to be verified.
-- Not simply an identifier, as we need to identifier sub-properties in tensors of
-- properties.
data PropertyAddress = PropertyAddress
  { propertyID :: PropertyID,
    propertyName :: PropertyName,
    propertyIndices :: TensorIndices
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData PropertyAddress

instance ToJSON PropertyAddress

instance FromJSON PropertyAddress

instance Pretty PropertyAddress where
  pretty (PropertyAddress _ name indices) =
    concatWith (\a b -> a <> "!" <> b) (pretty name : fmap pretty indices)

calculatePropertyFilePrefix :: PropertyAddress -> FilePath
calculatePropertyFilePrefix (PropertyAddress _ propertyName propertyIndices) = do
  let indexStr
        | null propertyIndices = ""
        | otherwise = showTensorIndices propertyIndices
  unpack propertyName <> indexStr

--------------------------------------------------------------------------------
-- Addresses

-- | The number of an individual query within a `Property` when traversed
-- depth-first.
type QueryID = Int

type QueryAddress = (PropertyAddress, QueryID)

calculateQueryFileName :: QueryAddress -> FilePath
calculateQueryFileName (propertyAddress, queryID) = do
  calculatePropertyFilePrefix propertyAddress <> "-query" <> show queryID <.> "txt"

--------------------------------------------------------------------------------
-- Queries

data QueryRelation
  = EqualRel
  | OrderRel OrderOp
  deriving (Show, Eq, Ord)

instance Pretty QueryRelation where
  pretty = \case
    EqualRel -> "="
    OrderRel op -> pretty op

flipQueryRel :: QueryRelation -> QueryRelation
flipQueryRel = \case
  EqualRel -> EqualRel
  OrderRel op -> OrderRel (flipOrder op)

createNetworkVarName :: Name -> Int -> InputOrOutput -> Doc a
createNetworkVarName networkName application inputOrOutput =
  pretty networkName
    <> pretty (fmap subscript (show application))
    <> brackets (pretty inputOrOutput)

--------------------------------------------------------------------------------
-- Network assignments

{-
instance Pretty NetworkVariableAssignment where
  pretty (NetworkVariableAssignment assignment) = do
    vsep (prettyVariable <$> Map.toList assignment)
    where
      prettyVariable :: (NetworkVerifierVariable, Rational) -> Doc a
      prettyVariable (var, value) = "x" <> pretty var <> ":" <+> pretty value
-}
