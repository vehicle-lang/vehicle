{-# LANGUAGE DerivingVia #-}

module Vehicle.Data.Builtin.Core.Derived where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Generic.Data (FiniteEnumeration (..))
import Vehicle.Data.AST.Name
import Vehicle.Data.Builtin.Core.BasicOperations
import Vehicle.Prelude.Prettyprinter

data DerivedFunction
  = TypeAnn
  | QuantifyIndex Quantifier
  | QuantifyInList Quantifier
  deriving (Eq, Show, Ord, Generic)
  deriving (Enum, Bounded) via (FiniteEnumeration DerivedFunction)

instance Pretty DerivedFunction where
  pretty = \case
    TypeAnn -> "typeAnn"
    QuantifyIndex q -> pretty q <> "Index"
    QuantifyInList q -> pretty q <> "InList"

instance HasIdentifier DerivedFunction where
  identifierOf f = stdlibIdentifier $ layoutAsText $ pretty f

instance HasName DerivedFunction Name where
  nameOf = nameOf . identifierOf

instance NFData DerivedFunction

instance Hashable DerivedFunction

instance Serialize DerivedFunction
