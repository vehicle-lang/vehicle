module Vehicle.Language.AST.Datatype where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

import Vehicle.Prelude
import Vehicle.Language.AST.Builtin.TypeClass
import Vehicle.Language.AST.Builtin.Polarity
import Vehicle.Language.AST.Builtin.Linearity

data Constructor
  -- TODO Move True/False

  -- Constructors for List
  = Nil
  | Cons

  -- Constructors for Type
  | Unit
  | Bool
  | Index
  | Nat
  | Int
  | Rat
  | List
  | Vector
  | TypeClass TypeClass

  -- Annotations - these should not be shown to the user.
  | Polarity  Polarity
  | Linearity Linearity
  deriving (Eq, Show, Generic)

instance NFData Constructor
instance Hashable Constructor

instance Pretty Constructor where
  pretty = \case
    Nil    -> "nil"
    Cons   -> "::"

    Unit   -> "Unit"
    Bool   -> "Bool"
    Index  -> "Index"
    Nat    -> "Nat"
    Int    -> "Int"
    Rat    -> "Rat"
    List   -> "List"
    Vector -> "Vector"

    Polarity  pol -> pretty pol
    Linearity lin -> pretty lin

    TypeClass   tc   -> pretty tc