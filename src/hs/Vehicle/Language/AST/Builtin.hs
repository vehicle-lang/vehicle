-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( Builtin(..)
  , Quantifier(..)
  , Order(..)
  , builtinFromSymbol
  , symbolFromBuiltin
  ) where


import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Standard builtins

-- |Builtins to the Vehicle language
data Builtin
  -- Types
  = Bool
  | Prop
  | Nat
  | Int
  | Real
  | List
  | Tensor
  -- Type classes
  | HasEq
  | HasOrd
  | IsTruth
  | IsNatural
  | IsIntegral
  | IsRational
  | IsReal
  | IsContainer
  | IsQuantifiable
  -- Expressions
  | If
  | Impl
  | And
  | Or
  | Not
  | Eq
  | Neq
  | Order Order
  | Mul
  | Div
  | Add
  | Sub
  | Neg
  | Cons
  | At
  | Map
  | Fold
  | Quant Quantifier
  | QuantIn Quantifier
  deriving (Eq, Ord, Show, Generic)

instance NFData Builtin

instance Pretty Builtin where
  pretty b = pretty $ symbolFromBuiltin b

-- | Property-level quantifiers supported
data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Show, Generic)

instance NFData Quantifier

instance Pretty Quantifier where
  pretty = \case
    All -> "every"
    Any -> "some"

-- | Numeric orders supported
data Order
  = Le
  | Lt
  | Ge
  | Gt
  deriving (Eq, Ord, Generic)

instance NFData Order

instance Show Order where
  show = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"

instance Pretty Order where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Conversion to symbols

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols =
  -- Types
  [ "Bool"         |-> Bool
  , "Prop"         |-> Prop
  , "Nat"          |-> Nat
  , "Int"          |-> Int
  , "Real"         |-> Real
  , "List"         |-> List
  , "Tensor"       |-> Tensor
  -- Type classes
  , "HasEq"        |-> HasEq
  , "HasOrd"       |-> HasOrd
  , "IsTruth"      |-> IsTruth
  , "IsContainer"  |-> IsContainer
  , "IsNatural"    |-> IsNatural
  , "IsIntegral"   |-> IsIntegral
  , "IsRational"   |-> IsRational
  , "IsReal"       |-> IsReal
  , "IsQuantify"   |-> IsQuantifiable
  -- Operations
  , "if"           |-> If
  , "implies"      |-> Impl
  , "and"          |-> And
  , "or"           |-> Or
  , "not"          |-> Not
  , "=="           |-> Eq
  , "!="           |-> Neq
  , "<="           |-> Order Le
  , "<"            |-> Order Lt
  , ">="           |-> Order Ge
  , ">"            |-> Order Gt
  , "+"            |-> Add
  , "*"            |-> Mul
  , "/"            |-> Div
  , "-"            |-> Sub
  , "~"            |-> Neg -- Negation is changed from "-" to "~" during elaboration.
  , "!"            |-> At
  , "::"           |-> Cons
  , "every"        |-> Quant All
  , "some"         |-> Quant Any
  , "everyIn"      |-> QuantIn All
  , "someIn"       |-> QuantIn Any
  , "map"          |-> Map
  , "fold"         |-> Fold
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Symbol
symbolFromBuiltin builtin =
  case lookup' builtin builtinSymbols of
    Nothing -> developerError $ "Missing symbol for builtin" <+> pretty (show builtin)
    Just s  -> s

lookup' :: (Eq b) => b -> [(a,b)] -> Maybe a
lookup' _ [] =  Nothing
lookup' key ((x,y):xys)
  | key == y  =  Just x
  | otherwise =  lookup' key xys