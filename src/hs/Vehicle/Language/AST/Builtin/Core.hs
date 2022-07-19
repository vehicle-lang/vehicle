-- | This module exports the datatype representations of the core builtin symbols.

module Vehicle.Language.AST.Builtin.Core
  ( Quantifier(..)
  , Order(..)
  , Equality(..)
  , isStrict
  , flipStrictness
  , flipOrder
  , chainable
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Hashable (Hashable (..))

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Equality

data Equality
  = Eq
  | Neq
  deriving (Eq, Ord, Generic)

instance NFData   Equality
instance Hashable Equality

instance Show Equality where
  show = \case
    Eq  -> "=="
    Neq -> "!="

instance Pretty Equality where
  pretty = pretty . show

instance Negatable Equality where
  neg Eq = Neq
  neg Neq = Eq

--------------------------------------------------------------------------------
-- Orders

data Order
  = Le
  | Lt
  | Ge
  | Gt
  deriving (Eq, Ord, Generic)

instance NFData   Order
instance Hashable Order

instance Show Order where
  show = \case
    Le -> "<="
    Lt -> "<"
    Ge -> ">="
    Gt -> ">"

instance Pretty Order where
  pretty = pretty . show

instance Negatable Order where
  neg = \case
    Le -> Gt
    Lt -> Ge
    Ge -> Lt
    Gt -> Le

isStrict :: Order -> Bool
isStrict order = order == Lt || order == Gt

flipStrictness :: Order -> Order
flipStrictness = \case
  Le -> Lt
  Lt -> Le
  Ge -> Gt
  Gt -> Ge

flipOrder :: Order -> Order
flipOrder = \case
  Le -> Ge
  Lt -> Gt
  Ge -> Le
  Gt -> Lt

chainable :: Order -> Order -> Bool
chainable e1 e2 = e1 == e2 || e1 == flipStrictness e2

--------------------------------------------------------------------------------
-- Quantifiers

data Quantifier
  = Forall
  | Exists
  deriving (Show, Eq, Ord, Generic)

instance NFData   Quantifier
instance Hashable Quantifier

instance Negatable Quantifier where
  neg Forall = Exists
  neg Exists = Forall

instance Pretty Quantifier where
  pretty = pretty . show