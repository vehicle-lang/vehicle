-- | This module exports the datatype representations of the builtin symbols.

module Vehicle.Language.AST.Builtin
  ( Builtin(..)
  , NumericType(..)
  , ContainerType(..)
  , Quantifier(..)
  , Order(..)
  , Equality(..)
  , TypeClass(..)
  , BooleanOp2(..)
  , NumericOp2(..)
  , Polarity(..)
  , PolarityProvenance(..)
  , PolarityTypeClass(..)
  , builtinFromSymbol
  , symbolFromBuiltin
  , isStrict
  , flipStrictness
  , flipOrder
  , chainable
  , prettyQuantifierArticle
  ) where

import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData(..))
import Data.Text (pack)
import Data.Hashable (Hashable (..))

import Vehicle.Prelude
import Vehicle.Language.AST.Provenance

-- TODO all the show instances should really be obtainable from the grammar
-- somehow.

--------------------------------------------------------------------------------
-- Numeric types

data NumericType
  = Nat
  | Int
  | Rat
  deriving (Eq, Ord, Show, Generic)

instance NFData   NumericType
instance Hashable NumericType

instance Pretty NumericType where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Container types

data ContainerType
  = List
  | Tensor
  deriving (Eq, Ord, Show, Generic)

instance NFData   ContainerType
instance Hashable ContainerType

instance Pretty ContainerType where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Type classes

data TypeClass
  = HasEq
  | HasOrd

  -- Operation type-classes
  | HasAdd
  | HasSub
  | HasMul
  | HasDiv
  | HasNeg
  | HasConOps

  -- Literal type-classes
  | HasNatLitsUpTo Int
  -- ^ The parameter is the maximum value (needed for Index).
  | HasIntLits
  | HasRatLits
  | HasConLitsOfSize Int
  -- ^ The parameter is the size of the container (needed for Tensor)
  deriving (Eq, Ord, Generic)

instance NFData   TypeClass
instance Hashable TypeClass

instance Show TypeClass where
  show = \case
    HasEq              -> "HasEq"
    HasOrd             -> "HasOrd"
    HasAdd             -> "HasAdd"
    HasSub             -> "HasSub"
    HasMul             -> "HasMul"
    HasDiv             -> "HasDiv"
    HasNeg             -> "HasNeg"
    HasConOps          -> "HasConOperations"
    HasNatLitsUpTo n   -> "HasNatLiteralsUpTo " <> show n
    HasIntLits         -> "HasIntLiterals"
    HasRatLits         -> "HasRatLiterals"
    HasConLitsOfSize n -> "HasConLiteralsOfSize " <> show n

instance Pretty TypeClass where
  pretty = pretty . show

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

prettyQuantifierArticle :: Quantifier -> Doc a
prettyQuantifierArticle q =
  (if q == Forall then "a" else "an") <+> squotes (pretty q)

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
-- Boolean operations

data BooleanOp2
  = Impl
  | And
  | Or
  deriving (Eq, Ord, Generic)

instance NFData   BooleanOp2
instance Hashable BooleanOp2

instance Show BooleanOp2 where
  show = \case
    Impl -> "implies"
    And  -> "and"
    Or   -> "or"

instance Pretty BooleanOp2 where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Numeric operations

data NumericOp2
  = Mul
  | Div
  | Add
  | Sub
  deriving (Eq, Ord, Generic)

instance NFData   NumericOp2
instance Hashable NumericOp2

instance Show NumericOp2 where
  show = \case
    Add -> "+"
    Mul -> "*"
    Div -> "/"
    Sub -> "-"

instance Pretty NumericOp2 where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Linearity

{-
-- | Used to annotate numeric types, representing whether it represents a
-- constant, linear or non-linear expression.
data Linearity
  = Constant
  | Linear
  | NonLinear
  deriving (Eq, Ord, Show, Generic)

instance NFData   Linearity
instance Hashable Linearity
-}

--------------------------------------------------------------------------------
-- Polarity

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | NegationProvenance Provenance PolarityProvenance
  deriving (Generic)

instance Show PolarityProvenance where
  show _x = ""

instance Eq PolarityProvenance where
  _x == _y = True

instance NFData PolarityProvenance where
  rnf _x = ()

instance Hashable PolarityProvenance where
  hashWithSalt s _p = s

-- | Used to annotate boolean types, represents what sort of pattern of
-- quantifiers it contains.
--
data Polarity
  = Unquantified
  | Quantified Quantifier PolarityProvenance
  -- | Stores the provenance of the `Forall` first followed by the `Exists`.
  | MixedParallel PolarityProvenance PolarityProvenance
  -- | Stores the type and provenance of the top-most quantifier first.
  | MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Show, Generic)

instance NFData Polarity

instance Hashable Polarity
{-
instance Negatable Polarity where
  neg = \case
    Unquantified    -> Unquantified
    Quantified q    -> Quantified $ neg q
    MixedParallel   -> MixedParallel
    MixedSequential -> MixedSequential
-}
data PolarityTypeClass
  = HasNot
  | HasAndOr
  | HasImpl
  | HasQuantifier Quantifier
  deriving (Eq, Generic)

instance NFData   PolarityTypeClass
instance Hashable PolarityTypeClass

instance Show PolarityTypeClass where
  show = \case
    HasNot             -> "HasNot"
    HasAndOr           -> "HasAndOr"
    HasImpl            -> "HasImpl"
    HasQuantifier q    -> "HasQuantifier " <> show q

--------------------------------------------------------------------------------
-- Builtin

-- |Builtins in the Vehicle language
data Builtin
  -- Types
  = Bool
  | NumericType   NumericType
  | ContainerType ContainerType
  | Index
  -- Type classes
  | TypeClass TypeClass
  -- Expressions
  | If
  | Not
  | BooleanOp2 BooleanOp2
  | Neg
  | NumericOp2 NumericOp2
  | Cons
  | At
  | Map
  | Fold
  | Equality  Equality
  | Order     Order
  | Quant     Quantifier
  | QuantIn   Quantifier
  | Foreach
  | ForeachIn

  -- Annotations - these should not be shown to the user.
  | AuxiliaryType
  -- ^ The type of Polarity/Linearity etc.
  | Polarity Polarity
  | PolarityTypeClass PolarityTypeClass
  deriving (Eq, Generic)

instance NFData   Builtin
instance Hashable Builtin

instance Pretty Builtin where
  pretty = pretty . show

--------------------------------------------------------------------------------
-- Conversion to symbols

instance Show Builtin where
  show = \case
    Bool                -> "Bool"
    NumericType   t     -> show t
    ContainerType t     -> show t
    Index               -> "Index"
    BooleanOp2 op       -> show op
    Not                 -> "not"
    NumericOp2 op       -> show op
    Neg                 -> "~"
    If                  -> "if"
    At                  -> "!"
    Cons                -> "::"
    Equality e          -> show e
    Order o             -> show o
    Map                 -> "map"
    Fold                -> "fold"
    Quant   Forall      -> "forall"
    Quant   Exists      -> "exists"
    QuantIn q           -> show (Quant q) <> "In"
    Foreach             -> "foreach"
    ForeachIn           -> "foreachIn"
    TypeClass tc        -> show tc
    AuxiliaryType       -> "AuxiliaryType"
    Polarity pol        -> show pol
    PolarityTypeClass t -> show t

builtinSymbols :: [(Symbol, Builtin)]
builtinSymbols = map (first pack)
  [ show Bool                         |-> Bool
  , show (NumericType Nat)            |-> NumericType Nat
  , show (NumericType Int)            |-> NumericType Int
  , show (NumericType Rat)            |-> NumericType Rat
  , show (ContainerType List)         |-> ContainerType List
  , show (ContainerType Tensor)       |-> ContainerType Tensor
  , show If                           |-> If
  , show (BooleanOp2 Impl)            |-> BooleanOp2 Impl
  , show (BooleanOp2 And)             |-> BooleanOp2 And
  , show (BooleanOp2 Or)              |-> BooleanOp2 Or
  , show Not                          |-> Not
  , show (Equality Eq)                |-> Equality Eq
  , show (Equality Neq)               |-> Equality Neq
  , show (Order Le)                   |-> Order Le
  , show (Order Lt)                   |-> Order Lt
  , show (Order Ge)                   |-> Order Ge
  , show (Order Gt)                   |-> Order Gt
  , show (NumericOp2 Add)             |-> NumericOp2 Add
  , show (NumericOp2 Mul)             |-> NumericOp2 Mul
  , show (NumericOp2 Div)             |-> NumericOp2 Div
  , show (NumericOp2 Sub)             |-> NumericOp2 Sub
  , show Neg                          |-> Neg
  , show At                           |-> At
  , show Cons                         |-> Cons
  , show (Quant Forall)               |-> Quant Forall
  , show (Quant Exists)               |-> Quant Exists
  , show (QuantIn Forall)             |-> QuantIn Forall
  , show (QuantIn Exists)             |-> QuantIn Exists
  , show Foreach                      |-> Foreach
  , show ForeachIn                    |-> ForeachIn
  , show Map                          |-> Map
  , show Fold                         |-> Fold
  , show AuxiliaryType                |-> AuxiliaryType
  ]

builtinFromSymbol :: Symbol -> Maybe Builtin
builtinFromSymbol symbol = lookup symbol builtinSymbols

symbolFromBuiltin :: Builtin -> Symbol
symbolFromBuiltin builtin = pack $ show builtin
