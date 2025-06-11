-- | Builtins for deciding whether or not a given expression uses alternating quantifiers
-- or not during compilation to verifier queries.
module Vehicle.Data.Builtin.Polarity where

import Control.DeepSeq (NFData (..))
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Blocked (BlockingStatus (DoesNotReduce), functionBlockingStatus)
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Interface.Print
import Vehicle.Data.DSL
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- PolarityProvenance

-- | Used to track where the polarity information came from.
data PolarityProvenance
  = QuantifierProvenance Provenance
  | NegateProvenance Provenance PolarityProvenance
  | LHSImpliesProvenance Provenance PolarityProvenance
  | PolFunctionProvenance Provenance PolarityProvenance FunctionPosition
  deriving (Generic)

instance Serialize PolarityProvenance

instance Show PolarityProvenance where
  show _x = ""

instance Eq PolarityProvenance where
  _x == _y = True

instance NFData PolarityProvenance where
  rnf _x = ()

instance Hashable PolarityProvenance where
  hashWithSalt s _p = s

--------------------------------------------------------------------------------
-- Polarity

-- | Used to annotate boolean types, represents what sort of pattern of
-- quantifiers it contains.
data Polarity
  = Unquantified
  | Quantified Quantifier PolarityProvenance
  | -- | Stores the provenance of the `Forall` first followed by the `Exists`.
    MixedParallel PolarityProvenance PolarityProvenance
  | -- | Stores the type and provenance of the top-most quantifier first.
    MixedSequential Quantifier Provenance PolarityProvenance
  deriving (Eq, Generic, Show)

instance NFData Polarity

instance Hashable Polarity

instance Serialize Polarity

instance Pretty Polarity where
  pretty = \case
    Unquantified -> "Unquantified"
    Quantified q _ -> "Quantified" <+> pretty q
    MixedParallel {} -> "MixedParallel"
    MixedSequential {} -> "MixedSequential"

mapPolarityProvenance :: (PolarityProvenance -> PolarityProvenance) -> Polarity -> Polarity
mapPolarityProvenance f = \case
  Unquantified -> Unquantified
  Quantified q pp -> Quantified q (f pp)
  MixedParallel pp1 pp2 -> MixedParallel (f pp1) (f pp2)
  -- At the moment we don't change non-Polar provenance because we
  -- want the minimal example.
  MixedSequential q p pp -> MixedSequential q p pp

--------------------------------------------------------------------------------
-- Polarity relationship

data PolarityRelation
  = NegPolarity
  | QuantifierPolarity Provenance Quantifier
  | AddPolarity Provenance Quantifier
  | ImpliesPolarity
  | IfPolarity
  | MaxPolarity
  | FunctionPolarity FunctionPosition
  deriving (Eq, Generic, Show)

instance Serialize PolarityRelation

instance NFData PolarityRelation

instance Hashable PolarityRelation

instance Pretty PolarityRelation where
  pretty = \case
    NegPolarity -> "NegPolarity"
    AddPolarity _ q -> "AddPolarity" <+> pretty q
    QuantifierPolarity _ q -> "QuantifierPolarity" <+> pretty q
    ImpliesPolarity -> "ImpliesPolarity"
    MaxPolarity -> "MaxPolarity"
    IfPolarity -> "IfPolarity"
    FunctionPolarity p -> "FunctionPolarity" <> pretty p

--------------------------------------------------------------------------------
-- Builtin type

data PolarityBuiltin
  = PolarityConstructor BuiltinConstructor
  | PolarityFunction BuiltinFunction
  | Polarity Polarity
  | PolarityRelation PolarityRelation
  deriving (Show, Eq, Generic)

instance Hashable PolarityBuiltin

instance Pretty PolarityBuiltin where
  pretty = \case
    PolarityConstructor c -> pretty c
    PolarityFunction f -> pretty f
    Polarity l -> pretty l
    PolarityRelation c -> pretty c

functionAccessor :: BuiltinFunction -> Accessor PolarityBuiltin ()
functionAccessor b =
  Access
    { getExpr = \case
        PolarityFunction b1 | b == b1 -> Just ()
        _ -> Nothing,
      mkExpr = \() -> PolarityFunction b
    }

instance BuiltinHasStandardData PolarityBuiltin where
  accessBuiltinFunction =
    Access
      { mkExpr = PolarityFunction,
        getExpr = \case
          PolarityFunction c -> Just c
          _ -> Nothing
      }

  accessBuiltinConstructor =
    Access
      { mkExpr = PolarityConstructor,
        getExpr = \case
          PolarityConstructor c -> Just c
          _ -> Nothing
      }

instance BuiltinHasNatLiterals PolarityBuiltin where
  accessNatLitBuiltin =
    Access
      { getExpr = \case
          PolarityConstructor (NatLiteral n) -> Just n
          _ -> Nothing,
        mkExpr = PolarityConstructor . NatLiteral
      }

  accessNatTensorLitBuiltin =
    Access
      { getExpr = \case
          PolarityConstructor (NatTensorLiteral b) -> Just b
          _ -> Nothing,
        mkExpr = PolarityConstructor . NatTensorLiteral
      }

  accessAddNatBuiltin = functionAccessor (Add AddNat)
  accessMulNatBuiltin = functionAccessor (Mul MulNat)

instance BuiltinHasListLiterals PolarityBuiltin where
  accessNilBuiltin =
    Access
      { getExpr = \case
          PolarityConstructor Nil -> Just ()
          _ -> Nothing,
        mkExpr = \() -> PolarityConstructor Nil
      }

  accessConsBuiltin =
    Access
      { getExpr = \case
          PolarityConstructor Cons -> Just ()
          _ -> Nothing,
        mkExpr = \() -> PolarityConstructor Cons
      }

  accessMapListBuiltin = functionAccessor MapList
  accessFoldListBuiltin = functionAccessor FoldList

instance BuiltinHasIterate PolarityBuiltin where
  accessIterateBuiltin = functionAccessor Iterate

-----------------------------------------------------------------------------
-- Printing

instance ConvertableBuiltin PolarityBuiltin Builtin where
  convertBuiltin p = \case
    PolarityConstructor c -> convertBuiltin p c
    PolarityFunction f -> convertBuiltin p f
    b -> cheatConvertBuiltin p $ pretty b

instance PrintableBuiltin PolarityBuiltin where
  coercionArgs _ = Nothing
  isDerivedBuiltin = const Nothing

-----------------------------------------------------------------------------
-- Normalisation

instance NormalisableBuiltin PolarityBuiltin where
  evalScheme b = case b of
    PolarityFunction Iterate -> NonSimple evalIterate
    PolarityFunction _ -> None
    _ -> None

  blockingStatus b spine = case b of
    PolarityFunction f -> functionBlockingStatus f spine
    _ -> DoesNotReduce

  isTypeClassOp _ = False
  isCast _ _ = Nothing

-----------------------------------------------------------------------------
-- DSL

type PolarityDSLExpr = DSLExpr PolarityBuiltin

forAllPolarities :: (PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarities f = forAll "p" tPol $ \p -> f p

forAllPolarityPairs :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityPairs f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      f p1 p2

forAllPolarityTriples :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityTriples f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      forAll "p3" tPol $ \p3 ->
        f p1 p2 p3

unquantified :: PolarityDSLExpr
unquantified = builtin (Polarity Unquantified)

polarityTypeClass :: PolarityRelation -> NonEmpty PolarityDSLExpr -> PolarityDSLExpr
polarityTypeClass tc args = builtin (PolarityRelation tc) @@ args

quantifierPolarity :: Provenance -> Quantifier -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
quantifierPolarity p q l1 l2 = polarityTypeClass (QuantifierPolarity p q) [l1, l2]

maxPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
maxPolarity l1 l2 l3 = polarityTypeClass MaxPolarity [l1, l2, l3]

ifPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
ifPolarity l1 l2 l3 l4 = polarityTypeClass IfPolarity [l1, l2, l3, l4]

impliesPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
impliesPolarity l1 l2 l3 = polarityTypeClass ImpliesPolarity [l1, l2, l3]

negPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
negPolarity l1 l2 = polarityTypeClass NegPolarity [l1, l2]

tPol :: PolarityDSLExpr
tPol = type0
