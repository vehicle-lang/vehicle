module Vehicle.Data.Real where

import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON (..), genericToJSON)
import Data.Hashable
import Data.Serialize
import GHC.Generics
import Vehicle.Prelude.Misc (jsonOptions)
import Vehicle.Prelude.Prettyprinter

data ExtendedRational
  = NegInfinity
  | Finite Rational
  | PosInfinity
  deriving (Show, Eq, Ord, Generic)

instance NFData ExtendedRational

instance Hashable ExtendedRational

instance Serialize ExtendedRational

instance ToJSON ExtendedRational where
  toJSON = genericToJSON jsonOptions

instance FromJSON ExtendedRational

instance Pretty ExtendedRational where
  pretty = \case
    NegInfinity -> "-infinity"
    Finite r -> pretty r
    PosInfinity -> "infinity"

instance Num ExtendedRational where
  -- Addition
  NegInfinity + NegInfinity = error "Infinity + (-Infinity) is undefined"
  NegInfinity + PosInfinity = error "(-Infinity) + Infinity is undefined"
  PosInfinity + _ = PosInfinity
  _ + PosInfinity = PosInfinity
  NegInfinity + _ = NegInfinity
  _ + NegInfinity = NegInfinity
  Finite r1 + Finite r2 = Finite (r1 + r2)

  -- Multiplication
  PosInfinity * PosInfinity = PosInfinity
  NegInfinity * NegInfinity = PosInfinity
  PosInfinity * NegInfinity = NegInfinity
  NegInfinity * PosInfinity = NegInfinity
  PosInfinity * Finite r = case signum r of
    1 -> PosInfinity
    -1 -> NegInfinity
    _ -> error "Infinity * 0 is undefined"
  NegInfinity * Finite r = case signum r of
    1 -> NegInfinity
    -1 -> PosInfinity
    _ -> error "(-Infinity) * 0 is undefined"
  Finite r * PosInfinity = Finite r * PosInfinity
  Finite r * NegInfinity = Finite r * NegInfinity
  Finite r1 * Finite r2 = Finite (r1 * r2)

  -- Negation
  negate PosInfinity = NegInfinity
  negate NegInfinity = PosInfinity
  negate (Finite r) = Finite (negate r)

  -- Abs
  abs PosInfinity = PosInfinity
  abs NegInfinity = PosInfinity
  abs (Finite r) = Finite (abs r)

  -- Sign
  signum PosInfinity = 1
  signum NegInfinity = -1
  signum (Finite r) = Finite (signum r)

  -- From integer
  fromInteger n = Finite (fromInteger n)

instance Fractional ExtendedRational where
  -- Division
  PosInfinity / PosInfinity = error "Infinity / Infinity is undefined (NaN disabled)"
  NegInfinity / NegInfinity = error "(-Infinity) / (-Infinity) is undefined (NaN disabled)"
  PosInfinity / NegInfinity = error "Infinity / (-Infinity) is undefined (NaN disabled)"
  NegInfinity / PosInfinity = error "(-Infinity) / Infinity is undefined (NaN disabled)"
  Finite _ / PosInfinity = 0
  Finite _ / NegInfinity = 0
  PosInfinity / Finite r = case signum r of
    1 -> PosInfinity
    -1 -> NegInfinity
    _ -> error "Infinity / 0 is undefined"
  NegInfinity / Finite r = case signum r of
    1 -> NegInfinity
    -1 -> PosInfinity
    _ -> error "(-Infinity) / 0 is undefined"
  Finite r1 / Finite r2 =
    if r2 == 0
      then case signum r1 of
        1 -> PosInfinity
        -1 -> NegInfinity
        _ -> error "0 / 0 is undefined (NaN disabled)"
      else Finite (r1 / r2)

  -- Reciprocal (1 / x)
  recip PosInfinity = 0
  recip NegInfinity = 0
  recip (Finite r) =
    if r == 0
      then error "Division by zero: reciprocal of 0 is undefined"
      else Finite (recip r)

  fromRational = Finite
