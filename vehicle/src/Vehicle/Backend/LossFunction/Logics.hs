module Vehicle.Backend.LossFunction.Logics
  ( LExpr (..),
    LDecl (..),
    Domain (..),
    Quantifier (..),
    DifferentialLogicImplementation (..),
    dl2Translation,
    godelTranslation,
    lukasiewiczTranslation,
    productTranslation,
    yagerTranslation,
    stlTranslation,
    implementationOf,
  )
where

import Data.Aeson (FromJSON (..), Options (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Prelude qualified as V

-- Target flag --target json
-- Resolve instance arguments
-- Erase all implicit arguments
-- (Later) Make relevant implicit arguments explicit
-- Move jsonOptions to somewhere, and add ToJSON, FromJSON.

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { tagSingleConstructors = True
    }

-- | Definiton of the LExpr - all expressions allowed in a loss constraint
data LExpr
  = -- | this is minus, not the logical operation of negation
    Negation LExpr
  | Constant Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Addition LExpr LExpr
  | Subtraction LExpr LExpr
  | Multiplication LExpr LExpr
  | Division LExpr LExpr
  | IndicatorFunction LExpr LExpr
  | -- | variable (bound)
    Variable V.Name
  | -- | variable (free)
    FreeVariable V.Name (NonEmpty LExpr)
  | NetworkApplication V.Name (NonEmpty LExpr)
  | -- | quantifiers forall, exists
    Quantifier Quantifier V.Name Domain LExpr
  | At LExpr LExpr
  | TensorLiteral [LExpr]
  | Lambda V.Name LExpr
  | Let V.Name LExpr LExpr
  | Power LExpr LExpr
  | Range LExpr
  | Map LExpr LExpr
  | -- | and for the STL translation specifics of which are handled on the Python side
    ExponentialAnd [LExpr]
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr

instance ToJSON LExpr

--------------------------------------------------------------------------------
-- Declaration definition

data LDecl
  = DefFunction
      V.Name -- Bound function name.
      LExpr -- Bound function body.
  deriving (Eq, Show, Generic)

instance FromJSON LDecl where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LDecl where
  toJSON = genericToJSON jsonOptions

--------------------------------------------------------------------------------
-- other definitions

data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Quantifier

instance ToJSON Quantifier

newtype Domain = Domain ()
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Domain

instance ToJSON Domain

-- | Template for different avilable differentiable logics
--  |part of the syntax translation that differ depending on chosen DL are:
--  |logical connectives (not, and, or, implies)
--  |comparisons (<, <=, >, >=, =, !=)
data DifferentialLogicImplementation = DifferentialLogicImplementation
  { compileAnd :: Either (LExpr -> LExpr -> LExpr) ([LExpr] -> LExpr),
    compileOr :: Either (LExpr -> LExpr -> LExpr) ([LExpr] -> LExpr),
    compileNot :: Maybe (LExpr -> LExpr),
    compileImplies :: LExpr -> LExpr -> LExpr,
    compileLe :: LExpr -> LExpr -> LExpr,
    compileLt :: LExpr -> LExpr -> LExpr,
    compileGe :: LExpr -> LExpr -> LExpr,
    compileGt :: LExpr -> LExpr -> LExpr,
    compileEq :: LExpr -> LExpr -> LExpr,
    compileNeq :: LExpr -> LExpr -> LExpr,
    compileTrue :: Double,
    compileFalse :: Double
  }

implementationOf :: DifferentiableLogic -> DifferentialLogicImplementation
implementationOf = \case
  DL2Loss -> dl2Translation
  GodelLoss -> godelTranslation
  LukasiewiczLoss -> lukasiewiczTranslation
  ProductLoss -> productTranslation
  YagerLoss -> yagerTranslation
  STLLoss -> stlTranslation

--------------------------------------------------------------------------------
-- different available  differentiable logics
-- (avilable options options and how to pass them can be found in Vehicle.Backend.Prelude
-- and the default option if none is provided is DL2)

-- | from Fischer, Marc, et al. "Dl2: Training and querying neural networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicImplementation
dl2Translation =
  DifferentialLogicImplementation
    { compileAnd = Left Addition,
      compileOr = Left Multiplication,
      compileNot = Nothing,
      compileImplies = \arg1 arg2 -> Multiplication (Max (Constant 0) arg1) arg2, -- Max (Negation arg1) arg2,
      compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileLt = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg1 arg2)) (IndicatorFunction arg1 arg2),
      compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileGt = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg2 arg1)) (IndicatorFunction arg2 arg1),
      compileNeq = IndicatorFunction,
      compileEq = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg1 arg2)) (Max (Constant 0) (Subtraction arg1 arg2)),
      compileTrue = 0,
      compileFalse = 1
    }

-- | from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
godelTranslation :: DifferentialLogicImplementation
godelTranslation =
  DifferentialLogicImplementation
    { compileAnd = Left (\arg1 arg2 -> Subtraction (Constant 1) (Min arg1 arg2)),
      compileOr = Left (\arg1 arg2 -> Subtraction (Constant 1) (Max arg1 arg2)),
      compileNot = Just (\arg -> Subtraction (Constant 1) arg),
      compileImplies = \arg1 arg2 -> Subtraction (Constant 1) (Max (Subtraction (Constant 1) arg1) arg2),
      compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileLt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileGt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileEq = \arg1 arg2 -> Subtraction (Constant 1) (IndicatorFunction arg1 arg2),
      compileNeq = IndicatorFunction,
      compileTrue = 0,
      compileFalse = 1
    }

-- | from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
lukasiewiczTranslation :: DifferentialLogicImplementation
lukasiewiczTranslation =
  DifferentialLogicImplementation
    { compileAnd = Left (\arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction (Addition arg1 arg2) (Constant 1)))),
      compileOr = Left (\arg1 arg2 -> Subtraction (Constant 1) (Min (Addition arg1 arg2) (Constant 1))),
      compileNot = Just (\arg -> Subtraction (Constant 1) arg),
      compileImplies = \arg1 arg2 -> Subtraction (Constant 1) (Min (Constant 1) (Addition (Subtraction (Constant 1) arg1) arg2)),
      compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileLt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileGt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileEq = \arg1 arg2 -> Subtraction (Constant 1) (IndicatorFunction arg1 arg2),
      compileNeq = IndicatorFunction,
      compileTrue = 0,
      compileFalse = 1
    }

-- | from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
productTranslation :: DifferentialLogicImplementation
productTranslation =
  DifferentialLogicImplementation
    { compileAnd = Left (\arg1 arg2 -> Subtraction (Constant 1) (Multiplication arg1 arg2)),
      compileOr = Left (\arg1 arg2 -> Subtraction (Constant 1) (Subtraction (Addition arg1 arg2) (Multiplication arg1 arg2))),
      compileNot = Just (\arg -> Subtraction (Constant 1) arg),
      compileImplies = \arg1 arg2 -> Subtraction (Constant 1) (Addition (Subtraction (Constant 1) arg1) (Multiplication arg1 arg2)),
      compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileLt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileGt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileEq = \arg1 arg2 -> Subtraction (Constant 1) (IndicatorFunction arg1 arg2),
      compileNeq = IndicatorFunction,
      compileTrue = 0,
      compileFalse = 1
    }

-- | sets parameter p for the Yager DL (by default set to 1)
yagerTranslation :: DifferentialLogicImplementation
yagerTranslation = parameterisedYagerTranslation 1 -- change constant here

-- | from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
parameterisedYagerTranslation :: Rational -> DifferentialLogicImplementation
parameterisedYagerTranslation p =
  DifferentialLogicImplementation
    { compileAnd =
        Left
          ( \arg1 arg2 ->
              Subtraction
                (Constant 1)
                ( Max
                    ( Subtraction
                        (Constant 1)
                        ( Power
                            ( Addition
                                (Power (Subtraction (Constant 1) arg1) (Constant (fromRational p)))
                                (Power (Subtraction (Constant 1) arg2) (Constant (fromRational p)))
                            )
                            (Division (Constant 1) (Constant (fromRational p)))
                        )
                    )
                    (Constant 0)
                )
          ),
      compileNot = Just (\arg -> Subtraction (Constant 1) arg),
      compileOr =
        Left
          ( \arg1 arg2 ->
              Subtraction
                (Constant 1)
                ( Min
                    ( Power
                        (Addition (Power arg1 (Constant (fromRational p))) (Power arg2 (Constant (fromRational p))))
                        (Division (Constant 1) (Constant (fromRational p)))
                    )
                    (Constant 1)
                )
          ),
      compileImplies = \arg1 arg2 ->
        Subtraction
          (Constant 1)
          ( Min
              ( Power
                  (Addition (Power (Subtraction (Constant 1) arg1) (Constant (fromRational p))) (Power arg2 (Constant (fromRational p))))
                  (Division (Constant 1) (Constant (fromRational p)))
              )
              (Constant 1)
          ),
      compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileLt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2),
      compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileGt = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1),
      compileEq = \arg1 arg2 -> Subtraction (Constant 1) (IndicatorFunction arg1 arg2),
      compileNeq = IndicatorFunction,
      compileTrue = 0,
      compileFalse = 1
    }

-- | from Varnai and Dimarogonas, "On Robustness Metrics for Learning STL Tasks." 2020
stlTranslation :: DifferentialLogicImplementation
stlTranslation =
  DifferentialLogicImplementation
    { compileAnd = Right (\arg -> ExponentialAnd arg),
      compileOr = Right (\arg -> Negation (ExponentialAnd (map Negation arg))),
      compileNot = Just (\arg -> Negation arg),
      compileImplies = \arg1 arg2 -> Negation (ExponentialAnd (map Negation [Negation arg1, arg2])),
      compileLe = \arg1 arg2 -> Subtraction arg2 arg1,
      compileLt = \arg1 arg2 -> Negation (Subtraction arg1 arg2),
      compileGe = \arg1 arg2 -> Subtraction arg1 arg2,
      compileGt = \arg1 arg2 -> Negation (Subtraction arg2 arg1),
      compileEq = IndicatorFunction,
      compileNeq = \arg1 arg2 -> Negation (IndicatorFunction arg1 arg2),
      compileTrue = 1,
      compileFalse = -1
    }
