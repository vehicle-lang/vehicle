module Vehicle.Backend.LossFunction.Logics
  ( LExpr (..),
    pattern Negation,
    pattern Min,
    pattern Max,
    pattern Addition,
    pattern Subtraction,
    pattern Multiplication,
    pattern Division,
    pattern IndicatorFunction,
    pattern At,
    pattern Power,
    pattern Map,
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

import Data.Aeson (FromJSON (..), Options (..), SumEncoding (..), ToJSON (..), Value, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Prelude qualified as V

jsonOptions :: String -> String -> Options
jsonOptions fieldLabelPrefix constructorTagPrefix =
  defaultOptions
    { fieldLabelModifier = modifier fieldLabelPrefix,
      constructorTagModifier = modifier constructorTagPrefix,
      allNullaryToStringTag = True,
      sumEncoding = UntaggedValue
    }
  where
    modifier prefix name = camelTo2 '_' (fromMaybe name (stripPrefix prefix name))

data UnaryOperator
  = OpNegation
  deriving (Eq, Ord, Generic, Show)

data BinaryOperator
  = OpMin
  | OpMax
  | OpAddition
  | OpSubtraction
  | OpMultiplication
  | OpDivision
  | OpIndicatorFunction
  | OpAt
  | OpPower
  | OpMap
  deriving (Eq, Ord, Generic, Show)

instance FromJSON UnaryOperator where
  parseJSON :: Value -> Parser UnaryOperator
  parseJSON = genericParseJSON (jsonOptions "" "Op")

instance ToJSON UnaryOperator where
  toJSON :: UnaryOperator -> Value
  toJSON = genericToJSON (jsonOptions "" "Op")

instance FromJSON BinaryOperator where
  parseJSON :: Value -> Parser BinaryOperator
  parseJSON = genericParseJSON (jsonOptions "" "Op")

instance ToJSON BinaryOperator where
  toJSON :: BinaryOperator -> Value
  toJSON = genericToJSON (jsonOptions "" "Op")

pattern Negation :: LExpr -> LExpr
pattern Negation e1 = UnaryOperator OpNegation e1

pattern Min :: LExpr -> LExpr -> LExpr
pattern Min e1 e2 = BinaryOperator OpMin e1 e2

pattern Max :: LExpr -> LExpr -> LExpr
pattern Max e1 e2 = BinaryOperator OpMax e1 e2

pattern Addition :: LExpr -> LExpr -> LExpr
pattern Addition e1 e2 = BinaryOperator OpAddition e1 e2

pattern Subtraction :: LExpr -> LExpr -> LExpr
pattern Subtraction e1 e2 = BinaryOperator OpSubtraction e1 e2

pattern Multiplication :: LExpr -> LExpr -> LExpr
pattern Multiplication e1 e2 = BinaryOperator OpMultiplication e1 e2

pattern Division :: LExpr -> LExpr -> LExpr
pattern Division e1 e2 = BinaryOperator OpDivision e1 e2

pattern IndicatorFunction :: LExpr -> LExpr -> LExpr
pattern IndicatorFunction e1 e2 = BinaryOperator OpIndicatorFunction e1 e2

pattern At :: LExpr -> LExpr -> LExpr
pattern At e1 e2 = BinaryOperator OpAt e1 e2

pattern Power :: LExpr -> LExpr -> LExpr
pattern Power e1 e2 = BinaryOperator OpPower e1 e2

pattern Map :: LExpr -> LExpr -> LExpr
pattern Map e1 e2 = BinaryOperator OpMap e1 e2

-- | Definiton of the LExpr - all expressions allowed in a loss constraint
data LExpr
  = -- | this is minus, not the logical operation of negation
    UnaryOperator
      { _unaryOperatorName :: UnaryOperator,
        _unaryOperatorArg :: LExpr
      }
  | BinaryOperator
      { _binaryOperatorName :: BinaryOperator,
        _binaryOperatorArg1 :: LExpr,
        _binaryOperatorArg2 :: LExpr
      }
  | Constant
      { _constantValue :: Double
      }
  | -- | variable (bound)
    Variable
      { _variableName :: V.Name
      }
  | -- | variable (free)
    FreeVariable
      { _functionName :: V.Name,
        _functionArgs :: NonEmpty LExpr
      }
  | NetworkApplication
      { _networkName :: V.Name,
        _networkArgs :: NonEmpty LExpr
      }
  | -- | quantifiers forall, exists
    Quantifier
      { _quantifier :: Quantifier,
        _quantifierBoundName :: V.Name,
        _quantifierDomain :: Domain,
        _quantifierBody :: LExpr
      }
  | TensorLiteral
      { _sequence :: [LExpr]
      }
  | Let
      { _letBoundName :: V.Name,
        _letBoundExpr :: LExpr,
        _letBody :: LExpr
      }
  | Lambda
      { _lambdaBoundName :: V.Name,
        _lambdaBody :: LExpr
      }
  | Range
      { _rangeExpr :: LExpr
      }
  | -- | and for the STL translation specifics of which are handled on the Python side
    ExponentialAnd
      { _exponentialAndExpr :: [LExpr]
      }
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr where
  parseJSON :: Value -> Parser LExpr
  parseJSON = genericParseJSON (jsonOptions "_" "")

instance ToJSON LExpr where
  toJSON :: LExpr -> Value
  toJSON = genericToJSON (jsonOptions "_" "")

--------------------------------------------------------------------------------
-- Declaration definition

data LDecl = DefFunction
  { -- | Bound function name.
    _declarationName :: V.Name,
    -- | Bound function body.
    _declarationBody :: LExpr
  }
  deriving (Eq, Show, Generic)

instance FromJSON LDecl where
  parseJSON :: Value -> Parser LDecl
  parseJSON = genericParseJSON (jsonOptions "_" "")

instance ToJSON LDecl where
  toJSON :: LDecl -> Value
  toJSON = genericToJSON (jsonOptions "_" "")

--------------------------------------------------------------------------------
-- other definitions

data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

instance FromJSON Quantifier where
  parseJSON :: Value -> Parser Quantifier
  parseJSON = genericParseJSON (jsonOptions "" "")

instance ToJSON Quantifier where
  toJSON :: Quantifier -> Value
  toJSON = genericToJSON (jsonOptions "" "")

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
  DL2 -> dl2Translation
  Godel -> godelTranslation
  Lukasiewicz -> lukasiewiczTranslation
  Product -> productTranslation
  Yager -> yagerTranslation
  STL -> stlTranslation

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
