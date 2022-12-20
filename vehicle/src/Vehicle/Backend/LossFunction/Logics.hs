module Vehicle.Backend.LossFunction.Logics
  ( LExpr (..)
  , Domain (..)
  , Quantifier (..)
  , DifferentialLogicImplementation (..)
  , dl2Translation
  , godelTranslation
  , lukasiewiczTranslation
  , productTranslation
  , yagerTranslation
  , implementationOf
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Vehicle.Backend.Prelude (DifferentiableLogic (..))
import Vehicle.Compile.Prelude qualified as V

-- |Definiton of the LExpr - all expressions allowed in a loss constraint
data LExpr
  -- | this is minus, not the logical operation of negation
  = Negation LExpr
  | Constant Double
  | Min LExpr LExpr
  | Max LExpr LExpr
  | Addition LExpr LExpr
  | Subtraction LExpr LExpr
  | Multiplication LExpr LExpr
  | Division LExpr LExpr
  | IndicatorFunction LExpr LExpr
  -- | variable (bound)
  | Variable Int -- DBIndex
  -- | variable (free)
  | FreeVariable V.Name
  | NetworkApplication V.Name (NonEmpty LExpr)
  -- | quantifiers forall, exists
  | Quantifier Quantifier V.Name Domain LExpr
  | At LExpr LExpr
  | TensorLiteral [LExpr]
  | Lambda V.Name LExpr
  | Let V.Name LExpr LExpr
  | Power LExpr LExpr
  deriving (Eq, Ord, Generic, Show)

instance FromJSON LExpr
instance ToJSON LExpr

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

-- |Template for different avilable differentiable logics
-- |part of the syntax translation that differ depending on chosen DL are:
    -- |logical connectives (not, and, or, implies)
    -- |comparisons (<, <=, >, >=, =, !=)
data DifferentialLogicImplementation = DifferentialLogicImplementation
  { compileAnd     :: LExpr -> LExpr -> LExpr
  , compileOr      :: LExpr -> LExpr -> LExpr
  , compileNot     :: Maybe (LExpr -> LExpr)
  , compileImplies :: LExpr -> LExpr -> LExpr

  , compileLe      :: LExpr -> LExpr -> LExpr
  , compileLt      :: LExpr -> LExpr -> LExpr
  , compileGe      :: LExpr -> LExpr -> LExpr
  , compileGt      :: LExpr -> LExpr -> LExpr
  , compileEq      :: LExpr -> LExpr -> LExpr
  , compileNeq     :: LExpr -> LExpr -> LExpr

  , compileTrue    :: Double
  , compileFalse   :: Double
  }

implementationOf :: DifferentiableLogic -> DifferentialLogicImplementation
implementationOf = \case
  DL2         -> dl2Translation
  Godel       -> godelTranslation
  Lukasiewicz -> lukasiewiczTranslation
  Product     -> productTranslation
  Yager       -> yagerTranslation

--------------------------------------------------------------------------------
-- different available  differentiable logics
-- (avilable options options and how to pass them can be found in Vehicle.Backend.Prelude
-- and the default option if none is provided is DL2)

-- |from Fischer, Marc, et al. "Dl2: Training and querying neural networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicImplementation
dl2Translation = DifferentialLogicImplementation
  { compileAnd = Addition
  , compileOr  = Multiplication
  , compileNot = Nothing
  , compileImplies = \arg1 arg2 -> Max (Negation arg1) arg2

  , compileLe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg1 arg2)
  , compileLt = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg1 arg2))  (IndicatorFunction  arg1 arg2)
  , compileGe = \arg1 arg2 -> Max (Constant 0) (Subtraction arg2 arg1)
  , compileGt = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg2 arg1))  (IndicatorFunction  arg2 arg1)
  , compileNeq = IndicatorFunction
  , compileEq = \arg1 arg2 -> Addition (Max (Constant 0) (Subtraction arg1 arg2)) (Max (Constant 0) (Subtraction arg1 arg2))

  , compileTrue = 0
  , compileFalse = 1
  }

-- |from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
godelTranslation :: DifferentialLogicImplementation
godelTranslation = DifferentialLogicImplementation
  { compileAnd = Min
  , compileOr  = Max
  , compileNot = Just (\arg -> Subtraction (Constant 1) arg)
  , compileImplies = \arg1 arg2 -> Max (Negation arg1) arg2

  , compileLe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2))
  , compileLt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2)))
  , compileGe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1))
  , compileGt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1)))
  , compileNeq = IndicatorFunction
  , compileEq = \arg1 arg2 -> Negation (IndicatorFunction arg1 arg2)

  , compileTrue = 1
  , compileFalse = 0
   }

-- |from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
lukasiewiczTranslation :: DifferentialLogicImplementation
lukasiewiczTranslation = DifferentialLogicImplementation
  { compileAnd = \arg1 arg2 -> Max (Subtraction (Addition arg1 arg2) (Constant 1)) arg2
  , compileOr  = \arg1 arg2 -> Min (Addition arg1 arg2) (Constant 1)
  , compileNot = Just (\arg -> Subtraction (Constant 1) arg)
  , compileImplies = \arg1 arg2 -> Min (Constant 1) (Addition (Subtraction (Constant 1) arg1) arg2)


  , compileLe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2))
  , compileLt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2)))
  , compileGe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1))
  , compileGt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1)))
  , compileNeq = IndicatorFunction
  , compileEq = \arg1 arg2 -> Negation (IndicatorFunction arg1 arg2)

  , compileTrue = 1
  , compileFalse = 0
   }

-- |from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
productTranslation :: DifferentialLogicImplementation
productTranslation = DifferentialLogicImplementation
  { compileAnd = Multiplication
  , compileOr  = \arg1 arg2 -> Subtraction (Addition arg1 arg2) (Multiplication arg1 arg2)
  , compileNot = Just (\arg -> Subtraction (Constant 1) arg)
  , compileImplies = \arg1 arg2 -> Addition (Subtraction (Constant 1) arg1) (Multiplication arg1 arg2)

  , compileLe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2))
  , compileLt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2)))
  , compileGe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1))
  , compileGt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1)))
  , compileNeq = IndicatorFunction
  , compileEq = \arg1 arg2 -> Negation (IndicatorFunction arg1 arg2)

  , compileTrue = 1
  , compileFalse = 0
   }

-- |sets parameter p for the Yager DL (by default set to 1)
yagerTranslation :: DifferentialLogicImplementation
yagerTranslation = parameterisedYagerTranslation 1 --change constant here

-- |from van Krieken, et al. "Analyzing differentiable fuzzy logic operators." 2022
parameterisedYagerTranslation :: Rational -> DifferentialLogicImplementation
parameterisedYagerTranslation p = DifferentialLogicImplementation
  { compileAnd = \arg1 arg2 -> Max
                                  (Subtraction
                                    (Constant 1)
                                      (Power
                                        (Addition
                                          (Power (Subtraction (Constant 1) arg1) (Constant (fromRational p)))
                                          (Power (Subtraction (Constant 1) arg2) (Constant (fromRational p))))
                                        (Division (Constant 1) (Constant (fromRational p)))))
                                    (Constant 0)
  , compileNot = Just (\arg -> Subtraction (Constant 1) arg)
  , compileOr = \arg1 arg2 -> Min
                                  (Power
                                     (Addition (Power arg1 (Constant (fromRational p))) (Power arg2 (Constant (fromRational p))))
                                     (Division (Constant 1) (Constant (fromRational p)))
                                    )
                                    (Constant 1)
  , compileImplies = \arg1 arg2 -> Min
                                  (Power
                                     (Addition (Power (Subtraction (Constant 1) arg1) (Constant (fromRational p))) (Power arg2 (Constant (fromRational p))))
                                     (Division (Constant 1) (Constant (fromRational p)))
                                    )
                                    (Constant 1)


  , compileLe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2))
  , compileLt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg1 arg2)))
  , compileGe = \arg1 arg2 -> Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1))
  , compileGt = \arg1 arg2 -> Negation (Subtraction (Constant 1) (Max (Constant 0) (Subtraction arg2 arg1)))
  , compileNeq = IndicatorFunction
  , compileEq = \arg1 arg2 -> Negation (IndicatorFunction arg1 arg2)

  , compileTrue = 1
  , compileFalse = 0
   }
