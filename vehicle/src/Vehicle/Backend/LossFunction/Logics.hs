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
  ) where

import Vehicle.Prelude
import Vehicle.Compile.Prelude qualified as V
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

-- definiton of the LExpr - all expressions allowed in a loss constraint

data LExpr
  = Negation LExpr                           -- this is minus, not the logical operation of negation
  | Constant Double                           -- constant
  | Min LExpr LExpr                           -- Min
  | Max LExpr LExpr                           -- Max
  | Addition LExpr LExpr                      -- addition
  | Subtraction LExpr LExpr                   -- subtraction
  | Multiplication LExpr LExpr                -- multiplication
  | Division LExpr LExpr                      -- division
  | IndicatorFunction LExpr LExpr             -- an indicator function
  | Variable V.DBIndex                        -- variable (bound)
  | FreeVariable Name                         -- variable (free)
  | NetworkApplication Name (NonEmpty LExpr)  -- neural network
  | Quantifier Quantifier Name Domain LExpr   -- quantifiers forall, exists
  | At LExpr LExpr                            -- at
  | TensorLiteral [LExpr]                     -- tensor
  | Lambda Name LExpr                         -- lambda expression
  | Let Name LExpr LExpr                      -- let expression
  | Power LExpr LExpr                         -- exponential
  deriving (Eq, Ord, Generic, Show)

--------------------------------------------------------------------------------
-- other Definitions

data Quantifier
  = All
  | Any
  deriving (Eq, Ord, Generic, Show)

newtype Domain = Domain ()
  deriving (Eq, Ord, Generic, Show)

data DifferentialLogicImplementation = DifferentialLogicImplementation
  { compileAnd          :: LExpr -> LExpr -> LExpr
  , compileOr           :: LExpr -> LExpr -> LExpr
  , compileNot          :: Maybe (LExpr -> LExpr)
  , compileImplies      :: LExpr -> LExpr -> LExpr

  , compileLe           :: LExpr -> LExpr -> LExpr
  , compileLt           :: LExpr -> LExpr -> LExpr
  , compileGe           :: LExpr -> LExpr -> LExpr
  , compileGt           :: LExpr -> LExpr -> LExpr
  , compileEq           :: LExpr -> LExpr -> LExpr
  , compileNeq          :: LExpr -> LExpr -> LExpr

  , compileTrue         :: Double
  , compileFalse        :: Double
    }


--------------------------------------------------------------------------------
-- different avilable differentiable logics (types of translation from the constraint to loss function) are
-- DL2
-- Godel
-- Lukasiewicz
-- Product based
-- Yager

--they can be found in Vehicle.Backend.Prelude and the default option if none is provided is DL2.

-- part of the syntax translation that differ depending on chosen DL are:
    -- logical connectives (not, and, or, implies)
    -- comparisons (<, <=, >, >=, =, !=)

dl2Translation :: DifferentialLogicImplementation
dl2Translation = DifferentialLogicImplementation
  { compileAnd = Addition
  , compileOr  = Multiplication
  , compileNot = Nothing --this should be normalised out and pushed to the innermost level of comparisons by now
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

--sets parameter p for the Yager DL (by default set to 1)
yagerTranslation :: DifferentialLogicImplementation
yagerTranslation = parameterisedYagerTranslation 1 --change constant here


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