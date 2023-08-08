module Vehicle.Backend.LossFunction.Logics
  ( DifferentialLogicImplementation (..),
    AndTranslation (..),
    OrTranslation (..),
    NotTranslation (..),
    implementationOf,
  )
where

import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Prelude (developerError)
import Vehicle.Compile.Type.Subsystem.Standard.Core (StandardDSLExpr)
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DSL
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin

--------------------------------------------------------------------------------
-- Patterns for building logics
--------------------------------------------------------------------------------

-- | A partial expression which requires provenance to construct.
type PLExpr = StandardDSLExpr

mkOp1 :: PLExpr -> (PLExpr -> PLExpr) -> PLExpr
mkOp1 t f = explLam "x" t (\x -> f x)

mkOp2 :: PLExpr -> (PLExpr -> PLExpr -> PLExpr) -> PLExpr
mkOp2 t f = explLam "x" t (\x -> explLam "y" t (\y -> f x y))

op1 :: BuiltinFunction -> PLExpr -> PLExpr
op1 op x = builtinFunction op @@ [x]

op2 :: BuiltinFunction -> PLExpr -> PLExpr -> PLExpr
op2 op x y = builtinFunction op @@ [x, y]

-- | Addition
(+:) :: PLExpr -> PLExpr -> PLExpr
(+:) = op2 (Add AddRat)

-- | Multiplication
(*:) :: PLExpr -> PLExpr -> PLExpr
(*:) = op2 (Mul MulRat)

-- | Subtraction
(-:) :: PLExpr -> PLExpr -> PLExpr
(-:) = op2 (Sub SubRat)

-- | Negation
neg :: PLExpr -> PLExpr
neg = op1 (Neg NegRat)

-- | Power
(^:) :: PLExpr -> Rational -> PLExpr
(^:) x y = op2 PowRat x (ratLit y)

-- | Indicator function
ind :: PLExpr -> PLExpr -> PLExpr
ind x y = builtinFunction If @@ [builtinFunction (Equals EqRat Eq) @@ [x, y], ratLit 1, ratLit 0]

-- | Maximum operator
lmax :: PLExpr -> PLExpr -> PLExpr
lmax x y = builtinFunction MaxRat @@ [x, y]

-- | Minimum operator
lmin :: PLExpr -> PLExpr -> PLExpr
lmin x y = builtinFunction MinRat @@ [x, y]

-- | Compiles a quantifier to a sampling procedure.
quantifierSampler :: Bool -> PLExpr -> PLExpr -> Name -> [Name] -> PLExpr
quantifierSampler maximise bop body varName ctx =
  builtinFunction (Optimise varName maximise ctx) @@ [bop, body]

-- | Compiles a `Forall` to a sampling procedure
forallSampler :: Bool -> PLExpr -> PLExpr -> Name -> [Name] -> PLExpr
forallSampler = quantifierSampler

-- | Compiles an `Exists` to a sampling procedure
existsSampler :: Bool -> PLExpr -> PLExpr -> Name -> [Name] -> PLExpr
existsSampler = quantifierSampler

--------------------------------------------------------------------------------
-- Logics
--------------------------------------------------------------------------------

data AndTranslation
  = BinaryAnd PLExpr -- LExpr -> LExpr -> LExpr
  | NaryAnd PLExpr -- ([LExpr] -> LExpr)

data OrTranslation
  = BinaryOr PLExpr -- LExpr -> LExpr -> LExpr
  | NaryOr PLExpr -- ([LExpr] -> LExpr)

data NotTranslation
  = TryToEliminate
  | UnaryNot PLExpr -- (LExpr -> LExpr)

--  | Template for different avilable differentiable logics
--  | part of the syntax translation that differ depending on chosen DL are:
--  | logical lconnectives (not, and, or, implies)
--  | comparisons (<, <=, >, >=, =, !=)
data DifferentialLogicImplementation = DifferentialLogicImplementation
  { logicID :: DifferentiableLogicID,
    compileBool :: PLExpr,
    compileTrue :: PLExpr,
    compileFalse :: PLExpr,
    compileAnd :: AndTranslation,
    compileOr :: OrTranslation,
    compileNot :: NotTranslation,
    compileImplies :: PLExpr,
    compileForall :: PLExpr -> Name -> [Name] -> PLExpr,
    compileExists :: PLExpr -> Name -> [Name] -> PLExpr,
    compileLe :: PLExpr,
    compileLt :: PLExpr,
    compileGe :: PLExpr,
    compileGt :: PLExpr,
    compileEq :: PLExpr,
    compileNeq :: PLExpr
  }

--------------------------------------------------------------------------------
-- Logic implementations
--------------------------------------------------------------------------------

implementationOf :: DifferentiableLogicID -> DifferentialLogicImplementation
implementationOf = \case
  VehicleLoss -> vehicleTranslation
  DL2Loss -> dl2Translation
  GodelLoss -> godelTranslation
  LukasiewiczLoss -> lukasiewiczTranslation
  ProductLoss -> productTranslation
  YagerLoss -> yagerTranslation
  STLLoss -> stlTranslation

--------------------------------------------------------------------------------
-- Main vehicle logic

vehicleTranslation :: DifferentialLogicImplementation
vehicleTranslation =
  DifferentialLogicImplementation
    { logicID = DL2Loss,
      compileBool = tRat,
      compileTrue = ratLit (-100000),
      compileFalse = ratLit 100000,
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr orOp,
      compileNot = UnaryNot $ builtinFunction (Neg NegRat),
      compileImplies = mkOp2 tRat $ \x y -> lmax (neg x) y,
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> x -: y,
      compileLt = mkOp2 tRat $ \x y -> x -: y,
      compileGe = mkOp2 tRat $ \x y -> y -: x,
      compileGt = mkOp2 tRat $ \x y -> y -: x,
      compileNeq = mkOp2 tRat ind,
      compileEq = mkOp2 tRat $ \x y -> lmax (x -: y) (y -: x)
    }
  where
    andOp = builtinFunction MaxRat
    orOp = builtinFunction MinRat

--------------------------------------------------------------------------------
-- DL2

-- | Logic from Fischer, Marc, et al. "Dl2: Training and querying neural
-- networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicImplementation
dl2Translation =
  DifferentialLogicImplementation
    { logicID = DL2Loss,
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1, -- TODO this should be infinity???
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr orOp,
      compileNot = TryToEliminate,
      compileImplies = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x *: y),
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: ind x y,
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x) +: ind y x,
      compileNeq = mkOp2 tRat ind,
      compileEq = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x)
    }
  where
    andOp = builtinFunction (Add AddRat)
    orOp = builtinFunction (Mul MulRat)

--------------------------------------------------------------------------------
-- Godel

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
godelTranslation :: DifferentialLogicImplementation
godelTranslation =
  DifferentialLogicImplementation
    { logicID = GodelLoss,
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1,
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr orOp,
      compileNot = UnaryNot (mkOp1 tRat $ \x -> ratLit 1 -: x),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 1 -: x) y,
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin x y
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax x y

--------------------------------------------------------------------------------
-- Lukasiewicz

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
lukasiewiczTranslation :: DifferentialLogicImplementation
lukasiewiczTranslation =
  DifferentialLogicImplementation
    { logicID = LukasiewiczLoss,
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1,
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr orOp,
      compileNot = UnaryNot (mkOp1 tRat $ \arg -> ratLit 1 -: arg),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (ratLit 1) ((ratLit 1 -: x) +: y),
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 0) ((x +: y) -: ratLit 1)
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (x +: y) (ratLit 1)

--------------------------------------------------------------------------------
-- Product

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
productTranslation :: DifferentialLogicImplementation
productTranslation =
  DifferentialLogicImplementation
    { logicID = ProductLoss,
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1,
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr andOp,
      compileNot = UnaryNot (mkOp1 tRat $ \x -> ratLit 1 -: x),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: ((ratLit 1 -: x) +: (x *: y)),
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> ratLit 0 `lmax` (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: (x *: y)
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: ((x +: y) -: (x *: y))

--------------------------------------------------------------------------------
-- Yager

-- | Sets parameter p for the Yager DL (by default set to 1)
yagerTranslation :: DifferentialLogicImplementation
yagerTranslation = parameterisedYagerTranslation 1 -- change lconstant here

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
parameterisedYagerTranslation :: Rational -> DifferentialLogicImplementation
parameterisedYagerTranslation p =
  DifferentialLogicImplementation
    { logicID = YagerLoss,
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1,
      compileAnd = BinaryAnd andOp,
      compileOr = BinaryOr orOp,
      compileNot = UnaryNot (mkOp1 tRat (ratLit 1 -:)),
      compileImplies = mkOp2 tRat $ \x y ->
        ratLit 1
          -: lmin
            ( (((ratLit 1 -: x) ^: p) +: (y ^: p))
                ^: (1 / p)
            )
            (ratLit 1),
      compileForall = forallSampler True andOp,
      compileExists = existsSampler True orOp,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }
  where
    andOp = mkOp2 tRat $ \x y ->
      ratLit 1
        -: lmax
          ( ratLit 1
              -: ( ((ratLit 1 -: x) ^: p)
                     +: ((ratLit 1 -: y) ^: p)
                 )
              ^: (1 / p)
          )
          (ratLit 0)
    orOp = mkOp2 tRat $ \x y ->
      ratLit 1
        -: lmin
          ( ((x ^: p) +: (y ^: p)) ^: (1 / p)
          )
          (ratLit 1)

--------------------------------------------------------------------------------
-- STL translation

-- | from Varnai and Dimarogonas, "On Robustness Metrics for Learning STL Tasks." 2020
stlTranslation :: DifferentialLogicImplementation
stlTranslation = developerError "STL logic not yet implemented"

{-
  DifferentialLogicImplementation
    { logicID = STLLoss,
      compileBool = builtin J.Rat,
      compileAnd = NaryAnd (mkOp1 tRat $ \x -> exponentialAnd x),
      compileOr = NaryOr (mkOp1 tRat $ \x -> neg (exponentialAnd (builtin _ @@ [x]))),
      compileNot = UnaryNot (mkOp1 tRat neg),
      compileImplies = mkOp2 tRat $ \x y -> neg (exponentialAnd (map neg [neg x, y])),
      compileLe = builtin (J.Sub SubRat),
      compileLt = builtin (J.Sub SubRat),
      compileGe = mkOp2 tRat (\x y -> y -: x),
      compileGt = mkOp2 tRat (\x y -> y -: x),
      compileEq = mkOp2 tRat ind,
      compileNeq = mkOp2 tRat $ \x y -> neg (ind x y),
      compileTrue = ratLit 1,
      compileFalse = ratLit (-1)
    }
    -}
