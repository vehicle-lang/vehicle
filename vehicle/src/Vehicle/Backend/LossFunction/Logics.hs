module Vehicle.Backend.LossFunction.Logics
  ( DifferentialLogicImplementation (..),
    AndTranslation (..),
    OrTranslation (..),
    NotTranslation (..),
    implementationOf,
  )
where

import Vehicle.Backend.JSON as J
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Prelude (developerError)
import Vehicle.Expr.DSL
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Patterns for building logics
--------------------------------------------------------------------------------

-- | A partial expression which requires provenance to construct.
type PLExpr = DSLExpr JBuiltin

mkOp1 :: PLExpr -> (PLExpr -> PLExpr) -> PLExpr
mkOp1 t f = explLam "x" t (\x -> f x)

mkOp2 :: PLExpr -> (PLExpr -> PLExpr -> PLExpr) -> PLExpr
mkOp2 t f = explLam "x" t (\x -> explLam "y" t (\y -> f x y))

op2 :: JBuiltin -> PLExpr -> PLExpr -> PLExpr
op2 op x y = builtin op @@ [x, y]

-- | Addition
(+:) :: PLExpr -> PLExpr -> PLExpr
(+:) = op2 (J.Add AddRat)

-- | Multiplication
(*:) :: PLExpr -> PLExpr -> PLExpr
(*:) = op2 (J.Mul MulRat)

-- | Subtraction
(-:) :: PLExpr -> PLExpr -> PLExpr
(-:) = op2 (J.Sub SubRat)

-- | Power
(^:) :: PLExpr -> Rational -> PLExpr
(^:) x y = op2 J.Pow x (lcon y)

-- | Indicator function
ind :: PLExpr -> PLExpr -> PLExpr
ind = op2 J.Indicator

-- | Maximum operator
lmax :: PLExpr -> PLExpr -> PLExpr
lmax = op2 J.Max

-- | Minimum operator
lmin :: PLExpr -> PLExpr -> PLExpr
lmin = op2 J.Min

-- | Constant
lcon :: Rational -> PLExpr
lcon x = builtin (toJBuiltin x)

rat :: PLExpr
rat = builtin J.RatType

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
    compileAnd :: AndTranslation,
    compileOr :: OrTranslation,
    compileNot :: NotTranslation,
    compileImplies :: PLExpr,
    compileLe :: PLExpr,
    compileLt :: PLExpr,
    compileGe :: PLExpr,
    compileGt :: PLExpr,
    compileEq :: PLExpr,
    compileNeq :: PLExpr,
    compileTrue :: PLExpr,
    compileFalse :: PLExpr
  }

--------------------------------------------------------------------------------
-- Logic implementations
--------------------------------------------------------------------------------

implementationOf :: DifferentiableLogicID -> DifferentialLogicImplementation
implementationOf = \case
  DL2Loss -> dl2Translation
  GodelLoss -> godelTranslation
  LukasiewiczLoss -> lukasiewiczTranslation
  ProductLoss -> productTranslation
  YagerLoss -> yagerTranslation
  STLLoss -> stlTranslation

--------------------------------------------------------------------------------
-- DL2

-- | Logic from Fischer, Marc, et al. "Dl2: Training and querying neural
-- networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicImplementation
dl2Translation =
  DifferentialLogicImplementation
    { logicID = DL2Loss,
      compileBool = builtin J.RatType,
      compileAnd = BinaryAnd $ builtin (J.Add AddRat),
      compileOr = BinaryOr $ builtin (J.Mul MulRat),
      compileNot = TryToEliminate,
      compileImplies = mkOp2 rat $ \x y -> lmax (lcon 0) (x *: y),
      compileLe = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileLt = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y) +: ind x y,
      compileGe = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileGt = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x) +: ind y x,
      compileNeq = mkOp2 rat ind,
      compileEq = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y) +: lmax (lcon 0) (x -: y),
      compileTrue = lcon 0,
      compileFalse = lcon 1
    }

--------------------------------------------------------------------------------
-- Godel

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
godelTranslation :: DifferentialLogicImplementation
godelTranslation =
  DifferentialLogicImplementation
    { logicID = GodelLoss,
      compileBool = builtin J.RatType,
      compileAnd = BinaryAnd (mkOp2 rat $ \x y -> lcon 1 -: lmin x y),
      compileOr = BinaryOr (mkOp2 rat $ \x y -> lcon 1 -: lmax x y),
      compileNot = UnaryNot (mkOp1 rat $ \x -> lcon 1 -: x),
      compileImplies = mkOp2 rat $ \x y -> lcon 1 -: lmax (lcon 1 -: x) y,
      compileLe = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileLt = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileGe = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileGt = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileEq = mkOp2 rat $ \x y -> lcon 1 -: ind x y,
      compileNeq = mkOp2 rat ind,
      compileTrue = lcon 0,
      compileFalse = lcon 1
    }

--------------------------------------------------------------------------------
-- Lukasiewicz

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
lukasiewiczTranslation :: DifferentialLogicImplementation
lukasiewiczTranslation =
  DifferentialLogicImplementation
    { logicID = LukasiewiczLoss,
      compileBool = builtin J.RatType,
      compileAnd = BinaryAnd (mkOp2 rat $ \x y -> lcon 1 -: lmax (lcon 0) ((x +: y) -: lcon 1)),
      compileOr = BinaryOr (mkOp2 rat $ \x y -> lcon 1 -: lmin (x +: y) (lcon 1)),
      compileNot = UnaryNot (mkOp1 rat $ \arg -> lcon 1 -: arg),
      compileImplies = mkOp2 rat $ \x y -> lcon 1 -: lmin (lcon 1) ((lcon 1 -: x) +: y),
      compileLe = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileLt = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileGe = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileGt = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileEq = mkOp2 rat $ \x y -> lcon 1 -: ind x y,
      compileNeq = mkOp2 rat ind,
      compileTrue = lcon 0,
      compileFalse = lcon 1
    }

--------------------------------------------------------------------------------
-- Product

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
productTranslation :: DifferentialLogicImplementation
productTranslation =
  DifferentialLogicImplementation
    { logicID = ProductLoss,
      compileBool = builtin J.RatType,
      compileAnd = BinaryAnd (mkOp2 rat $ \x y -> lcon 1 -: (x *: y)),
      compileOr = BinaryOr (mkOp2 rat $ \x y -> lcon 1 -: ((x +: y) -: (x *: y))),
      compileNot = UnaryNot (mkOp1 rat $ \x -> lcon 1 -: x),
      compileImplies = mkOp2 rat $ \x y -> lcon 1 -: ((lcon 1 -: x) +: (x *: y)),
      compileLe = mkOp2 rat $ \x y -> lcon 0 `lmax` (x -: y),
      compileLt = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileGe = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileGt = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileEq = mkOp2 rat $ \x y -> lcon 1 -: ind x y,
      compileNeq = mkOp2 rat ind,
      compileTrue = lcon 0,
      compileFalse = lcon 1
    }

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
      compileBool = builtin J.RatType,
      compileAnd =
        BinaryAnd
          ( mkOp2 rat $ \x y ->
              lcon 1
                -: lmax
                  ( lcon 1
                      -: ( ((lcon 1 -: x) ^: p)
                             +: ((lcon 1 -: y) ^: p)
                         )
                      ^: (1 / p)
                  )
                  (lcon 0)
          ),
      compileNot = UnaryNot (mkOp1 rat (lcon 1 -:)),
      compileOr =
        BinaryOr
          ( mkOp2 rat $ \x y ->
              lcon 1
                -: lmin
                  ( ((x ^: p) +: (y ^: p)) ^: (1 / p)
                  )
                  (lcon 1)
          ),
      compileImplies = mkOp2 rat $ \x y ->
        lcon 1
          -: lmin
            ( (((lcon 1 -: x) ^: p) +: (y ^: p))
                ^: (1 / p)
            )
            (lcon 1),
      compileLe = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileLt = mkOp2 rat $ \x y -> lmax (lcon 0) (x -: y),
      compileGe = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileGt = mkOp2 rat $ \x y -> lmax (lcon 0) (y -: x),
      compileEq = mkOp2 rat $ \x y -> lcon 1 -: ind x y,
      compileNeq = mkOp2 rat ind,
      compileTrue = lcon 0,
      compileFalse = lcon 1
    }

--------------------------------------------------------------------------------
-- STL translation

-- | from Varnai and Dimarogonas, "On Robustness Metrics for Learning STL Tasks." 2020
stlTranslation :: DifferentialLogicImplementation
stlTranslation = developerError "STL logic not yet implemented"

{-
  DifferentialLogicImplementation
    { logicID = STLLoss,
      compileBool = builtin J.Rat,
      compileAnd = NaryAnd (mkOp1 rat $ \x -> exponentialAnd x),
      compileOr = NaryOr (mkOp1 rat $ \x -> neg (exponentialAnd (builtin _ @@ [x]))),
      compileNot = UnaryNot (mkOp1 rat neg),
      compileImplies = mkOp2 rat $ \x y -> neg (exponentialAnd (map neg [neg x, y])),
      compileLe = builtin (J.Sub SubRat),
      compileLt = builtin (J.Sub SubRat),
      compileGe = mkOp2 rat (\x y -> y -: x),
      compileGt = mkOp2 rat (\x y -> y -: x),
      compileEq = mkOp2 rat ind,
      compileNeq = mkOp2 rat $ \x y -> neg (ind x y),
      compileTrue = lcon 1,
      compileFalse = lcon (-1)
    }
    -}
