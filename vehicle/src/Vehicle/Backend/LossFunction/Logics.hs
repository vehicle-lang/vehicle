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
import Vehicle.Compile.Type.Subsystem.Standard.Core (StandardType)
import Vehicle.Expr.DSL
import Vehicle.Libraries.StandardLibrary (StdLibFunction (..))
import Vehicle.Syntax.AST

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

-- | Big operation (e.g. bigAnd, bigOr)
bigOp :: StdLibFunction -> PLExpr -> PLExpr
bigOp ident arg =
  free ident @@@ [tListRaw] @@@@ [builtinFunction (Fold FoldList) @@@ [tRat]] @@ [arg]

-- | Map a list
mapList :: PLExpr -> PLExpr -> PLExpr -> PLExpr -> PLExpr
mapList t1 t2 f xs = builtinFunction MapList @@@ [t1, t2] @@ [f, xs]

-- | Compiles a quantifier to a sampling procedure.
quantifierSampler :: StdLibFunction -> StandardType -> Name -> [Name] -> PLExpr
quantifierSampler bigOpIdent varType varName ctx = explLam (varName <> "_body") tUnit $ \f ->
  -- We are not getting the instance or the implicit arguments right here,
  -- but as they are erased, hopefully it's okay!
  bigOp bigOpIdent (mapList (toDSL varType) tRat f (builtinFunction (Sample varName ctx)))

-- | Compiles a `Forall` to a sampling procedure
forallSampler :: StandardType -> Name -> [Name] -> PLExpr
forallSampler = quantifierSampler StdBigAnd

-- | Compiles an `Exists` to a sampling procedure
existsSampler :: StandardType -> Name -> [Name] -> PLExpr
existsSampler = quantifierSampler StdBigOr

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
    compileForall :: StandardType -> Name -> [Name] -> PLExpr,
    compileExists :: StandardType -> Name -> [Name] -> PLExpr,
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
      compileAnd = BinaryAnd $ builtinFunction MaxRat,
      compileOr = BinaryOr $ builtinFunction MinRat,
      compileNot = UnaryNot $ builtinFunction (Neg NegRat),
      compileImplies = mkOp2 tRat $ \x y -> lmax (neg x) y,
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> x -: y,
      compileLt = mkOp2 tRat $ \x y -> x -: y,
      compileGe = mkOp2 tRat $ \x y -> y -: x,
      compileGt = mkOp2 tRat $ \x y -> y -: x,
      compileNeq = mkOp2 tRat ind,
      compileEq = mkOp2 tRat $ \x y -> lmax (x -: y) (y -: x)
    }

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
      compileAnd = BinaryAnd $ builtinFunction (Add AddRat),
      compileOr = BinaryOr $ builtinFunction (Mul MulRat),
      compileNot = TryToEliminate,
      compileImplies = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x *: y),
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: ind x y,
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x) +: ind y x,
      compileNeq = mkOp2 tRat ind,
      compileEq = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x)
    }

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
      compileAnd = BinaryAnd (mkOp2 tRat $ \x y -> ratLit 1 -: lmin x y),
      compileOr = BinaryOr (mkOp2 tRat $ \x y -> ratLit 1 -: lmax x y),
      compileNot = UnaryNot (mkOp1 tRat $ \x -> ratLit 1 -: x),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 1 -: x) y,
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }

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
      compileAnd = BinaryAnd (mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 0) ((x +: y) -: ratLit 1)),
      compileOr = BinaryOr (mkOp2 tRat $ \x y -> ratLit 1 -: lmin (x +: y) (ratLit 1)),
      compileNot = UnaryNot (mkOp1 tRat $ \arg -> ratLit 1 -: arg),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (ratLit 1) ((ratLit 1 -: x) +: y),
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
    }

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
      compileAnd = BinaryAnd (mkOp2 tRat $ \x y -> ratLit 1 -: (x *: y)),
      compileOr = BinaryOr (mkOp2 tRat $ \x y -> ratLit 1 -: ((x +: y) -: (x *: y))),
      compileNot = UnaryNot (mkOp1 tRat $ \x -> ratLit 1 -: x),
      compileImplies = mkOp2 tRat $ \x y -> ratLit 1 -: ((ratLit 1 -: x) +: (x *: y)),
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> ratLit 0 `lmax` (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
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
      compileBool = tRat,
      compileTrue = ratLit 0,
      compileFalse = ratLit 1,
      compileAnd =
        BinaryAnd
          ( mkOp2 tRat $ \x y ->
              ratLit 1
                -: lmax
                  ( ratLit 1
                      -: ( ((ratLit 1 -: x) ^: p)
                             +: ((ratLit 1 -: y) ^: p)
                         )
                      ^: (1 / p)
                  )
                  (ratLit 0)
          ),
      compileNot = UnaryNot (mkOp1 tRat (ratLit 1 -:)),
      compileOr =
        BinaryOr
          ( mkOp2 tRat $ \x y ->
              ratLit 1
                -: lmin
                  ( ((x ^: p) +: (y ^: p)) ^: (1 / p)
                  )
                  (ratLit 1)
          ),
      compileImplies = mkOp2 tRat $ \x y ->
        ratLit 1
          -: lmin
            ( (((ratLit 1 -: x) ^: p) +: (y ^: p))
                ^: (1 / p)
            )
            (ratLit 1),
      compileForall = forallSampler,
      compileExists = existsSampler,
      compileLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      compileGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      compileEq = mkOp2 tRat $ \x y -> ratLit 1 -: ind x y,
      compileNeq = mkOp2 tRat ind
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
