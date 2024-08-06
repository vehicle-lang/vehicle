module Vehicle.Backend.LossFunction.Logics
  ( DifferentialLogicDSL (..),
    NotTranslation (..),
    PLExpr,
    dslFor,
  )
where

import Prettyprinter
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Prelude (developerError)
import Vehicle.Data.Builtin.Loss
import Vehicle.Data.DSL
import Vehicle.Syntax.Builtin qualified as V

--------------------------------------------------------------------------------
-- Patterns for building logics
--------------------------------------------------------------------------------

-- | A partial expression which requires provenance to construct.
type PLExpr = DSLExpr LossBuiltin

mkOp1 :: PLExpr -> (PLExpr -> PLExpr) -> PLExpr
mkOp1 t f = explLam "x" t (\x -> f x)

mkOp2 :: PLExpr -> (PLExpr -> PLExpr -> PLExpr) -> PLExpr
mkOp2 t f = explLam "x" t (\x -> explLam "y" t (\y -> f x y))

op1 :: LossBuiltin -> PLExpr -> PLExpr
op1 op x = builtin op @@ [x]

op2 :: LossBuiltin -> PLExpr -> PLExpr -> PLExpr
op2 op x y = builtin op @@ [x, y]

-- | Addition
(+:) :: PLExpr -> PLExpr -> PLExpr
(+:) = op2 (Add V.AddRat)

-- | Multiplication
(*:) :: PLExpr -> PLExpr -> PLExpr
(*:) = op2 (Mul V.MulRat)

-- | Subtraction
(-:) :: PLExpr -> PLExpr -> PLExpr
(-:) = op2 (Sub V.SubRat)

-- | Division
(/:) :: PLExpr -> PLExpr -> PLExpr
(/:) = op2 (Div V.DivRat)

-- | Negation
neg :: PLExpr -> PLExpr
neg = op1 (Neg V.NegRat)

-- | Power
(^:) :: PLExpr -> Rational -> PLExpr
(^:) x y = op2 PowRat x (ratLit y)

-- | Maximum operator
lmax :: PLExpr -> PLExpr -> PLExpr
lmax x y = builtin MaxRat @@ [x, y]

ratLit :: Rational -> PLExpr
ratLit r = builtin (Rat r)

tRat :: PLExpr
tRat = builtin RatType

-- | Minimum operator
lmin :: PLExpr -> PLExpr -> PLExpr
lmin x y = builtin MinRat @@ [x, y]

--------------------------------------------------------------------------------
-- Logics
--------------------------------------------------------------------------------

data NotTranslation
  = TryToEliminate
  | UnaryNot PLExpr -- (LExpr -> LExpr)

-- | Template for different avilable differentiable logics
-- part of the syntax translation that differ depending on chosen DL are:
-- logical connectives (not, and, or, implies)
-- comparisons (<, <=, >, >=, =, !=)
data DifferentialLogicDSL = DifferentialLogicDSL
  { logicID :: DifferentiableLogicID,
    translateBool :: PLExpr,
    translateTrue :: PLExpr,
    translateFalse :: PLExpr,
    translateAnd :: PLExpr,
    translateOr :: PLExpr,
    translateNot :: PLExpr,
    translateImplies :: PLExpr,
    translateLe :: PLExpr,
    translateLt :: PLExpr,
    translateGe :: PLExpr,
    translateGt :: PLExpr,
    translateEq :: PLExpr,
    translateNeq :: PLExpr
  }

--------------------------------------------------------------------------------
-- Logic implementations
--------------------------------------------------------------------------------

dslFor :: DifferentiableLogicID -> DifferentialLogicDSL
dslFor = \case
  VehicleLoss -> vehicleTranslation
  DL2Loss -> dl2Translation
  GodelLoss -> godelTranslation
  LukasiewiczLoss -> lukasiewiczTranslation
  ProductLoss -> productTranslation
  YagerLoss -> yagerTranslation
  STLLoss -> stlTranslation

--------------------------------------------------------------------------------
-- Main vehicle logic

vehicleTranslation :: DifferentialLogicDSL
vehicleTranslation =
  DifferentialLogicDSL
    { logicID = DL2Loss,
      translateBool = tRat,
      translateTrue = ratLit (-100000),
      translateFalse = ratLit 100000,
      translateAnd = andOp,
      translateOr = orOp,
      translateNot = builtin (Neg V.NegRat),
      translateImplies = mkOp2 tRat $ \x y -> lmax (neg x) y,
      translateLe = mkOp2 tRat $ \x y -> x -: y,
      translateLt = mkOp2 tRat $ \x y -> x -: y,
      translateGe = mkOp2 tRat $ \x y -> y -: x,
      translateGt = mkOp2 tRat $ \x y -> y -: x,
      translateNeq = mkOp2 tRat $ \x y -> neg (lmax (x -: y) (y -: x)),
      translateEq = mkOp2 tRat $ \x y -> lmax (x -: y) (y -: x)
    }
  where
    andOp = builtin MaxRat
    orOp = builtin MinRat

--------------------------------------------------------------------------------
-- DL2

-- | Logic from Fischer, Marc, et al. "Dl2: Training and querying neural
-- networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicDSL
dl2Translation =
  DifferentialLogicDSL
    { logicID = DL2Loss,
      translateBool = tRat,
      translateTrue = ratLit 0,
      translateFalse = ratLit 1, -- TODO this should be infinity???
      translateAnd = andOp,
      translateOr = orOp,
      translateNot = mkOp1 tRat $ \x -> ratLit 1 /: x,
      translateImplies = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x *: y),
      translateLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateNeq = mkOp2 tRat $ \x y -> neg (lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x)),
      translateEq = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x)
    }
  where
    andOp = builtin (Add V.AddRat)
    orOp = builtin (Mul V.MulRat)

--------------------------------------------------------------------------------
-- Godel

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
godelTranslation :: DifferentialLogicDSL
godelTranslation =
  DifferentialLogicDSL
    { logicID = GodelLoss,
      translateBool = tRat,
      translateTrue = ratLit 0,
      translateFalse = ratLit 1,
      translateAnd = andOp,
      translateOr = orOp,
      translateNot = mkOp1 tRat $ \x -> ratLit 1 -: x,
      translateImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 1 -: x) y,
      translateLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateEq = unsupported "==" "Godel",
      translateNeq = unsupported "!=" "Godel"
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin x y
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax x y

--------------------------------------------------------------------------------
-- Lukasiewicz

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
lukasiewiczTranslation :: DifferentialLogicDSL
lukasiewiczTranslation =
  DifferentialLogicDSL
    { logicID = LukasiewiczLoss,
      translateBool = tRat,
      translateTrue = ratLit 0,
      translateFalse = ratLit 1,
      translateAnd = andOp,
      translateOr = orOp,
      translateNot = mkOp1 tRat $ \arg -> ratLit 1 -: arg,
      translateImplies = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (ratLit 1) ((ratLit 1 -: x) +: y),
      translateLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateEq = unsupported "==" "Lukasiewicz",
      translateNeq = unsupported "!=" "Lukasiewicz"
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 0) ((x +: y) -: ratLit 1)
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (x +: y) (ratLit 1)

--------------------------------------------------------------------------------
-- Product

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
productTranslation :: DifferentialLogicDSL
productTranslation =
  DifferentialLogicDSL
    { logicID = ProductLoss,
      translateBool = tRat,
      translateTrue = ratLit 0,
      translateFalse = ratLit 1,
      translateAnd = andOp,
      translateOr = andOp,
      translateNot = mkOp1 tRat $ \x -> ratLit 1 -: x,
      translateImplies = mkOp2 tRat $ \x y -> ratLit 1 -: ((ratLit 1 -: x) +: (x *: y)),
      translateLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateEq = unsupported "==" "Product",
      translateNeq = unsupported "!=" "Product"
    }
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: (x *: y)

--------------------------------------------------------------------------------
-- Yager

-- | Sets parameter p for the Yager DL (by default set to 1)
yagerTranslation :: DifferentialLogicDSL
yagerTranslation = parameterisedYagerTranslation 1 -- change lconstant here

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
parameterisedYagerTranslation :: Rational -> DifferentialLogicDSL
parameterisedYagerTranslation p =
  DifferentialLogicDSL
    { logicID = YagerLoss,
      translateBool = tRat,
      translateTrue = ratLit 0,
      translateFalse = ratLit 1,
      translateAnd = andOp,
      translateOr = orOp,
      translateNot = mkOp1 tRat (ratLit 1 -:),
      translateImplies = mkOp2 tRat $ \x y ->
        ratLit 1
          -: lmin
            ( (((ratLit 1 -: x) ^: p) +: (y ^: p))
                ^: (1 / p)
            )
            (ratLit 1),
      translateLe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateLt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y),
      translateGe = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateGt = mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x),
      translateEq = unsupported "==" "Yager",
      translateNeq = unsupported "!=" "Yager"
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
stlTranslation :: DifferentialLogicDSL
stlTranslation = developerError "STL logic not yet implemented"

unsupported :: Doc a -> Doc a -> a
unsupported op logic = developerError $ "Translating" <+> op <+> "not yet supported for" <+> logic <+> "logic"

{-
  DifferentialLogicDSL
    { logicID = STLLoss,
      translateBool = builtin J.Rat,
      translateAnd = NaryAnd (mkOp1 tRat $ \x -> exponentialAnd x),
      translateOr = NaryOr (mkOp1 tRat $ \x -> neg (exponentialAnd (builtin _ @@ [x]))),
      translateNot = UnaryNot (mkOp1 tRat neg),
      translateImplies = mkOp2 tRat $ \x y -> neg (exponentialAnd (map neg [neg x, y])),
      translateLe = builtin (J.Sub SubRat),
      translateLt = builtin (J.Sub SubRat),
      translateGe = mkOp2 tRat (\x y -> y -: x),
      translateGt = mkOp2 tRat (\x y -> y -: x),
      translateEq = mkOp2 tRat ind,
      translateNeq = mkOp2 tRat $ \x y -> neg (ind x y),
      translateTrue = ratLit 1,
      translateFalse = ratLit (-1)
    }
-}
