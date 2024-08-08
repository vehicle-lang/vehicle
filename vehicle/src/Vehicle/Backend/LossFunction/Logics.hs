module Vehicle.Backend.LossFunction.Logics
  ( DifferentialLogicDSL,
    PLExpr,
    dslFor,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.LinkedHashMap (LinkedHashMap)
import Data.LinkedHashMap qualified as Map (fromList)
import Prettyprinter
import Vehicle.Backend.LossFunction.Core as L
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Prelude (Expr, Ix, developerError)
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.DSL

--------------------------------------------------------------------------------
-- Patterns for building logics
--------------------------------------------------------------------------------

-- | A partial expression which requires provenance to construct.
type PLExpr = DSLExpr Builtin

mkOp1 :: PLExpr -> (PLExpr -> PLExpr) -> PLExpr
mkOp1 t f = explLam "x" t (\x -> f x)

mkOp2 :: PLExpr -> (PLExpr -> PLExpr -> PLExpr) -> PLExpr
mkOp2 t f = explLam "x" t (\x -> explLam "y" t (\y -> f x y))

builtinFunction :: BuiltinFunction -> PLExpr
builtinFunction op = builtin (BuiltinFunction op)

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

-- | Division
(/:) :: PLExpr -> PLExpr -> PLExpr
(/:) = op2 (Div DivRat)

-- | Negation
neg :: PLExpr -> PLExpr
neg = op1 (Neg NegRat)

-- | Power
(^:) :: PLExpr -> Rational -> PLExpr
(^:) x y = op2 PowRat x (ratLit y)

-- | Maximum operator
lmax :: PLExpr -> PLExpr -> PLExpr
lmax = op2 MaxRat

-- | Minimum operator
lmin :: PLExpr -> PLExpr -> PLExpr
lmin = op2 MinRat

ratLit :: Rational -> PLExpr
ratLit r = builtin (BuiltinConstructor (LRat r))

tRat :: PLExpr
tRat = builtin (BuiltinType Rat)

--------------------------------------------------------------------------------
-- Logics
--------------------------------------------------------------------------------

-- | Template for different avilable differentiable logics
-- part of the syntax translation that differ depending on chosen DL are:
-- logical connectives (not, and, or, implies)
-- comparisons (<, <=, >, >=, =, !=)
type DifferentialLogicDSL = LinkedHashMap DifferentiableLogicField (Expr Ix Builtin)

mkDSL :: [(DifferentiableLogicField, PLExpr)] -> DifferentialLogicDSL
mkDSL = Map.fromList . fmap (second (fromDSL mempty))

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
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit (-100000)),
      (L.Falsity, ratLit 100000),
      (L.Conjunction, andOp),
      (L.Disjunction, orOp),
      (L.Negation, builtinFunction (Neg NegRat)),
      (L.Implication, mkOp2 tRat $ \x y -> lmax (neg x) y),
      (L.LessThan, mkOp2 tRat $ \x y -> x -: y),
      (L.LessEqual, mkOp2 tRat $ \x y -> x -: y),
      (L.GreaterThan, mkOp2 tRat $ \x y -> y -: x),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> y -: x),
      (L.Equal, mkOp2 tRat $ \x y -> neg (lmax (x -: y) (y -: x))),
      (L.NotEqual, mkOp2 tRat $ \x y -> lmax (x -: y) (y -: x))
    ]
  where
    andOp = builtinFunction MaxRat
    orOp = builtinFunction MinRat

--------------------------------------------------------------------------------
-- DL2

-- | Logic from Fischer, Marc, et al. "Dl2: Training and querying neural
-- networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicDSL
dl2Translation =
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit 0),
      (L.Falsity, ratLit 1), -- TODO this should be infinity??)
      (L.Conjunction, andOp),
      (L.Disjunction, orOp),
      (L.Negation, mkOp1 tRat $ \x -> ratLit 1 /: x),
      (L.Implication, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x *: y)),
      (L.LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.Equal, mkOp2 tRat $ \x y -> neg (lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x))),
      (L.NotEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x))
    ]
  where
    andOp = builtinFunction (Add AddRat)
    orOp = builtinFunction (Mul MulRat)

--------------------------------------------------------------------------------
-- Godel

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
godelTranslation :: DifferentialLogicDSL
godelTranslation =
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit 0),
      (L.Falsity, ratLit 1),
      (L.Conjunction, andOp),
      (L.Disjunction, orOp),
      (L.Negation, mkOp1 tRat $ \x -> ratLit 1 -: x),
      (L.Implication, mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 1 -: x) y),
      (L.LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.Equal, unsupported "==" "Godel"),
      (L.NotEqual, unsupported "!=" "Godel")
    ]
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin x y
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax x y

--------------------------------------------------------------------------------
-- Lukasiewicz

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
lukasiewiczTranslation :: DifferentialLogicDSL
lukasiewiczTranslation =
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit 0),
      (L.Falsity, ratLit 1),
      (L.Conjunction, andOp),
      (L.Disjunction, orOp),
      (L.Negation, mkOp1 tRat $ \arg -> ratLit 1 -: arg),
      (L.Implication, mkOp2 tRat $ \x y -> ratLit 1 -: lmin (ratLit 1) ((ratLit 1 -: x) +: y)),
      (L.LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.Equal, unsupported "==" "Lukasiewicz"),
      (L.NotEqual, unsupported "!=" "Lukasiewicz")
    ]
  where
    andOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 0) ((x +: y) -: ratLit 1)
    orOp = mkOp2 tRat $ \x y -> ratLit 1 -: lmin (x +: y) (ratLit 1)

--------------------------------------------------------------------------------
-- Product

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
productTranslation :: DifferentialLogicDSL
productTranslation =
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit 0),
      (L.Falsity, ratLit 1),
      (L.Conjunction, andOp),
      (L.Disjunction, andOp),
      (L.Negation, mkOp1 tRat $ \x -> ratLit 1 -: x),
      (L.Implication, mkOp2 tRat $ \x y -> ratLit 1 -: ((ratLit 1 -: x) +: (x *: y))),
      (L.LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.Equal, unsupported "==" "Product"),
      (L.NotEqual, unsupported "!=" "Product")
    ]
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
  mkDSL
    [ (L.Bool, tRat),
      (L.Truthity, ratLit 0),
      (L.Falsity, ratLit 1),
      (L.Conjunction, andOp),
      (L.Disjunction, orOp),
      (L.Negation, mkOp1 tRat (ratLit 1 -:)),
      ( L.Implication,
        mkOp2 tRat $ \x y ->
          ratLit 1
            -: lmin
              ( (((ratLit 1 -: x) ^: p) +: (y ^: p))
                  ^: (1 / p)
              )
              (ratLit 1)
      ),
      (L.LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (L.GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (L.Equal, unsupported "==" "Yager"),
      (L.NotEqual, unsupported "!=" "Yager")
    ]
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
  mkDSL
    [ ( STL,ss)
    , (Bool, builtin J.Rat)
    , (And =,aryAnd (mkOp1 tRat $ \x -> exponentialAnd x))
    , (Conjunction,NaryOr (mkOp1 tRat $ \x -> neg (exponentialAnd (builtin _ @@ [x]))))
    , (Disjunction,= UnaryNot (mkOp1 tRat neg))
    , (Negation,es = mkOp2 tRat $ \x y -> neg (exponentialAnd (map neg [neg x, y])))
    , (Implication,tin (J.Sub SubRat))
    , (LessThan, builtin (J.Sub SubRat))
    , (LessEqual, mkOp2 tRat (\x y -> y -: x))
    , (GreaterThan, mkOp2 tRat (\x y -> y -: x))
    , (GreaterEqual, mkOp2 tRat ind)
    , (Neual,= mkOp2 tRat $ \x y -> neg (ind x y))
    , (NotEqual,= ratLit 1)
    ] translateFalse = ratLit (-1)
    }
-}
