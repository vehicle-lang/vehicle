module Vehicle.Backend.Loss.Logics
  ( DifferentialLogicDSL,
    CompiledDifferentiableLogic,
    DifferentiableLogicImplementation,
    BooleanDifferentiableLogicField (..),
    TensorDifferentiableLogicField (..),
    PLExpr,
    dslFor,
    pattern VLam2,
    comparisonOpToField,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map (fromList)
import GHC.Generics (Generic)
import Prettyprinter
import Vehicle.Backend.Prelude (DifferentiableLogicID (..))
import Vehicle.Compile.Prelude (Binder, Expr (..), developerError)
import Vehicle.Data.Builtin.Loss (LossBuiltin)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.DSL (dimNil, ratLit, tRatTensor)
import Vehicle.Data.Code.Value
import Vehicle.Data.DSL

--------------------------------------------------------------------------------
-- Boolean implementation

data BooleanDifferentiableLogicField
  = Truthity
  | Falsity
  | Conjunction
  | Disjunction
  | Negation
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Ord, Show, Generic)

instance Pretty BooleanDifferentiableLogicField where
  pretty = pretty . show

instance Hashable BooleanDifferentiableLogicField

--------------------------------------------------------------------------------
-- Tensor implementation

data TensorDifferentiableLogicField
  = TruthityElement
  | FalsityElement
  | PointwiseConjunction
  | PointwiseDisjunction
  | PointwiseNegation
  | PointwiseLe
  | PointwiseLt
  | PointwiseGe
  | PointwiseGt
  | PointwiseEq
  | PointwiseNe
  | ReduceConjunction
  | ReduceDisjunction
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

instance Pretty TensorDifferentiableLogicField where
  pretty = pretty . show

type DifferentiableLogicImplementation =
  Map TensorDifferentiableLogicField (Value LossBuiltin)

type CompiledDifferentiableLogic = (DifferentiableLogicID, DifferentiableLogicImplementation)

comparisonOpToField :: ComparisonOp -> TensorDifferentiableLogicField
comparisonOpToField = \case
  Le -> PointwiseLe
  Lt -> PointwiseLt
  Ge -> PointwiseGe
  Gt -> PointwiseGt
  Eq -> PointwiseEq
  Ne -> PointwiseNe

--------------------------------------------------------------------------------
-- Other

pattern VLam2 :: VBinder builtin -> BoundEnv builtin -> Binder builtin -> Expr builtin -> Value builtin
pattern VLam2 binder1 env binder2 body <- VLam binder1 (Closure env (Lam _ binder2 body))

--------------------------------------------------------------------------------
-- Patterns for building logics
--------------------------------------------------------------------------------

-- | A partial expression which requires provenance to construct.
type PLExpr = DSLExpr Builtin

tRat :: PLExpr
tRat = tRatTensor dimNil

mkOp1 :: PLExpr -> (PLExpr -> PLExpr) -> PLExpr
mkOp1 t f = explLam "x" t (\x -> f x)

mkOp2 :: PLExpr -> (PLExpr -> PLExpr -> PLExpr) -> PLExpr
mkOp2 t f = explLam "x" t (\x -> explLam "y" t (\y -> f x y))

builtinFunction :: BuiltinFunction -> PLExpr
builtinFunction op = builtin (BuiltinFunction op)

op1 :: BuiltinFunction -> PLExpr -> PLExpr
op1 op x = builtinFunction op .@@@ [dimNil] @@ [x]

op2 :: BuiltinFunction -> PLExpr -> PLExpr -> PLExpr
op2 op x y = builtinFunction op .@@@ [dimNil] @@ [x, y]

-- | Negation
ne :: PLExpr -> PLExpr
ne = op1 $ Neg NegRatTensor

-- | Addition
(+:) :: PLExpr -> PLExpr -> PLExpr
(+:) = op2 $ Add AddRatTensor

-- | Multiplication
(*:) :: PLExpr -> PLExpr -> PLExpr
(*:) = op2 $ Mul MulRatTensor

-- | Subtraction
(-:) :: PLExpr -> PLExpr -> PLExpr
(-:) = op2 $ Sub SubRatTensor

-- | Division
(/:) :: PLExpr -> PLExpr -> PLExpr
(/:) = op2 $ Div DivRatTensor

-- | Maximum operator
lmax :: PLExpr -> PLExpr -> PLExpr
lmax = op2 $ Max MaxRatTensor

-- | Minimum operator
lmin :: PLExpr -> PLExpr -> PLExpr
lmin = op2 $ Min MinRatTensor

-- | Power
(^:) :: PLExpr -> Rational -> PLExpr
(^:) x y = builtinFunction PowRat @@ [x, ratLit y]

--------------------------------------------------------------------------------
-- Logics
--------------------------------------------------------------------------------

-- | Template for different avilable differentiable logics
-- part of the syntax translation that differ depending on chosen DL are:
-- logical connectives (not, and, or, implies)
-- comparisons (<, <=, >, >=, =, !=)
type DifferentialLogicDSL = Map BooleanDifferentiableLogicField (Expr Builtin)

mkDSL :: [(BooleanDifferentiableLogicField, PLExpr)] -> DifferentialLogicDSL
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
    [ (Truthity, ratLit (-100000)),
      (Falsity, ratLit 100000),
      (Conjunction, mkOp2 tRat $ \x y -> lmax x y),
      (Disjunction, mkOp2 tRat $ \x y -> lmin x y),
      (Negation, mkOp1 tRat $ \x -> ne x),
      (LessThan, mkOp2 tRat $ \x y -> x -: y),
      (LessEqual, mkOp2 tRat $ \x y -> x -: y),
      (GreaterThan, mkOp2 tRat $ \x y -> y -: x),
      (GreaterEqual, mkOp2 tRat $ \x y -> y -: x),
      (Equal, mkOp2 tRat $ \x y -> ne (lmax (x -: y) (y -: x))),
      (NotEqual, mkOp2 tRat $ \x y -> lmax (x -: y) (y -: x))
    ]

--------------------------------------------------------------------------------
-- DL2

-- | Logic from Fischer, Marc, et al. "Dl2: Training and querying neural
-- networks with logic."  PMLR, 2019.
dl2Translation :: DifferentialLogicDSL
dl2Translation =
  mkDSL
    [ (Truthity, ratLit 0),
      (Falsity, ratLit 1), -- TODO this should be infinity??)
      (Conjunction, mkOp2 tRat $ \x y -> x +: y),
      (Disjunction, mkOp2 tRat $ \x y -> x *: y),
      (Negation, mkOp1 tRat $ \x -> ratLit 1 /: x),
      (LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (Equal, mkOp2 tRat $ \x y -> ne (lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x))),
      (NotEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y) +: lmax (ratLit 0) (y -: x))
    ]

--------------------------------------------------------------------------------
-- Godel

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
godelTranslation :: DifferentialLogicDSL
godelTranslation =
  mkDSL
    [ (Truthity, ratLit 0),
      (Falsity, ratLit 1),
      (Conjunction, mkOp2 tRat $ \x y -> ratLit 1 -: lmin x y),
      (Disjunction, mkOp2 tRat $ \x y -> ratLit 1 -: lmax x y),
      (Negation, mkOp1 tRat $ \x -> ratLit 1 -: x),
      (LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (Equal, unsupported "==" "Godel"),
      (NotEqual, unsupported "!=" "Godel")
    ]

--------------------------------------------------------------------------------
-- Lukasiewicz

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
lukasiewiczTranslation :: DifferentialLogicDSL
lukasiewiczTranslation =
  mkDSL
    [ (Truthity, ratLit 0),
      (Falsity, ratLit 1),
      (Conjunction, mkOp2 tRat $ \x y -> ratLit 1 -: lmax (ratLit 0) ((x +: y) -: ratLit 1)),
      (Disjunction, mkOp2 tRat $ \x y -> ratLit 1 -: lmin (x +: y) (ratLit 1)),
      (Negation, mkOp1 tRat $ \arg -> ratLit 1 -: arg),
      (LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (Equal, unsupported "==" "Lukasiewicz"),
      (NotEqual, unsupported "!=" "Lukasiewicz")
    ]

--------------------------------------------------------------------------------
-- Product

-- | From van Krieken, et al. "Analyzing differentiable fuzzy logic operators."
-- 2022
productTranslation :: DifferentialLogicDSL
productTranslation =
  mkDSL
    [ (Truthity, ratLit 0),
      (Falsity, ratLit 1),
      (Conjunction, mkOp2 tRat $ \x y -> ratLit 1 -: (x *: y)),
      (Disjunction, mkOp2 tRat $ \x y -> (ratLit 1 -: x) *: (ratLit 1 -: y)),
      (Negation, mkOp1 tRat $ \x -> ratLit 1 -: x),
      (LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (Equal, unsupported "==" "Product"),
      (NotEqual, unsupported "!=" "Product")
    ]

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
    [ (Truthity, ratLit 0),
      (Falsity, ratLit 1),
      (Conjunction, andOp),
      (Disjunction, orOp),
      (Negation, mkOp1 tRat (ratLit 1 -:)),
      (LessThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (LessEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (x -: y)),
      (GreaterThan, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (GreaterEqual, mkOp2 tRat $ \x y -> lmax (ratLit 0) (y -: x)),
      (Equal, unsupported "==" "Yager"),
      (NotEqual, unsupported "!=" "Yager")
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
