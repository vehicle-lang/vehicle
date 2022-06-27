module Vehicle.Language.DSL
  ( DSL(..)
  , DSLExpr(..)
  , fromDSL
  , type0
  , tBool
  , tNat
  , tInt
  , tRat
  , tList
  , tTensor
  , tIndex
  , tPol
  , tLin
  , hasEq
  , hasOrd
  , hasAdd
  , hasSub
  , hasMul
  , hasDiv
  , hasNeg
  , hasFold
  , hasQuantifierIn
  , hasNatLitsUpTo
  , hasIntLits
  , hasRatLits
  , hasNot
  , hasAnd
  , hasOr
  , hasImpl
  , hasQuantifier
  , tMax
  , tHole
  , piType
  , nil
  , cons
  , unquantified
  , constant
  ) where

import Prelude hiding (pi)
import Data.List.NonEmpty (NonEmpty)

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Definition

class DSL expr where
  infixl 4 `app`
  infixr 4 ~>
  infixr 4 ~~>
  infixr 4 ~~~>

  app  :: expr -> NonEmpty (Visibility, expr) -> expr
  pi   :: Visibility -> expr -> (expr -> expr) -> expr
  lseq :: expr -> expr -> [expr] -> expr

  eApp :: expr -> NonEmpty expr -> expr
  eApp f args = app f (fmap (Explicit,) args)

  iApp :: expr -> NonEmpty expr -> expr
  iApp f args = app f (fmap (Implicit,) args)

  -- | Explicit function type
  (~>) :: expr -> expr -> expr
  x ~> y = pi Explicit x (const y)

  -- | Implicit function type
  (~~>) :: expr -> expr -> expr
  x ~~> y = pi Implicit x (const y)

  -- | Instance function type
  (~~~>) :: expr -> expr -> expr
  x ~~~> y = pi Instance x (const y)

  forall :: expr -> (expr -> expr) -> expr
  forall = pi Implicit

newtype DSLExpr = DSL
  { unDSL :: Provenance -> BindingDepth -> CheckedExpr
  }

fromDSL :: Provenance -> DSLExpr -> CheckedExpr
fromDSL ann e = unDSL e ann 0

boundVar :: BindingDepth -> DSLExpr
boundVar i = DSL $ \ann j -> Var ann (Bound (j - (i + 1)))

instance DSL DSLExpr where
  pi v argType bodyFn = DSL $ \ann i ->
    let varType = unDSL argType ann i
        var     = boundVar i
        binder  = Binder ann v Nothing varType
        body    = unDSL (bodyFn var) ann (i + 1)
    in Pi ann binder body

  app fun args = DSL $ \ann i ->
    let fun' = unDSL fun ann i
        args' = fmap (\(v, e) -> Arg ann v (unDSL e ann i)) args
    in App ann fun' args'

  lseq tElem tCont args = DSL $ \ann i ->
    SeqExpr ann
      (unDSL tElem ann i)
      (unDSL tCont ann i)
      (fmap (\e -> unDSL e ann i) args)

piType :: CheckedExpr -> CheckedExpr -> CheckedExpr
piType t1 t2 = t1 `tMax` t2

universeLevel :: CheckedExpr -> UniverseLevel
universeLevel (TypeUniverse _ l)   = l
universeLevel (Meta _ _ )          = 0 -- This is probably going to bite us, apologies.
universeLevel (App _ (Meta _ _) _) = 0 -- This is probably going to bite us, apologies.
universeLevel (Pi _ _ r)           = universeLevel r -- This is probably going to bite us, apologies.
universeLevel t                    = developerError $
  "Expected argument of type Type. Found" <+> prettyVerbose t <> "."

tMax :: CheckedExpr -> CheckedExpr -> CheckedExpr
tMax t1 t2  = if universeLevel t1 > universeLevel t2 then t1 else t2

con :: Builtin -> DSLExpr
con b = DSL $ \ann _ -> Builtin ann b

--------------------------------------------------------------------------------
-- Types

universe :: Universe -> DSLExpr
universe u = DSL $ \ann _ -> Universe ann u

type0 :: DSLExpr
type0 = universe $ TypeUniv 0

tPol :: DSLExpr
tPol = universe PolarityUniv

tLin :: DSLExpr
tLin = universe LinearityUniv

tBool :: DSLExpr -> DSLExpr ->  DSLExpr
tBool linearity polarity = con Bool `iApp` [linearity, polarity]

tNat, tInt, tRat :: DSLExpr
tNat  = con (NumericType Nat)
tInt  = con (NumericType Int)
tRat  = con (NumericType Rat)

tTensor :: DSLExpr -> DSLExpr -> DSLExpr
tTensor tElem dims = con (ContainerType Tensor) `eApp` [tElem, dims]

tList :: DSLExpr -> DSLExpr
tList tElem = con (ContainerType List) `eApp` [tElem]

tIndex :: DSLExpr -> DSLExpr
tIndex n = con Index `eApp` [n]

tHole :: Symbol -> DSLExpr
tHole name = DSL $ \ann _ -> Hole ann name

--------------------------------------------------------------------------------
-- TypeClass

typeClass :: TypeClass -> NonEmpty DSLExpr -> DSLExpr
typeClass tc args = con (TypeClass tc) `eApp` args

hasEq :: Equality -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasEq eq t1 t2 t3 = typeClass (HasEq eq) [t1, t2, t3]

hasOrd :: Order -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasOrd ord t1 t2 t3 = typeClass (HasOrd ord) [t1, t2, t3]

hasNot :: DSLExpr -> DSLExpr -> DSLExpr
hasNot t1 t2 = typeClass HasNot [t1, t2]

hasAnd :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasAnd t1 t2 t3 = typeClass HasAnd [t1, t2, t3]

hasOr :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasOr t1 t2 t3 = typeClass HasOr [t1, t2, t3]

hasImpl :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasImpl t1 t2 t3 = typeClass HasImpl [t1, t2, t3]

hasQuantifier :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasQuantifier q t1 t2 t3 = typeClass (HasQuantifier q) [t1, t2, t3]

hasAdd :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasAdd t1 t2 t3 = typeClass HasAdd [t1, t2, t3]

hasSub :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasSub t1 t2 t3 = typeClass HasSub [t1, t2, t3]

hasMul :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasMul t1 t2 t3 = typeClass HasMul [t1, t2, t3]

hasDiv :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasDiv t1 t2 t3 = typeClass HasDiv [t1, t2, t3]

hasNeg :: DSLExpr -> DSLExpr -> DSLExpr
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasFold :: DSLExpr -> DSLExpr -> DSLExpr
hasFold tCont tElem = typeClass HasFold [tCont, tElem]

hasQuantifierIn :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLitsUpTo :: Int -> DSLExpr -> DSLExpr
hasNatLitsUpTo n t = typeClass (HasNatLitsUpTo n) [t]

hasIntLits :: DSLExpr -> DSLExpr
hasIntLits t = typeClass HasIntLits [t]

hasRatLits :: DSLExpr -> DSLExpr
hasRatLits t = typeClass HasRatLits [t]

--------------------------------------------------------------------------------
-- Operations

nil :: DSLExpr -> DSLExpr -> DSLExpr
nil tElem tCont = lseq tElem tCont []

cons :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
cons tElem x xs = app (con Cons) [(Implicit, tElem), (Explicit, x), (Explicit, xs)]

--------------------------------------------------------------------------------
-- Polarities

constant :: DSLExpr
constant = con (Linearity Constant)

unquantified :: DSLExpr
unquantified = con (Polarity Unquantified)
