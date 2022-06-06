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
  , tAux
  , hasEq
  , hasOrd
  , hasAdd
  , hasSub
  , hasMul
  , hasDiv
  , hasNeg
  , hasConOps
  , hasNatLitsUpTo
  , hasIntLits
  , hasRatLits
  , hasConLitsOfSize
  , hasNot
  , hasAndOr
  , hasImpl
  , hasQuantifier
  , tMax
  , tHole
  , piType
  , nil
  , cons
  , unquantified
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
  { unDSL :: CheckedAnn -> BindingDepth -> CheckedExpr
  }

fromDSL :: CheckedAnn -> DSLExpr -> CheckedExpr
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
universeLevel (Type _ l) = l
universeLevel (Meta _ _) = 0 -- This is probably going to bite us, apologies.
universeLevel t          = developerError $
  "Expected argument of type Type. Found" <+> prettyVerbose t <> "."

tMax :: CheckedExpr -> CheckedExpr -> CheckedExpr
tMax t1 t2  = if universeLevel t1 > universeLevel t2 then t1 else t2

con :: Builtin -> DSLExpr
con b = DSL $ \ann _ -> Builtin ann b

--------------------------------------------------------------------------------
-- Types

type0 :: DSLExpr
type0 = DSL $ \ann _ -> Type ann 0

tAux :: DSLExpr
tAux = con AuxiliaryType

tBool :: DSLExpr -> DSLExpr
tBool polarity = con Bool `iApp` [polarity]

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

hasSimpleTC :: TypeClass -> DSLExpr -> DSLExpr
hasSimpleTC tc tArg = con (TypeClass tc) `eApp` [tArg]

hasEq :: DSLExpr -> DSLExpr
hasEq = hasSimpleTC HasEq

hasOrd :: DSLExpr -> DSLExpr
hasOrd = hasSimpleTC HasOrd

hasAdd :: DSLExpr -> DSLExpr
hasAdd = hasSimpleTC HasAdd

hasSub :: DSLExpr -> DSLExpr
hasSub = hasSimpleTC HasSub

hasMul :: DSLExpr -> DSLExpr
hasMul = hasSimpleTC HasMul

hasDiv :: DSLExpr -> DSLExpr
hasDiv = hasSimpleTC HasDiv

hasNeg :: DSLExpr -> DSLExpr
hasNeg = hasSimpleTC HasNeg

hasConOps :: DSLExpr -> DSLExpr -> DSLExpr
hasConOps tCont tElem = con (TypeClass HasConOps) `eApp` [tCont, tElem]

hasNatLitsUpTo :: Int -> DSLExpr -> DSLExpr
hasNatLitsUpTo n t = con (TypeClass (HasNatLitsUpTo n)) `eApp` [t]

hasIntLits :: DSLExpr -> DSLExpr
hasIntLits t = con (TypeClass HasIntLits) `eApp` [t]

hasRatLits :: DSLExpr -> DSLExpr
hasRatLits t = con (TypeClass HasRatLits) `eApp` [t]

hasConLitsOfSize :: Int -> DSLExpr -> DSLExpr -> DSLExpr
hasConLitsOfSize n tCont tElem =
  con (TypeClass (HasConLitsOfSize n)) `eApp` [tCont, tElem]

-- Polarity type-classes
hasNot :: DSLExpr -> DSLExpr -> DSLExpr
hasNot pol1 pol2 = con (PolarityTypeClass HasNot) `eApp` [pol1, pol2]

hasAndOr :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasAndOr pol1 pol2 pol3 = con (PolarityTypeClass HasAndOr) `eApp` [pol1, pol2, pol3]

hasImpl :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasImpl pol1 pol2 pol3 = con (PolarityTypeClass HasImpl) `eApp` [pol1, pol2, pol3]

hasQuantifier :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasQuantifier q t pol1 pol2 = con (PolarityTypeClass $ HasQuantifier q) `eApp` [t, pol1, pol2]

--------------------------------------------------------------------------------
-- Operations

nil :: DSLExpr -> DSLExpr -> DSLExpr
nil tElem tCont = lseq tElem tCont []

cons :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
cons tElem x xs = app (con Cons) [(Implicit, tElem), (Explicit, x), (Explicit, xs)]

--------------------------------------------------------------------------------
-- Polarities

unquantified :: DSLExpr
unquantified = con (Polarity Unquantified)
