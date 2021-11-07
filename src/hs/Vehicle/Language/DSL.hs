{-# LANGUAGE OverloadedLists #-}

module Vehicle.Language.DSL
  ( DSL(..)
  , DSLExpr(..)
  , fromDSL
  , type0
  , tBool
  , tProp
  , tNat
  , tInt
  , tReal
  , tList
  , tTensor
  , hasEq
  , hasOrd
  , isTruth
  , isNatural
  , isIntegral
  , isRational
  , isReal
  , isContainer
  , isQuantifiable
  , tMax
  , tHole
  , piType
  ) where

import Prelude hiding (pi)
import GHC.Stack (HasCallStack)
import Data.List.NonEmpty (NonEmpty)

import Vehicle.Language.AST
import Vehicle.Prelude
import Vehicle.Language.Print (prettyVerbose)

class DSL expr where
  infixl 4 `app`
  infixr 4 ~>
  infixr 4 ~~>
  infixr 4 ~~~>

  app :: expr -> NonEmpty expr -> expr
  -- lam :: Provenance -> Visibility -> Name -> expr -> (expr -> expr) -> expr
  pi  :: Provenance -> Visibility -> Name -> expr -> (expr -> expr) -> expr

  unnamedPi :: Visibility -> expr -> (expr -> expr) -> expr
  unnamedPi v = pi mempty v Machine

  (~>) :: expr -> expr -> expr
  x ~> y = unnamedPi Explicit x (const y)

  (~~>) :: expr -> expr -> expr
  x ~~> y = unnamedPi Implicit x (const y)

  (~~~>) :: expr -> expr -> expr
  x ~~~> y = unnamedPi Instance x (const y)

  forall :: expr -> (expr -> expr) -> expr
  forall = unnamedPi Implicit

newtype DSLExpr = DSL
  { unDSL :: BindingDepth -> CheckedExpr
  }

fromDSL :: DSLExpr -> CheckedExpr
fromDSL = flip unDSL 0

boundVar :: BindingDepth -> DSLExpr
boundVar i = DSL $ \j -> Var mempty (Bound (j - (i + 1)))

instance DSL DSLExpr where
  pi p v n argType bodyFn = DSL $ \i ->
    let varType = unDSL argType i
        var     = boundVar i
        binder  = Binder p TheMachine v n varType
        body    = unDSL (bodyFn var) (i + 1)
    in Pi mempty binder body

  app fun args = DSL $ \i ->
    let fun' = unDSL fun i
        args' = fmap (\e -> ExplicitArg (unDSL e i)) args
    in App mempty fun' args'

--lamType :: Provenance -> Visibility -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
--lamType p v n varType bodyType = fromDSL (pi p v n (toDSL varType) (const (toDSL bodyType)))

piType :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
piType t1 t2 = t1 `tMax` t2

tMax :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
tMax (Type l1)  (Type l2)  = Type (l1 `max` l2)
tMax t1         t2         = developerError $
  "Expected arguments of type Type. Found" <+> prettyVerbose t1 <+> "and" <+> prettyVerbose t2 <> "."

con :: Builtin -> DSLExpr
con b = DSL $ \_ -> Builtin mempty b

-- * Types

type0 :: DSLExpr
type0 = DSL $ const Type0

tBool, tProp, tNat, tInt, tReal :: DSLExpr
tBool = con (BooleanType Bool)
tProp = con (BooleanType Prop)
tNat  = con (NumericType Nat)
tInt  = con (NumericType Int)
tReal = con (NumericType Real)

tTensor :: DSLExpr -> DSLExpr -> DSLExpr
tTensor tElem dims = con Tensor `app` [tElem, dims]

tList :: DSLExpr -> DSLExpr
tList tElem = con List `app` [tElem]

tHole :: Symbol -> DSLExpr
tHole name = DSL $ const $ Hole mempty name

-- * TypeClass

typeClass :: Provenance -> Builtin -> DSLExpr
typeClass p op = DSL $ \_ -> Builtin p op

hasEq :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
hasEq p tArg tRes = typeClass p (TypeClass HasEq) `app` [tArg, tRes]

hasOrd :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
hasOrd p tArg tRes = typeClass p (TypeClass HasOrd) `app` [tArg, tRes]

isTruth :: Provenance -> DSLExpr -> DSLExpr
isTruth p t = typeClass p (TypeClass IsTruth) `app` [t]

isNatural :: Provenance -> DSLExpr -> DSLExpr
isNatural p t = typeClass p (TypeClass IsNatural) `app` [t]

isIntegral :: Provenance -> DSLExpr -> DSLExpr
isIntegral p t = typeClass p (TypeClass IsIntegral) `app` [t]

isRational :: Provenance -> DSLExpr -> DSLExpr
isRational p t = typeClass p (TypeClass IsRational) `app` [t]

isReal :: Provenance -> DSLExpr -> DSLExpr
isReal p t = typeClass p (TypeClass IsReal) `app` [t]

isContainer :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
isContainer p tCont tElem = typeClass p (TypeClass IsContainer) `app` [tCont, tElem]

isQuantifiable :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
isQuantifiable p tDom tTruth = typeClass p (TypeClass IsQuantifiable) `app` [tDom, tTruth]