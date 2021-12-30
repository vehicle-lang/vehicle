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

import Vehicle.Language.Print (prettyVerbose)
import Vehicle.Compile.Prelude

class DSL expr where
  infixl 4 `app`
  infixr 4 ~>
  infixr 4 ~~>
  infixr 4 ~~~>

  app :: expr -> NonEmpty expr -> expr
  -- lam :: Provenance -> Visibility -> Name -> expr -> (expr -> expr) -> expr
  pi  :: Provenance -> Visibility -> expr -> (expr -> expr) -> expr

  unnamedPi :: Visibility -> expr -> (expr -> expr) -> expr
  unnamedPi = pi mempty

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
boundVar i = DSL $ \j -> Var emptyMachineAnn (Bound (j - (i + 1)))

instance DSL DSLExpr where
  pi p v argType bodyFn = DSL $ \i ->
    let varType = unDSL argType i
        var     = boundVar i
        binder  = Binder (p, TheMachine) v Nothing varType
        body    = unDSL (bodyFn var) (i + 1)
    in Pi emptyMachineAnn binder body

  app fun args = DSL $ \i ->
    let fun' = unDSL fun i
        args' = fmap (\e -> ExplicitArg emptyMachineAnn (unDSL e i)) args
    in App emptyMachineAnn fun' args'

--lamType :: Provenance -> Visibility -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
--lamType p v n varType bodyType = fromDSL (pi p v n (toDSL varType) (const (toDSL bodyType)))

piType :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
piType t1 t2 = t1 `tMax` t2

universeLevel :: CheckedExpr -> UniverseLevel
universeLevel (Type l)   = l
universeLevel (Meta _ _) = 0 -- This is probably going to bite us, apologies.
universeLevel t          = developerError $
  "Expected argument of type Type. Found" <+> prettyVerbose t <> "."

tMax :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
tMax t1 t2  = Type (universeLevel t1 `max` universeLevel t2)

con :: Builtin -> DSLExpr
con b = DSL $ \_ -> Builtin emptyMachineAnn b

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
tTensor tElem dims = con (ContainerType Tensor) `app` [tElem, dims]

tList :: DSLExpr -> DSLExpr
tList tElem = con (ContainerType List) `app` [tElem]

tHole :: Symbol -> DSLExpr
tHole name = DSL $ const $ Hole mempty name

-- * TypeClass

typeClass :: CheckedAnn -> Builtin -> DSLExpr
typeClass p op = DSL $ \_ -> Builtin p op

hasEq :: CheckedAnn -> DSLExpr -> DSLExpr -> DSLExpr
hasEq p tArg tRes = typeClass p (TypeClass HasEq) `app` [tArg, tRes]

hasOrd :: CheckedAnn -> DSLExpr -> DSLExpr -> DSLExpr
hasOrd p tArg tRes = typeClass p (TypeClass HasOrd) `app` [tArg, tRes]

isTruth :: CheckedAnn -> DSLExpr -> DSLExpr
isTruth p t = typeClass p (TypeClass IsTruth) `app` [t]

isNatural :: CheckedAnn -> DSLExpr -> DSLExpr
isNatural p t = typeClass p (TypeClass IsNatural) `app` [t]

isIntegral :: CheckedAnn -> DSLExpr -> DSLExpr
isIntegral p t = typeClass p (TypeClass IsIntegral) `app` [t]

isRational :: CheckedAnn -> DSLExpr -> DSLExpr
isRational p t = typeClass p (TypeClass IsRational) `app` [t]

isReal :: CheckedAnn -> DSLExpr -> DSLExpr
isReal p t = typeClass p (TypeClass IsReal) `app` [t]

isContainer :: CheckedAnn -> DSLExpr -> DSLExpr -> DSLExpr
isContainer p tCont tElem = typeClass p (TypeClass IsContainer) `app` [tCont, tElem]

isQuantifiable :: CheckedAnn -> DSLExpr -> DSLExpr -> DSLExpr
isQuantifiable p tDom tTruth = typeClass p (TypeClass IsQuantifiable) `app` [tDom, tTruth]