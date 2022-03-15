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

  app :: expr -> NonEmpty (Visibility, expr) -> expr
  -- lam :: Visibility -> Name -> expr -> (expr -> expr) -> expr
  pi  :: Visibility -> expr -> (expr -> expr) -> expr

  eApp :: expr -> NonEmpty expr -> expr
  eApp f args = app f (fmap (Explicit,) args)

  (~>) :: expr -> expr -> expr
  x ~> y = pi Explicit x (const y)

  (~~>) :: expr -> expr -> expr
  x ~~> y = pi Implicit x (const y)

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
con b = DSL $ \ann _ -> Builtin ann b

-- * Types

type0 :: DSLExpr
type0 = DSL $ const $ const Type0

tBool, tProp, tNat, tInt, tReal :: DSLExpr
tBool = con (BooleanType Bool)
tProp = con (BooleanType Prop)
tNat  = con (NumericType Nat)
tInt  = con (NumericType Int)
tReal = con (NumericType Real)

tTensor :: DSLExpr -> DSLExpr -> DSLExpr
tTensor tElem dims = con (ContainerType Tensor) `eApp` [tElem, dims]

tList :: DSLExpr -> DSLExpr
tList tElem = con (ContainerType List) `eApp` [tElem]

tHole :: Symbol -> DSLExpr
tHole name = DSL $ \ann _ -> Hole ann name

-- * TypeClass

typeClass :: Builtin -> DSLExpr
typeClass op = DSL $ \ann _ -> Builtin ann op

hasEq :: DSLExpr -> DSLExpr -> DSLExpr
hasEq tArg tRes = typeClass (TypeClass HasEq) `eApp` [tArg, tRes]

hasOrd :: DSLExpr -> DSLExpr -> DSLExpr
hasOrd tArg tRes = typeClass (TypeClass HasOrd) `eApp` [tArg, tRes]

isTruth :: DSLExpr -> DSLExpr
isTruth t = typeClass (TypeClass IsTruth) `eApp` [t]

isNatural :: DSLExpr -> DSLExpr
isNatural t = typeClass (TypeClass IsNatural) `eApp` [t]

isIntegral :: DSLExpr -> DSLExpr
isIntegral t = typeClass (TypeClass IsInteger) `eApp` [t]

isRational :: DSLExpr -> DSLExpr
isRational t = typeClass (TypeClass IsRational) `eApp` [t]

isReal :: DSLExpr -> DSLExpr
isReal t = typeClass (TypeClass IsReal) `eApp` [t]

isContainer :: DSLExpr -> DSLExpr -> DSLExpr
isContainer tCont tElem = typeClass (TypeClass IsContainer) `eApp` [tCont, tElem]

isQuantifiable :: DSLExpr -> DSLExpr -> DSLExpr
isQuantifiable tDom tTruth = typeClass (TypeClass IsQuantifiable) `eApp` [tDom, tTruth]