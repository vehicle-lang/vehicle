
module Vehicle.Core.Compile.DSL
  ( DSL(..)
  , DSLExpr
  , fromDSL
  , type0
  , constraint
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
  , cApp
  , appType
  , piType
  ) where

import Prelude hiding (pi)
import Prettyprinter ((<+>), Pretty(pretty))
import GHC.Stack (HasCallStack)

import Vehicle.Core.AST
import Vehicle.Prelude
import Vehicle.Core.Print.Core ()

class DSL expr where
  infixl 4 `app`
  infixr 4 ~>
  infixr 4 ~~>

  app :: expr -> expr -> expr
  -- lam :: Provenance -> Visibility -> Name -> expr -> (expr -> expr) -> expr
  pi  :: Provenance -> Visibility -> Name -> expr -> (expr -> expr) -> expr

  unnamedPi :: Visibility -> expr -> (expr -> expr) -> expr
  unnamedPi vis = pi mempty vis Machine

  (~>) :: expr -> expr -> expr
  x ~> y = unnamedPi Explicit x (const y)

  (~~>) :: expr -> expr -> expr
  x ~~> y = unnamedPi Implicit x (const y)

  forall :: expr -> (expr -> expr) -> expr
  forall = unnamedPi Implicit

newtype DSLExpr = DSL
  { unDSL :: BindingDepth -> CheckedExpr
  }

fromDSL :: DSLExpr -> CheckedExpr
fromDSL = flip unDSL 0

toDSL :: CheckedExpr -> DSLExpr
toDSL e = DSL $ \i -> liftDBIndices i e

boundVar :: CheckedExpr -> BindingDepth -> DSLExpr
boundVar t i = DSL $ \j -> Var (typeAnn t) (Bound (j - (i + 1)))

instance DSL DSLExpr where
  {-
  lam p v n argType bodyFn = DSL $ \i ->
    let varType = unDSL argType i
        var     = boundVar varType i
        binder  = Binder p v n varType
        body    = unDSL (bodyFn var) (i + 1)
    in Lam (typeAnn $ lamType p v n varType (getType body)) binder body
-}
  pi p v n argType bodyFn = DSL $ \i ->
    let varType = unDSL argType i
        var     = boundVar varType i
        binder  = Binder p v n varType
        body    = unDSL (bodyFn var) (i + 1)
    in Pi (typeAnn $ piType (getType varType) (getType (getType body))) binder body

  app fun arg = DSL $ \i ->
    let fun' = unDSL fun i
        arg' = unDSL arg i
    in App (typeAnn $ appType fun' arg') fun' (Arg (prov arg') Explicit arg')

typeAnn :: CheckedExpr -> CheckedAnn
typeAnn t = RecAnn t mempty

lamType :: Provenance -> Visibility -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
lamType p v n varType bodyType = fromDSL (pi p v n (toDSL varType) (const (toDSL bodyType)))

piType :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
piType t1 t2 = t1 `tMax` t2

appType :: CheckedExpr -> CheckedExpr -> CheckedExpr
appType fun arg = arg `substInto` getFunResultType (getType fun)

-- TODO think whether we need to recurse in the case of an implicit binder
-- TODO the provided type might not be in beta-normal form, so we'd have to reduce it
-- TODO the provided type might contain defined identifiers, which need to be expanded
-- TODO (alternative) make sure this is only called on types in beta-normal form
getFunResultType :: CheckedExpr -> CheckedExpr
getFunResultType (Pi _ann _binder res) = res
getFunResultType t = developerError $ "Expecting a Pi type. Found" <+> pretty t <> "."

cApp :: CheckedExpr -> CheckedExpr -> CheckedExpr
cApp x y = unDSL (toDSL x `app` toDSL y) 0

tMax :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
tMax (Type l1)  (Type l2)  = Type (l1 `max` l2)
tMax (Type l1)  Constraint = Type l1
tMax Constraint (Type l2)  = Type l2
tMax Constraint Constraint = Type 1
tMax t1         t2         = developerError $
  "Expected arguments of type Type. Found" <+> pretty t1 <+> "and" <+> pretty t2 <> "."

con :: Builtin -> DSLExpr -> DSLExpr
con b t = DSL $ \i -> Builtin (typeAnn (unDSL t i)) b

-- * Types

type0 :: DSLExpr
type0 = toDSL Type0

constraint :: DSLExpr
constraint = toDSL Constraint

tBool, tProp, tNat, tInt, tReal :: DSLExpr
tBool = con Bool type0
tProp = con Prop type0
tNat  = con Nat  type0
tInt  = con Int  type0
tReal = con Real type0

tTensor :: DSLExpr -> DSLExpr -> DSLExpr
tTensor tElem dims = con Tensor (type0 ~> tList tNat ~> type0) `app` tElem `app` dims

tList :: DSLExpr -> DSLExpr
tList tElem = con List (type0 ~> type0) `app` tElem

tHole :: Symbol -> DSLExpr
tHole name = toDSL $ Hole mempty name

-- * TypeClass

typeClass :: Provenance -> Builtin -> DSLExpr -> DSLExpr
typeClass p op t = DSL $ \i -> Builtin (RecAnn (unDSL t i) p) op

hasEq :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
hasEq p tArg tRes = typeClass p HasEq (type0 ~> type0 ~> constraint) `app` tArg `app` tRes

hasOrd :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
hasOrd p tArg tRes = typeClass p HasOrd (type0 ~> type0 ~> constraint) `app` tArg `app` tRes

isTruth :: Provenance -> DSLExpr -> DSLExpr
isTruth p t = typeClass p IsTruth (type0 ~> constraint) `app` t

isNatural :: Provenance -> DSLExpr -> DSLExpr
isNatural p t = typeClass p IsNatural (type0 ~> constraint) `app` t

isIntegral :: Provenance -> DSLExpr -> DSLExpr
isIntegral p t = typeClass p IsIntegral (type0 ~> constraint) `app` t

isRational :: Provenance -> DSLExpr -> DSLExpr
isRational p t = typeClass p IsRational (type0 ~> constraint) `app` t

isReal :: Provenance -> DSLExpr -> DSLExpr
isReal p t = typeClass p IsReal (type0 ~> constraint) `app` t

isContainer :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
isContainer p tCont tElem = typeClass p IsContainer (type0 ~> type0 ~> constraint) `app` tCont `app` tElem

isQuantifiable :: Provenance -> DSLExpr -> DSLExpr -> DSLExpr
isQuantifiable p tDom tTruth = typeClass p IsQuantifiable (type0 ~> type0 ~> constraint) `app` tDom `app` tTruth