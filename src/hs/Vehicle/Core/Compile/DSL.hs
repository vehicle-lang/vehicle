
module Vehicle.Core.Compile.DSL where

import Vehicle.Core.AST
import Vehicle.Prelude

makeTypeAnn :: CheckedExpr -> CheckedAnn
makeTypeAnn t = RecAnn t mempty

getFunResultType :: CheckedExpr -> CheckedExpr
getFunResultType (Pi _ann _binder res) = res
getFunResultType _                     = error "expecting a Pi type"

-- * DSL for writing kinds as info annotations

tPi :: Provenance -> Visibility -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
tPi p vis name a b =
  let aType = getType a
      bType = getType b
      abType = aType `tMax` bType
  in  Pi (makeTypeAnn abType) (Binder p vis name a) b

tPiInternal :: Visibility -> CheckedExpr -> CheckedExpr -> CheckedExpr
tPiInternal vis = tPi mempty vis Machine

-- TODO figure out how to do this without horrible -1 hacks
tForall
  :: CheckedExpr
  -> (CheckedExpr -> CheckedExpr)
  -> CheckedExpr
tForall k f = quantBody
  where
    badBody   = f (Bound (RecAnn Type0 mempty) (Index (-1)))
    body      = liftAcc (-1) badBody
    quantBody = tPiInternal Implicit k body

(~>) :: CheckedExpr -> CheckedExpr -> CheckedExpr
x ~> y = tPiInternal Explicit x y

(~~>) :: CheckedExpr -> CheckedExpr -> CheckedExpr
x ~~> y = tPiInternal Implicit x y

con :: Builtin -> CheckedExpr -> CheckedExpr
con b t = Builtin (makeTypeAnn t) b

app :: CheckedExpr -> CheckedExpr -> CheckedExpr
app fun arg = App (makeTypeAnn (getFunResultType fun)) fun (Arg mempty Explicit arg)

hole :: Symbol -> CheckedExpr
hole = Hole mempty

-- * Types

tMax :: CheckedExpr -> CheckedExpr -> CheckedExpr
tMax (Type l1) (Type l2) = Type (l1 `max` l2)
tMax _         _         = error "Expected something of type Type. Found of type"

tPrim :: PrimitiveType -> CheckedExpr
tPrim t = con (PrimitiveType t) Type0

tPrimNumber :: PrimitiveNumberType -> CheckedExpr
tPrimNumber = tPrim . TNumber

tPrimTruth :: PrimitiveTruthType -> CheckedExpr
tPrimTruth = tPrim . TTruth

tBool, tProp, tNat, tInt, tReal :: CheckedExpr
tBool    = tPrimTruth  TBool
tProp    = tPrimTruth  TProp
tNat     = tPrimNumber TNat
tInt     = tPrimNumber TInt
tReal    = tPrimNumber TReal

tTensor :: CheckedExpr -> CheckedExpr -> CheckedExpr
tTensor tElem dims = con Tensor (Type0 ~> tList tNat ~> Type0) `app` tElem `app` dims

tList :: CheckedExpr -> CheckedExpr
tList tElem = con List (Type0 ~> Type0) `app` tElem


-- * Constraints

constraint :: Provenance -> Constraint -> CheckedExpr
constraint p c = Builtin (RecAnn Constraint p) (Implements c)

hasEq :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
hasEq p t1 t2 = constraint p HasEq `app` t1 `app` t2

hasOrd :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
hasOrd p t1 t2 = constraint p HasOrd `app` t1 `app` t2

isTruth :: Provenance -> CheckedExpr -> CheckedExpr
isTruth p t = constraint p IsTruth `app` t

isNatural :: Provenance -> CheckedExpr -> CheckedExpr
isNatural p t = constraint p IsNatural `app` t

isIntegral :: Provenance -> CheckedExpr -> CheckedExpr
isIntegral p t = constraint p IsIntegral `app` t

isRational :: Provenance -> CheckedExpr -> CheckedExpr
isRational p t = constraint p IsRational `app` t

isReal :: Provenance -> CheckedExpr -> CheckedExpr
isReal p t = constraint p IsReal `app` t

isContainer :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isContainer p tCont tElem = constraint p IsContainer `app` tCont `app` tElem

isQuantifiable :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isQuantifiable p tDom tTruth = constraint p IsQuantifiable `app` tDom `app` tTruth

-- * Expressions

list :: [CheckedExpr] -> CheckedExpr -> CheckedExpr
list es tElem =
  tForall Type0 $ \tCont ->
    isContainer mempty tCont tElem ~> Seq (makeTypeAnn tCont) es