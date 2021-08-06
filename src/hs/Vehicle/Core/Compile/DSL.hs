
module Vehicle.Core.Compile.DSL where

import Prettyprinter ((<+>), Pretty(pretty))
import Debug.Trace (trace)
import GHC.Stack (HasCallStack)

import Vehicle.Core.AST
import Vehicle.Prelude
import Vehicle.Core.Print.Core ()
import Vehicle.Core.Print.Frontend (prettyFrontend)

makeTypeAnn :: CheckedExpr -> CheckedAnn
makeTypeAnn t = RecAnn t mempty

-- TODO think whether we need to recurse in the case of an implicit binder
getFunResultType :: CheckedExpr -> CheckedExpr
getFunResultType (Pi _ann _binder res) = res
getFunResultType t = developerError $ "Expecting a Pi type. Found" <+> pretty t <> "."

-- * Kinds

tMax :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
tMax (Type l1)  (Type l2)  = Type (l1 `max` l2)
tMax (Type l1)  Constraint = Type l1
tMax Constraint (Type l2)  = Type l2
tMax t1         t2         = developerError $
  "Expected arguments of type Type. Found" <+> pretty t1 <+> "and" <+> pretty t2 <> "."

-- * DSL for writing kinds as info annotations

tPi :: HasCallStack => Provenance -> Visibility -> Name -> CheckedExpr -> CheckedExpr -> CheckedExpr
tPi p vis name a b =
  let aType  = getType a
      bType  = getType b
      abType = aType `tMax` bType
  in  Pi (makeTypeAnn abType) (Binder p vis name a) b

tPiInternal :: HasCallStack => Visibility -> CheckedExpr -> CheckedExpr -> CheckedExpr
tPiInternal vis = tPi mempty vis Machine

-- TODO figure out how to do this without horrible -1 hacks
tForall
  :: CheckedExpr
  -> (CheckedExpr -> CheckedExpr)
  -> CheckedExpr
tForall k f = quantBody
  where
    badBody   = f (Var (RecAnn Type0 mempty) (Bound (-1)))
    body      = cleanDBIndices (-1) badBody
    quantBody = tPiInternal Implicit k body

infixr 4 ~>
(~>) :: CheckedExpr -> CheckedExpr -> CheckedExpr
x ~> y = tPiInternal Explicit x y

infixr 4 ~~>
(~~>) :: HasCallStack => CheckedExpr -> CheckedExpr -> CheckedExpr
x ~~> y = tPiInternal Implicit x y

con :: Builtin -> CheckedExpr -> CheckedExpr
con b t = Builtin (makeTypeAnn t) b

infixl 4 `app`
app :: CheckedExpr -> CheckedExpr -> CheckedExpr
app fun arg = App (makeTypeAnn (getFunResultType (getType fun))) fun (Arg mempty Explicit arg)

hole :: Symbol -> CheckedExpr
hole = Hole mempty

-- * Types

tBool, tProp, tNat, tInt, tReal :: CheckedExpr
tBool    = con Bool Type0
tProp    = con Prop Type0
tNat     = con Nat  Type0
tInt     = con Int  Type0
tReal    = con Real Type0

tTensor :: CheckedExpr -> CheckedExpr -> CheckedExpr
tTensor tElem dims = con Tensor (Type0 ~> tList tNat ~> Type0) `app` tElem `app` dims

tList :: CheckedExpr -> CheckedExpr
tList tElem = con List (Type0 ~> Type0) `app` tElem

-- * TypeClass

typeClass :: Provenance -> Builtin -> CheckedExpr -> CheckedExpr
typeClass p op t = Builtin (RecAnn t p) op

hasEq :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
hasEq p tArg tRes = typeClass p HasEq (Type0 ~> Type0 ~> Constraint) `app` tArg `app` tRes

hasOrd :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
hasOrd p tArg tRes = typeClass p HasOrd (Type0 ~> Type0 ~> Constraint) `app` tArg `app` tRes

isTruth :: Provenance -> CheckedExpr -> CheckedExpr
isTruth p t = typeClass p IsTruth (Type0 ~> Constraint) `app` t

isNatural :: Provenance -> CheckedExpr -> CheckedExpr
isNatural p t = typeClass p IsNatural (Type0 ~> Constraint) `app` t

isIntegral :: Provenance -> CheckedExpr -> CheckedExpr
isIntegral p t = typeClass p IsIntegral (Type0 ~> Constraint) `app` t

isRational :: Provenance -> CheckedExpr -> CheckedExpr
isRational p t = typeClass p IsRational (Type0 ~> Constraint) `app` t

isReal :: Provenance -> CheckedExpr -> CheckedExpr
isReal p t = typeClass p IsReal (Type0 ~> Constraint) `app` t

isContainer :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isContainer p tCont tElem = typeClass p IsContainer (Type0 ~> Type0 ~> Constraint) `app` tCont `app` tElem

isQuantifiable :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isQuantifiable p tDom tTruth = typeClass p IsQuantifiable (Type0 ~> Type0 ~> Constraint) `app` tDom `app` tTruth

-- * Expressions

list :: [CheckedExpr] -> CheckedExpr -> CheckedExpr
list es tElem =
  tForall Type0 $ \tCont ->
    isContainer mempty tCont tElem ~> Seq (makeTypeAnn tCont) es