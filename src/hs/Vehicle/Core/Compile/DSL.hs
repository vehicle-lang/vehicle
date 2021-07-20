
module Vehicle.Core.Compile.DSL where

import Vehicle.Core.AST
import Vehicle.Prelude

makeTypeAnn :: CheckedExpr -> CheckedAnn
makeTypeAnn t = RecAnn t mempty

getFunResultType :: CheckedExpr -> CheckedExpr
getFunResultType (Pi _ann _binder res) = res
getFunResultType _                     = error "expecting a Pi type"

-- * DSL for writing kinds as info annotations

tPi :: Visibility -> CheckedExpr -> CheckedExpr -> CheckedExpr
tPi vis a b =
  let aType = getType a
      bType = getType b
      abType = aType `tMax` bType
  in  Pi (makeTypeAnn abType) (Binder (makeTypeAnn aType) vis Machine a) b

-- TODO figure out how to do this without horrible -1 hacks
tForall
  :: CheckedExpr
  -> (CheckedExpr -> CheckedExpr)
  -> CheckedExpr
tForall k f = quantBody
  where
    badBody   = f (Bound (RecAnn Type0 mempty) (Index (-1)))
    body      = liftAcc (-1) badBody
    quantBody = tPi Implicit k body

(~>) :: CheckedExpr -> CheckedExpr -> CheckedExpr
x ~> y = tPi Explicit x y

con :: Builtin -> CheckedExpr -> CheckedExpr
con b t = Builtin (makeTypeAnn t) b

app :: CheckedExpr -> CheckedExpr -> CheckedExpr
app fun arg = App (makeTypeAnn (getFunResultType fun)) fun (Arg Explicit arg)

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

isNumber :: Provenance -> CheckedExpr -> CheckedExpr
isNumber p t = constraint p IsNumber `app` t

isContainer :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isContainer p tCont tElem = constraint p IsContainer `app` tCont `app` tElem

isQuantifiable :: Provenance -> CheckedExpr -> CheckedExpr -> CheckedExpr
isQuantifiable p tDom tTruth = constraint p IsQuantifiable `app` tDom `app` tTruth

-- * Expressions

list :: [CheckedExpr] -> CheckedExpr -> CheckedExpr
list es tElem =
  tForall Type0 $ \tCont ->
    isContainer mempty tCont tElem ~> Seq (makeTypeAnn tCont) es

-- * Builtins

-- |Return the kind for builtin exprs.
typeOf :: Provenance -> Builtin -> CheckedExpr
typeOf p = \case
  PrimitiveType _ -> Type0
  List            -> Type0 ~> Type0
  Tensor          -> Type0 ~> tList tNat ~> Type0

  Implements HasEq          -> Type0 ~> Type0 ~> Constraint
  Implements HasOrd         -> Type0 ~> Type0 ~> Constraint
  Implements IsTruth        -> Type0 ~> Constraint
  Implements IsNumber       -> Type0 ~> Constraint
  Implements IsContainer    -> Type0 ~> Constraint
  Implements IsQuantifiable -> Type0 ~> Type0 ~> Constraint

  If   -> tForall Type0 $ \t -> tProp ~> t ~> t
  Cons -> tForall Type0 $ \t -> t ~> tList t ~> tList t

  Impl -> typeOfBoolOp2 p
  And  -> typeOfBoolOp2 p
  Or   -> typeOfBoolOp2 p
  Not  -> typeOfBoolOp1 p

  Eq   -> typeOfEqualityOp p
  Neq  -> typeOfEqualityOp p

  Le   -> typeOfComparisonOp p
  Lt   -> typeOfComparisonOp p
  Ge   -> typeOfComparisonOp p
  Gt   -> typeOfComparisonOp p

  Add  -> typeOfNumOp2 p
  Sub  -> typeOfNumOp2 p
  Mul  -> typeOfNumOp2 p
  Div  -> typeOfNumOp2 p
  Neg  -> typeOfNumOp1 p

  At   -> typeOfAtOp p

  All  -> typeOfQuantifierOp p
  Any  -> typeOfQuantifierOp p

typeOfEqualityOp :: Provenance -> CheckedExpr
typeOfEqualityOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasEq p t r ~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> CheckedExpr
typeOfComparisonOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasOrd p t r ~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> CheckedExpr
typeOfBoolOp2 p =
  tForall Type0 $ \t ->
    isTruth p t ~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> CheckedExpr
typeOfBoolOp1 p =
  tForall Type0 $ \t ->
    isTruth p t ~> t ~> t

typeOfNumOp2 :: Provenance -> CheckedExpr
typeOfNumOp2 p =
  tForall Type0 $ \t ->
    isNumber p t ~> t ~> t ~> t

typeOfNumOp1 :: Provenance -> CheckedExpr
typeOfNumOp1 p =
  tForall Type0 $ \t ->
    isNumber p t ~> t ~> t

typeOfQuantifierOp :: Provenance -> CheckedExpr
typeOfQuantifierOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      isQuantifiable p t r ~> t ~> (t ~> r) ~> r

typeOfAtOp :: Provenance -> CheckedExpr
typeOfAtOp p =
  tForall Type0 $ \tCont ->
    tForall Type0 $ \tElem ->
      isContainer p tCont tElem ~> tCont ~> tNat ~> tElem