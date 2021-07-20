{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.AST.DSL where

import Data.Sequence (Seq)

import Vehicle.Core.AST.Builtin
import Vehicle.Core.AST.Core
import Vehicle.Core.AST.DeBruijn
import Vehicle.Core.AST.Utils (RecAnn(..), annotation)
import Vehicle.Prelude



type TypedAnn  = DeBruijnAnn Provenance
type TypedExpr = DeBruijnExpr TypedAnn

makeTypeAnn :: TypedExpr -> TypedAnn
makeTypeAnn t = RecAnn t mempty

getFunResultType :: TypedExpr -> TypedExpr
getFunResultType (Pi _ann _binder res) = res
getFunResultType _                     = error "expecting a Pi type"

-- * DSL for writing kinds as info annotations

con :: Builtin -> TypedExpr -> TypedExpr
con b t = Builtin (makeTypeAnn t) b

(~>) :: TypedExpr -> TypedExpr -> TypedExpr
x ~> y = Pi (makeTypeAnn kType) (Binder (makeTypeAnn x) Explicit Machine x) y

app :: TypedExpr -> TypedExpr -> TypedExpr
app fun arg = App (makeTypeAnn (getFunResultType fun)) fun (Arg Explicit arg)

-- * Kinds

kType :: TypedExpr
kType = con Type Kind

kConstraint :: TypedExpr
kConstraint = con Constraint Kind

-- * Types

tUpperBound :: TypedExpr -> TypedExpr -> TypedExpr
tUpperBound e1 e2 = case annotation e1 of
  Kind -> _
  _ -> _

tPrim :: PrimitiveType -> TypedExpr
tPrim t = con (PrimitiveType t) kType

tPrimNumber :: PrimitiveNumberType -> TypedExpr
tPrimNumber = tPrim . TNumber

tPrimTruth :: PrimitiveTruthType -> TypedExpr
tPrimTruth = tPrim . TTruth

tBool, tProp, tNat, tInt, tReal :: TypedExpr
tBool    = tPrimTruth  TBool
tProp    = tPrimTruth  TProp
tNat     = tPrimNumber TNat
tInt     = tPrimNumber TInt
tReal    = tPrimNumber TReal

tTensor :: TypedExpr -> TypedExpr -> TypedExpr
tTensor tElem tDim = con Tensor kType `app` tDim `app` tElem

tList :: TypedExpr -> TypedExpr
tList tElem = con List (typeOf mempty List) `app` tElem

-- TODO figure out how to do this without horrible -1 hacks
tForall
  :: TypedExpr
  -> (TypedExpr -> TypedExpr)
  -> TypedExpr
tForall k f = quantBody
  where
    badBody   = f (Bound (RecAnn kType mempty) (Index (-1)))
    body      = liftAcc (-1) badBody
    quantBody = Pi (RecAnn kType mempty) (Binder (makeTypeAnn k) Implicit Machine k) body

-- * Constraints

constraint :: Provenance -> Constraint -> TypedExpr
constraint p c = Builtin (RecAnn kConstraint p) (Implements c)

hasEq :: Provenance -> TypedExpr -> TypedExpr -> TypedExpr
hasEq p t1 t2 = constraint p HasEq `app` t1 `app` t2

hasOrd :: Provenance -> TypedExpr -> TypedExpr -> TypedExpr
hasOrd p t1 t2 = constraint p HasOrd `app` t1 `app` t2

isTruth :: Provenance -> TypedExpr -> TypedExpr
isTruth p t = constraint p IsTruth `app` t

isNumber :: Provenance -> TypedExpr -> TypedExpr
isNumber p t = constraint p IsNumber `app` t

isContainer :: Provenance -> TypedExpr -> TypedExpr -> TypedExpr
isContainer p tCont tElem = constraint p IsContainer `app` tCont `app` tElem

isQuantifiable :: Provenance -> TypedExpr -> TypedExpr -> TypedExpr
isQuantifiable p tDom tTruth = constraint p IsQuantifiable `app` tDom `app` tTruth

-- * Expressions

list :: Seq TypedExpr -> TypedExpr -> TypedExpr
list es tElem =
  tForall kType $ \tCont ->
    isContainer mempty tCont tElem ~> Seq (makeTypeAnn tCont) es

-- * Builtins

-- |Return the kind for builtin exprs.
typeOf :: Provenance -> Builtin -> TypedExpr
typeOf p = \case
  Type            -> Kind
  Constraint      -> Kind

  PrimitiveType _ -> kType
  List            -> kType ~> kType
  Tensor          -> kType ~> tList tNat ~> kType

  Implements HasEq          -> kType ~> kType ~> kConstraint
  Implements HasOrd         -> kType ~> kType ~> kConstraint
  Implements IsTruth        -> kType ~> kConstraint
  Implements IsNumber       -> kType ~> kConstraint
  Implements IsContainer    -> kType ~> kConstraint
  Implements IsQuantifiable -> kType ~> kType ~> kConstraint

  If   -> tForall kType $ \t -> tProp ~> t ~> t
  Cons -> tForall kType $ \t -> t ~> tList t ~> tList t

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

typeOfEqualityOp :: Provenance -> TypedExpr
typeOfEqualityOp p =
  tForall kType $ \t ->
    tForall kType $ \r ->
      hasEq p t r ~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> TypedExpr
typeOfComparisonOp p =
  tForall kType $ \t ->
    tForall kType $ \r ->
      hasOrd p t r ~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> TypedExpr
typeOfBoolOp2 p =
  tForall kType $ \t ->
    isTruth p t ~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> TypedExpr
typeOfBoolOp1 p =
  tForall kType $ \t ->
    isTruth p t ~> t ~> t

typeOfNumOp2 :: Provenance -> TypedExpr
typeOfNumOp2 p =
  tForall kType $ \t ->
    isNumber p t ~> t ~> t ~> t

typeOfNumOp1 :: Provenance -> TypedExpr
typeOfNumOp1 p =
  tForall kType $ \t ->
    isNumber p t ~> t ~> t

typeOfQuantifierOp :: Provenance -> TypedExpr
typeOfQuantifierOp p =
  tForall kType $ \t ->
    tForall kType $ \r ->
      isQuantifiable p t r ~> t ~> (t ~> r) ~> r

typeOfAtOp :: Provenance -> TypedExpr
typeOfAtOp p =
  tForall kType $ \tCont ->
    tForall kType $ \tElem ->
      isContainer p tCont tElem ~> tCont ~> tNat ~> tElem