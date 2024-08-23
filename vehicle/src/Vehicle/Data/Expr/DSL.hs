module Vehicle.Data.Expr.DSL where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.DSL
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Types DSL

builtinType :: (BuiltinHasStandardTypes builtin) => BuiltinType -> DSLExpr builtin
builtinType = builtin . mkBuiltinType

tUnit :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tUnit = builtinType Unit

tBool, tNat, tRat :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tBool = builtinType Bool
tNat = builtinType Nat
tRat = builtinType Rat

tVector :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
tVector tElem dim = builtinType Vector @@ [tElem] .@@ [dim]

tVectorFunctor :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tVectorFunctor n = explLam "A" type0 (`tVector` n)

tListRaw :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tListRaw = builtinType List

tList :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tList tElem = tListRaw @@ [tElem]

tIndex :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tIndex n = builtinType Index .@@ [n]

forAllNat :: (BuiltinHasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllNat = forAll "n" tNat

forAllIrrelevantNat :: (BuiltinHasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllIrrelevantNat name = pi (Just name) (Implicit False) Irrelevant tNat

irrelImplNatLam :: (BuiltinHasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
irrelImplNatLam n = lam n (Implicit False) Irrelevant tNat

natInDomainConstraint :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
natInDomainConstraint n t = builtin mkNatInDomainConstraint @@ [n, t]

--------------------------------------------------------------------------------
-- Constructors DSL

builtinConstructor :: (BuiltinHasStandardData builtin) => BuiltinConstructor -> DSLExpr builtin
builtinConstructor = builtin . mkBuiltinConstructor

nil :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin
nil tElem = builtinConstructor Nil @@@ [tElem]

cons :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
cons tElem x xs = builtinConstructor Cons @@@ [tElem] @@ [x, xs]

natLit :: (BuiltinHasStandardData builtin) => Int -> DSLExpr builtin
natLit n = builtinConstructor (LNat n)

boolLit :: (BuiltinHasStandardData builtin) => Bool -> DSLExpr builtin
boolLit n = builtinConstructor (LBool n)

ratLit :: (BuiltinHasStandardData builtin) => Rational -> DSLExpr builtin
ratLit r = builtinConstructor (LRat r)

unitLit :: (BuiltinHasStandardData builtin) => DSLExpr builtin
unitLit = builtinConstructor LUnit

--------------------------------------------------------------------------------
-- Functions DSL

builtinFunction :: (BuiltinHasStandardData builtin) => BuiltinFunction -> DSLExpr builtin
builtinFunction = builtin . mkBuiltinFunction

addNat :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
addNat x y = builtinFunction (Add AddNat) @@ [x, y]

ite ::
  (BuiltinHasStandardData builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin
ite t c e1 e2 = builtinFunction If @@@ [t] @@ [c, e1, e2]

--------------------------------------------------------------------------------
-- Type classes

builtinTypeClass :: (BuiltinHasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin
builtinTypeClass = builtin . mkBuiltinTypeClass

typeClass :: (BuiltinHasStandardTypeClasses builtin) => TypeClass -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
typeClass tc args = builtinTypeClass tc @@ args

hasEq :: (BuiltinHasStandardTypeClasses builtin) => EqualityOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasEq eq t1 t2 = typeClass (HasEq eq) [t1, t2]

hasOrd :: (BuiltinHasStandardTypeClasses builtin) => OrderOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasOrd ord t1 t2 = typeClass (HasOrd ord) [t1, t2]

hasQuantifier :: (BuiltinHasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin
hasQuantifier q t = typeClass (HasQuantifier q) [t]

numOp2TypeClass :: (BuiltinHasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
numOp2TypeClass tc t1 t2 t3 = typeClass tc [t1, t2, t3]

hasAdd :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasAdd = numOp2TypeClass HasAdd

hasSub :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasSub = numOp2TypeClass HasSub

hasMul :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasMul = numOp2TypeClass HasMul

hasDiv :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasDiv = numOp2TypeClass HasDiv

hasNeg :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasMap tCont = typeClass HasMap [tCont]

hasFold :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasFold tCont = typeClass HasFold [tCont]

hasQuantifierIn :: (BuiltinHasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasNatLits t = typeClass HasNatLits [t]

hasRatLits :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasVecLits n d = typeClass HasVecLits [n, d]
