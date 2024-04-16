module Vehicle.Data.BuiltinInterface.DSL where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.DSL
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Types DSL

builtinType :: (HasStandardTypes builtin) => BuiltinType -> DSLExpr builtin
builtinType = builtin . mkBuiltinType

tUnit :: (HasStandardTypes builtin) => DSLExpr builtin
tUnit = builtinType Unit

tBool, tNat, tRat :: (HasStandardTypes builtin) => DSLExpr builtin
tBool = builtinType Bool
tNat = builtinType Nat
tRat = builtinType Rat

tVector :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
tVector tElem dim = builtinType Vector @@ [tElem] .@@ [dim]

tVectorFunctor :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tVectorFunctor n = explLam "A" type0 (`tVector` n)

tListRaw :: (HasStandardTypes builtin) => DSLExpr builtin
tListRaw = builtinType List

tList :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tList tElem = tListRaw @@ [tElem]

tIndex :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tIndex n = builtinType Index .@@ [n]

forAllNat :: (HasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllNat = forAll "n" tNat

forAllIrrelevantNat :: (HasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllIrrelevantNat name = pi (Just name) (Implicit False) Irrelevant tNat

irrelImplNatLam :: (HasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
irrelImplNatLam n = lam n (Implicit False) Irrelevant tNat

natInDomainConstraint :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
natInDomainConstraint n t = builtin mkNatInDomainConstraint @@ [n, t]

--------------------------------------------------------------------------------
-- Constructors DSL

builtinConstructor :: (HasStandardData builtin) => BuiltinConstructor -> DSLExpr builtin
builtinConstructor = builtin . mkBuiltinConstructor

nil :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin
nil tElem = builtinConstructor Nil @@@ [tElem]

cons :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
cons tElem x xs = builtinConstructor Cons @@@ [tElem] @@ [x, xs]

natLit :: (HasStandardData builtin) => Int -> DSLExpr builtin
natLit n = builtinConstructor (LNat n)

boolLit :: (HasStandardData builtin) => Bool -> DSLExpr builtin
boolLit n = builtinConstructor (LBool n)

ratLit :: (HasStandardData builtin) => Rational -> DSLExpr builtin
ratLit r = builtinConstructor (LRat r)

unitLit :: (HasStandardData builtin) => DSLExpr builtin
unitLit = builtinConstructor LUnit

--------------------------------------------------------------------------------
-- Functions DSL

builtinFunction :: (HasStandardData builtin) => BuiltinFunction -> DSLExpr builtin
builtinFunction = builtin . mkBuiltinFunction

addNat :: (HasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
addNat x y = builtinFunction (Add AddNat) @@ [x, y]

ite ::
  (HasStandardData builtin) =>
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin ->
  DSLExpr builtin
ite t c e1 e2 = builtinFunction If @@@ [t] @@ [c, e1, e2]

--------------------------------------------------------------------------------
-- Type classes

builtinTypeClass :: (HasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin
builtinTypeClass = builtin . mkBuiltinTypeClass

typeClass :: (HasStandardTypeClasses builtin) => TypeClass -> NonEmpty (DSLExpr builtin) -> DSLExpr builtin
typeClass tc args = builtinTypeClass tc @@ args

hasEq :: (HasStandardTypeClasses builtin) => EqualityOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasEq eq t1 t2 = typeClass (HasEq eq) [t1, t2]

hasOrd :: (HasStandardTypeClasses builtin) => OrderOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasOrd ord t1 t2 = typeClass (HasOrd ord) [t1, t2]

hasQuantifier :: (HasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin
hasQuantifier q t = typeClass (HasQuantifier q) [t]

numOp2TypeClass :: (HasStandardTypeClasses builtin) => TypeClass -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
numOp2TypeClass tc t1 t2 t3 = typeClass tc [t1, t2, t3]

hasAdd :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasAdd = numOp2TypeClass HasAdd

hasSub :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasSub = numOp2TypeClass HasSub

hasMul :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasMul = numOp2TypeClass HasMul

hasDiv :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasDiv = numOp2TypeClass HasDiv

hasNeg :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasMap tCont = typeClass HasMap [tCont]

hasFold :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasFold tCont = typeClass HasFold [tCont]

hasQuantifierIn :: (HasStandardTypeClasses builtin) => Quantifier -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasNatLits t = typeClass HasNatLits [t]

hasRatLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: (HasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasVecLits n d = typeClass HasVecLits [n, d]
