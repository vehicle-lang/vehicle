module Vehicle.Data.Code.DSL where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.DSL
import Vehicle.Data.Tensor as T (Tensor, shapeOf, pattern ZeroDimTensor)
import Vehicle.Prelude
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Standard types DSL

builtinType :: (BuiltinHasStandardTypes builtin) => BuiltinType -> DSLExpr builtin
builtinType = builtin . mkExpr accessBuiltinType

tUnit :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tUnit = builtinType UnitType

tBool, tNat, tRat :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tNat = builtinType NatType
tBool = builtinType BoolType
tRat = builtinType RatType

tTensorRaw :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tTensorRaw = builtinType TensorType

tTensor :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
tTensor tElem ds = tTensorRaw @@ [tElem] .@@ [ds]

tVector :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
tVector tElem d = builtinType VectorType @@ [tElem] .@@ [d]

tVectorRaw :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tVectorRaw = builtinType VectorType

tListRaw :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tListRaw = builtinType ListType

tList :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tList tElem = tListRaw @@ [tElem]

tIndex :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tIndex n = builtinType IndexType .@@ [n]

tBoolTensor :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tBoolTensor = tTensor tBool

tNatTensor :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tNatTensor = tTensor tNat

tRatTensor :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin
tRatTensor = tTensor tRat

forAllNat :: (BuiltinHasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllNat = forAll "n" tNat

forAllIrrelevantNat :: (BuiltinHasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllIrrelevantNat name = pi (Just name) (Implicit False) Irrelevant tNat

irrelImplNatLam :: (BuiltinHasStandardTypes builtin) => Name -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
irrelImplNatLam n = lam n (Implicit False) Irrelevant tNat

--------------------------------------------------------------------------------
-- Constructors DSL

builtinConstructor :: (BuiltinHasStandardData builtin) => BuiltinConstructor -> DSLExpr builtin
builtinConstructor = builtin . mkExpr accessBuiltinConstructor

nil :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin
nil tElem = builtinConstructor Nil @@@ [tElem]

cons :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
cons tElem x xs = builtinConstructor Cons @@@ [tElem] @@ [x, xs]

natLit :: (BuiltinHasStandardData builtin) => Int -> DSLExpr builtin
natLit n = builtinConstructor (NatLiteral n)

boolLit :: (BuiltinHasStandardData builtin) => Bool -> DSLExpr builtin
boolLit b = builtinConstructor (BoolTensorLiteral (ZeroDimTensor b))

ratLit :: (BuiltinHasStandardData builtin) => Rational -> DSLExpr builtin
ratLit r = builtinConstructor (RatTensorLiteral (ZeroDimTensor r))

unitLit :: (BuiltinHasStandardData builtin) => DSLExpr builtin
unitLit = builtinConstructor UnitLiteral

shapeOf :: (BuiltinHasStandardData builtin, BuiltinHasStandardTypes builtin) => Tensor a -> DSLExpr builtin
shapeOf t = foldr (\x xs -> cons tNat (natLit x) xs) (nil tNat) (T.shapeOf t)

--------------------------------------------------------------------------------
-- Functions DSL

builtinFunction :: (BuiltinHasStandardData builtin) => BuiltinFunction -> DSLExpr builtin
builtinFunction = builtin . mkExpr accessBuiltinFunction

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

hasCompare :: (BuiltinHasStandardTypeClasses builtin) => ComparisonOp -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasCompare eq t1 t2 t3 = typeClass (HasCompare eq) [t1, t2, t3]

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

hasAt :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasAt tCont tInd tRes = typeClass HasAt [tCont, tInd, tRes]

hasForeach :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasForeach tInd tElem tRes = typeClass HasForeach [tInd, tElem, tRes]

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

hasVecLits :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
hasVecLits tCont tElem d = typeClass HasVecLits [tCont, tElem, d]

validParameterType :: (BuiltinHasStandardTypeClasses builtin) => ParameterSort -> DSLExpr builtin -> DSLExpr builtin
validParameterType s t = typeClass (ValidParameterType s) [t]

validPropertyType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validPropertyType t = typeClass ValidPropertyType [t]

validInferableParameterType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validInferableParameterType t = typeClass (ValidParameterType Inferable) [t]

validNonInferableParameterType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validNonInferableParameterType t = typeClass (ValidParameterType NonInferable) [t]

validNetworkType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validNetworkType t = typeClass ValidNetworkType [t]

validNetworkTensorType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validNetworkTensorType t = typeClass ValidNetworkTensorType [t]

validDatasetType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validDatasetType t = typeClass ValidDatasetType [t]

validDatasetListElementType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validDatasetListElementType t = typeClass ValidDatasetListElementType [t]

validDatasetTensorElementType :: (BuiltinHasStandardTypeClasses builtin) => DSLExpr builtin -> DSLExpr builtin
validDatasetTensorElementType t = typeClass ValidDatasetTensorElementType [t]

--------------------------------------------------------------------------------
-- Dimension types DSL

-- We keep these seperate even though they are implemented with basic `Nat` for
-- now so it's easy to change it to their own type in future

tDim :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tDim = tNat

tDims :: (BuiltinHasStandardTypes builtin) => DSLExpr builtin
tDims = tList tNat

dimNil :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => DSLExpr builtin
dimNil = nil tDim

dim :: (BuiltinHasStandardData builtin) => Int -> DSLExpr builtin
dim = natLit

dimCons :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
dimCons = cons tNat

singletonDim :: (BuiltinHasStandardTypes builtin, BuiltinHasStandardData builtin) => Int -> DSLExpr builtin
singletonDim d = dimCons (dim d) dimNil

forAllDim :: (BuiltinHasStandardTypes builtin) => Relevance -> (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllDim r = pi (Just "d") (Implicit False) r tDim

forAllDims :: (BuiltinHasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
forAllDims = forAllIrrelevant "ds" tDims

lamType :: (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
lamType = lam "t" Explicit Irrelevant type0

lamDim :: (BuiltinHasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
lamDim = lam "d" (Implicit False) Irrelevant tDim

lamDims :: (BuiltinHasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
lamDims = lam "ds" (Implicit False) Irrelevant tDims

constTensor :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
constTensor t x dims = builtinFunction ConstTensor @@@ [t] @@ [x, dims]

iterate :: (BuiltinHasStandardData builtin) => DSLExpr builtin -> (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
iterate t f n e = do
  let fn = explLam "f" (t ~> t) $ \iterFn -> explLam "e" t $ \resultSoFar -> f iterFn resultSoFar
  builtinFunction Iterate @@@ [t] @@ [fn, n, e]
