{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Vehicle.Expr.DSL
  ( DSL (..),
    DSLExpr (..),
    StandardDSLExpr,
    PolarityDSLExpr,
    LinearityDSLExpr,
    fromDSL,
    type0,
    (~>),
    (~~>),
    (~~~>),
    (.~~~>),
    (@@),
    (@@@),
    (@@@@),
    (.@@@@),
    explLam,
    implLam,
    instLam,
    naryFunc,
    forAllExpl,
    forAll,
    forAllInstance,
    forAllNat,
    forAllTypeTriples,
    implTypeTripleLam,
    builtin,
    builtinFunction,
    tUnit,
    tBool,
    tNat,
    tInt,
    tRat,
    tList,
    tListRaw,
    tIndex,
    tVector,
    tVectorFunctor,
    hasEq,
    hasOrd,
    hasAdd,
    hasSub,
    hasMul,
    hasDiv,
    hasNeg,
    hasMap,
    hasFold,
    hasQuantifierIn,
    hasNatLits,
    hasRatLits,
    hasVecLits,
    hasQuantifier,
    natInDomainConstraint,
    natLit,
    unitLit,
    addNat,
    tHole,
    nil,
    cons,
    -- Linearity
    tLin,
    forAllLinearities,
    forAllLinearityTriples,
    constant,
    linear,
    maxLinearity,
    mulLinearity,
    quantLinearity,
    -- Polarity
    tPol,
    forAllPolarities,
    forAllPolarityPairs,
    forAllPolarityTriples,
    unquantified,
    quantifierPolarity,
    negPolarity,
    ifPolarity,
    maxPolarity,
    impliesPolarity,
    eqPolarity,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Linearity.Core
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalisable
import Vehicle.Libraries.StandardLibrary (StdLibFunction)
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Definition
--------------------------------------------------------------------------------

class DSL expr where
  infixl 4 `app`

  hole :: expr
  app :: expr -> NonEmpty (Visibility, Relevance, expr) -> expr
  pi :: Maybe Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lam :: Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  free :: StdLibFunction -> expr

newtype DSLExpr types = DSL
  { unDSL :: Provenance -> Lv -> NormalisableExpr types
  }

fromDSL :: Provenance -> DSLExpr types -> NormalisableExpr types
fromDSL p e = unDSL e p 0

boundVar :: Lv -> DSLExpr types
boundVar i = DSL $ \p j -> BoundVar p (dbLevelToIndex j i)

approxPiForm :: Maybe Name -> Visibility -> BinderDisplayForm
approxPiForm name = \case
  Explicit {} -> BinderDisplayForm OnlyType False
  Implicit {} -> BinderDisplayForm (OnlyName $ fromMaybe "_" name) True
  Instance {} -> BinderDisplayForm OnlyType False

instance DSL (DSLExpr types) where
  hole = DSL $ \p _i ->
    Hole p "_"

  pi name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        form = approxPiForm name v
        binder = Binder p form v r varType
        body = unDSL (bodyFn var) p (i + 1)
     in Pi p binder body

  lam name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        binder = Binder p (BinderDisplayForm (OnlyName name) True) v r varType
        body = unDSL (bodyFn var) p (i + 1)
     in Lam p binder body

  app fun args = DSL $ \p i ->
    let fun' = unDSL fun p i
        args' = fmap (\(v, r, e) -> Arg p v r (unDSL e p i)) args
     in App p fun' args'

  free stdlibFn = DSL $ \p _i ->
    FreeVar p (identifierOf stdlibFn)

--------------------------------------------------------------------------------
-- AST
--------------------------------------------------------------------------------

-- | Explicit function type
infixr 4 ~>

(~>) :: DSLExpr types -> DSLExpr types -> DSLExpr types
x ~> y = pi Nothing Explicit Relevant x (const y)

-- | Implicit function type
infixr 4 ~~>

(~~>) :: DSLExpr types -> DSLExpr types -> DSLExpr types
x ~~> y = pi Nothing (Implicit False) Relevant x (const y)

-- | Instance function type
infixr 4 ~~~>

(~~~>) :: DSLExpr types -> DSLExpr types -> DSLExpr types
x ~~~> y = pi Nothing (Instance False) Relevant x (const y)

-- | Irrelevant instance function type
infixr 4 .~~~>

(.~~~>) :: DSLExpr types -> DSLExpr types -> DSLExpr types
x .~~~> y = pi Nothing (Instance False) Irrelevant x (const y)

explLam :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
explLam n = lam n Explicit Relevant

implLam :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
implLam n = lam n (Implicit False) Relevant

instLam :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
instLam n = lam n (Instance False) Relevant

implTypeTripleLam :: (DSLExpr types -> DSLExpr types -> DSLExpr types -> DSLExpr types) -> DSLExpr types
implTypeTripleLam f =
  implLam "t1" type0 $ \t1 ->
    implLam "t2" type0 $ \t2 ->
      implLam "t3" type0 $ \t3 ->
        f t1 t2 t3

infixl 6 @@

(@@) :: DSLExpr types -> NonEmpty (DSLExpr types) -> DSLExpr types
(@@) f args = app f (fmap (Explicit,Relevant,) args)

infixl 6 @@@

(@@@) :: DSLExpr types -> NonEmpty (DSLExpr types) -> DSLExpr types
(@@@) f args = app f (fmap (Implicit True,Relevant,) args)

infixl 6 @@@@

(@@@@) :: DSLExpr types -> NonEmpty (DSLExpr types) -> DSLExpr types
(@@@@) f args = app f (fmap (Instance True,Relevant,) args)

infixl 6 .@@@@

(.@@@@) :: DSLExpr types -> NonEmpty (DSLExpr types) -> DSLExpr types
(.@@@@) f args = app f (fmap (Instance True,Irrelevant,) args)

naryFunc :: Int -> DSLExpr types -> DSLExpr types -> DSLExpr types
naryFunc n a b = foldr (\_ r -> a ~> r) b ([0 .. n - 1] :: [Int])

forAllExpl :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
forAllExpl name = pi (Just name) Explicit Relevant

forAll :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
forAll name = pi (Just name) (Implicit False) Relevant

forAllInstance :: Name -> DSLExpr types -> (DSLExpr types -> DSLExpr types) -> DSLExpr types
forAllInstance name = pi (Just name) (Instance False) Relevant

universe :: UniverseLevel -> DSLExpr types
universe u = DSL $ \p _ -> Universe p u

type0 :: DSLExpr types
type0 = universe $ UniverseLevel 0

forAllTypeTriples :: (DSLExpr types -> DSLExpr types -> DSLExpr types -> DSLExpr types) -> DSLExpr types
forAllTypeTriples f =
  forAll "t1" type0 $ \t1 ->
    forAll "t2" type0 $ \t2 ->
      forAll "t3" type0 $ \t3 -> f t1 t2 t3

builtin :: NormalisableBuiltin types -> DSLExpr types
builtin b = DSL $ \p _ -> Builtin p b

builtinFunction :: BuiltinFunction -> DSLExpr types
builtinFunction b = DSL $ \p _ -> Builtin p (CFunction b)

builtinConstructor :: BuiltinConstructor -> DSLExpr types
builtinConstructor = builtin . CConstructor

--------------------------------------------------------------------------------
-- Standard types
--------------------------------------------------------------------------------

type StandardDSLExpr = DSLExpr StandardBuiltinType

builtinType :: BuiltinType -> StandardDSLExpr
builtinType = builtin . CType . StandardBuiltinType

tUnit :: StandardDSLExpr
tUnit = builtinType Unit

tBool, tNat, tInt, tRat :: StandardDSLExpr
tBool = builtinType Bool
tNat = builtinType Nat
tInt = builtinType Int
tRat = builtinType Rat

tVector :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
tVector tElem dim = builtinType Vector @@ [tElem, dim]

tVectorFunctor :: StandardDSLExpr -> StandardDSLExpr
tVectorFunctor n = explLam "A" type0 (\a -> tVector a n)

tListRaw :: StandardDSLExpr
tListRaw = builtinType List

tList :: StandardDSLExpr -> StandardDSLExpr
tList tElem = tListRaw @@ [tElem]

tIndex :: StandardDSLExpr -> StandardDSLExpr
tIndex n = builtinType Index @@ [n]

tHole :: Name -> StandardDSLExpr
tHole name = DSL $ \p _ -> Hole p name

forAllNat :: (StandardDSLExpr -> StandardDSLExpr) -> StandardDSLExpr
forAllNat = forAll "n" tNat

--------------------------------------------------------------------------------
-- TypeClass

builtinTypeClass :: TypeClass -> StandardDSLExpr
builtinTypeClass = builtin . CType . StandardTypeClass

typeClass :: TypeClass -> NonEmpty StandardDSLExpr -> StandardDSLExpr
typeClass tc args = builtinTypeClass tc @@ args

hasEq :: EqualityOp -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasEq eq t1 t2 = typeClass (HasEq eq) [t1, t2]

hasOrd :: OrderOp -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasOrd ord t1 t2 = typeClass (HasOrd ord) [t1, t2]

hasQuantifier :: Quantifier -> StandardDSLExpr -> StandardDSLExpr
hasQuantifier q t = typeClass (HasQuantifier q) [t]

numOp2TypeClass :: TypeClass -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
numOp2TypeClass tc t1 t2 t3 = typeClass tc [t1, t2, t3]

hasAdd :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasAdd = numOp2TypeClass HasAdd

hasSub :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasSub = numOp2TypeClass HasSub

hasMul :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasMul = numOp2TypeClass HasMul

hasDiv :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasDiv = numOp2TypeClass HasDiv

hasNeg :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: StandardDSLExpr -> StandardDSLExpr
hasMap tCont = typeClass HasMap [tCont]

hasFold :: StandardDSLExpr -> StandardDSLExpr
hasFold tCont = typeClass HasFold [tCont]

hasQuantifierIn :: Quantifier -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: StandardDSLExpr -> StandardDSLExpr
hasNatLits t = typeClass HasNatLits [t]

hasRatLits :: StandardDSLExpr -> StandardDSLExpr
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
hasVecLits n d = typeClass HasVecLits [n, d]

natInDomainConstraint :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
natInDomainConstraint n t = typeClass NatInDomainConstraint [n, t]

--------------------------------------------------------------------------------
-- Constructors

nil :: StandardDSLExpr -> StandardDSLExpr
nil tElem = builtinConstructor Nil @@@ [tElem]

cons :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
cons tElem x xs = builtinConstructor Cons @@@ [tElem] @@ [x, xs]

natLit :: Int -> StandardDSLExpr
natLit n = builtinConstructor (LNat n)

unitLit :: StandardDSLExpr
unitLit = builtinConstructor LUnit

--------------------------------------------------------------------------------
-- Operations

addNat :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
addNat x y = builtinFunction (Add AddNat) @@ [x, y]

--------------------------------------------------------------------------------
-- Linearity

type LinearityDSLExpr = DSLExpr LinearityType

forAllLinearities :: (LinearityDSLExpr -> LinearityDSLExpr) -> LinearityDSLExpr
forAllLinearities f = forAll "l" tLin $ \l -> f l

forAllLinearityTriples :: (LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr) -> LinearityDSLExpr
forAllLinearityTriples f =
  forAll "l1" tLin $ \l1 ->
    forAll "l2" tLin $ \l2 ->
      forAll "l3" tLin $ \l3 -> f l1 l2 l3

constant :: LinearityDSLExpr
constant = builtin (CType (Linearity Constant))

linearityTypeClass :: LinearityTypeClass -> NonEmpty LinearityDSLExpr -> LinearityDSLExpr
linearityTypeClass tc args = builtin (CType (LinearityTypeClass tc)) @@ args

maxLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
maxLinearity l1 l2 l3 = linearityTypeClass MaxLinearity [l1, l2, l3]

mulLinearity :: LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
mulLinearity l1 l2 l3 = linearityTypeClass MulLinearity [l1, l2, l3]

quantLinearity :: Quantifier -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr
quantLinearity q l1 l2 = linearityTypeClass (QuantifierLinearity q) [l1, l2]

linear :: LinearityDSLExpr
linear = DSL $ \p _ -> Builtin p (CType (Linearity (Linear $ prov p "")))
  where
    prov = QuantifiedVariableProvenance

tLin :: LinearityDSLExpr
tLin = type0

--------------------------------------------------------------------------------
-- Polarities

type PolarityDSLExpr = DSLExpr PolarityType

forAllPolarities :: (PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarities f = forAll "p" tPol $ \p -> f p

forAllPolarityPairs :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityPairs f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      f p1 p2

forAllPolarityTriples :: (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) -> PolarityDSLExpr
forAllPolarityTriples f =
  forAll "p1" tPol $ \p1 ->
    forAll "p2" tPol $ \p2 ->
      forAll "p3" tPol $ \p3 ->
        f p1 p2 p3

unquantified :: PolarityDSLExpr
unquantified = builtin (CType (Polarity Unquantified))

polarityTypeClass :: PolarityTypeClass -> NonEmpty PolarityDSLExpr -> PolarityDSLExpr
polarityTypeClass tc args = builtin (CType (PolarityTypeClass tc)) @@ args

quantifierPolarity :: Quantifier -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
quantifierPolarity q l1 l2 = polarityTypeClass (QuantifierPolarity q) [l1, l2]

maxPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
maxPolarity l1 l2 l3 = polarityTypeClass MaxPolarity [l1, l2, l3]

ifPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
ifPolarity l1 l2 l3 l4 = polarityTypeClass IfPolarity [l1, l2, l3, l4]

eqPolarity :: EqualityOp -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
eqPolarity eq p1 p2 p3 = polarityTypeClass (EqPolarity eq) [p1, p2, p3]

impliesPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
impliesPolarity l1 l2 l3 = polarityTypeClass ImpliesPolarity [l1, l2, l3]

negPolarity :: PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr
negPolarity l1 l2 = polarityTypeClass NegPolarity [l1, l2]

tPol :: PolarityDSLExpr
tPol = type0
