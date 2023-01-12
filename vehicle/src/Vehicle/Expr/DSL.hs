module Vehicle.Expr.DSL
  ( DSL (..),
    DSLExpr (..),
    fromDSL,
    type0,
    (~>),
    (~~>),
    (.~~>),
    (~~~>),
    (.~~~>),
    forAll,
    forAllIrrelevant,
    forAllInstance,
    forAllNat,
    forAllTypeTriples,
    forAllLinearityTriples,
    forAllPolarityTriples,
    builtin,
    tUnit,
    tBool,
    tNat,
    tInt,
    tRat,
    tAnnBool,
    tAnnRat,
    tList,
    tListRaw,
    tIndex,
    tPol,
    tLin,
    tVector,
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
    hasNot,
    hasAnd,
    hasOr,
    hasImplies,
    hasQuantifier,
    hasIf,
    natInDomainConstraint,
    maxLinearity,
    mulLinearity,
    addPolarity,
    maxPolarity,
    impliesPolarity,
    negPolarity,
    eqPolarity,
    natLit,
    tMax,
    tHole,
    piType,
    nil,
    cons,
    unquantified,
    constant,
    linear,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Definition

class DSL expr where
  infixl 4 `app`

  hole :: expr
  app :: expr -> NonEmpty (Visibility, Relevance, expr) -> expr
  pi :: Maybe Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lam :: Name -> Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lseq :: expr -> [expr] -> expr
  free :: Identifier -> expr

newtype DSLExpr = DSL
  { unDSL :: Provenance -> DBLevel -> DBExpr
  }

fromDSL :: Provenance -> DSLExpr -> DBExpr
fromDSL p e = unDSL e p 0

boundVar :: DBLevel -> DSLExpr
boundVar i = DSL $ \p j -> Var p (Bound (dbLevelToIndex j i))

approxPiForm :: Maybe Name -> Visibility -> BinderDisplayForm
approxPiForm name = \case
  Explicit {} -> BinderDisplayForm OnlyType False
  Implicit {} -> BinderDisplayForm (OnlyName $ fromMaybe "_" name) True
  Instance {} -> BinderDisplayForm OnlyType False

instance DSL DSLExpr where
  hole = DSL $ \p _i ->
    Hole p "_"

  pi name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        form = approxPiForm name v
        binder = Binder p form v r () varType
        body = unDSL (bodyFn var) p (i + 1)
     in Pi p binder body

  lam name v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var = boundVar i
        binder = Binder p (BinderDisplayForm (OnlyName name) True) v r () varType
        body = unDSL (bodyFn var) p (i + 1)
     in Lam p binder body

  app fun args = DSL $ \p i ->
    let fun' = unDSL fun p i
        args' = fmap (\(v, r, e) -> Arg p v r (unDSL e p i)) args
     in App p fun' args'

  lseq tElem args = DSL $ \p i ->
    App
      p
      (LVec p (fmap (\e -> unDSL e p i) args))
      [ ImplicitArg p (unDSL tElem p i)
      ]

  free ident = DSL $ \p _i ->
    FreeVar p ident

piType :: DBExpr -> DBExpr -> DBExpr
piType t1 t2 = t1 `tMax` t2

universeLevel :: DBExpr -> UniverseLevel
universeLevel (Universe _ (TypeUniv l)) = l
universeLevel (Meta _ _) = 0 -- This is probably going to bite us, apologies.
universeLevel (App _ (Meta _ _) _) = 0 -- This is probably going to bite us, apologies.
universeLevel (Pi _ _ r) = universeLevel r -- This is probably going to bite us, apologies.
universeLevel t =
  developerError $
    "Expected argument of type Type. Found" <+> pretty (show t) <> "."

tMax :: DBExpr -> DBExpr -> DBExpr
tMax t1 t2 = if universeLevel t1 > universeLevel t2 then t1 else t2

builtin :: Builtin -> DSLExpr
builtin b = DSL $ \p _ -> Builtin p b

constructor :: BuiltinConstructor -> DSLExpr
constructor = builtin . Constructor

--------------------------------------------------------------------------------
-- Types

infix 4 @@

(@@) :: DSLExpr -> NonEmpty DSLExpr -> DSLExpr
(@@) f args = app f (fmap (Explicit,Relevant,) args)

-- | Explicit function type
infixr 4 ~>

(~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~> y = pi Nothing Explicit Relevant x (const y)

-- | Implicit function type
infixr 4 ~~>

(~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~~> y = pi Nothing (Implicit False) Relevant x (const y)

-- | Irrelevant implicit function type
infixr 4 .~~>

(.~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x .~~> y = pi Nothing (Implicit False) Irrelevant x (const y)

-- | Instance function type
infixr 4 ~~~>

(~~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~~~> y = pi Nothing (Instance False) Relevant x (const y)

-- | Irrelevant instance function type
infixr 4 .~~~>

(.~~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x .~~~> y = pi Nothing (Instance False) Irrelevant x (const y)

forAll :: Name -> DSLExpr -> (DSLExpr -> DSLExpr) -> DSLExpr
forAll name = pi (Just name) (Implicit False) Relevant

forAllIrrelevant :: Name -> DSLExpr -> (DSLExpr -> DSLExpr) -> DSLExpr
forAllIrrelevant name = pi (Just name) (Implicit False) Irrelevant

forAllInstance :: Name -> DSLExpr -> (DSLExpr -> DSLExpr) -> DSLExpr
forAllInstance name = pi (Just name) (Instance False) Relevant

forAllNat :: (DSLExpr -> DSLExpr) -> DSLExpr
forAllNat = forAll "n" tNat

forAllTypeTriples :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
forAllTypeTriples f =
  forAllIrrelevant "t1" type0 $ \t1 ->
    forAllIrrelevant "t2" type0 $ \t2 ->
      forAllIrrelevant "t3" type0 $ \t3 -> f t1 t2 t3

forAllLinearityTriples :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
forAllLinearityTriples f =
  forAllIrrelevant "l1" tLin $ \l1 ->
    forAllIrrelevant "l2" tLin $ \l2 ->
      forAllIrrelevant "l3" tLin $ \l3 -> f l1 l2 l3

forAllPolarityTriples :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
forAllPolarityTriples f =
  forAllIrrelevant "p1" tPol $ \l1 ->
    forAllIrrelevant "p2" tPol $ \l2 ->
      forAllIrrelevant "p3" tPol $ \l3 -> f l1 l2 l3

universe :: Universe -> DSLExpr
universe u = DSL $ \p _ -> Universe p u

type0 :: DSLExpr
type0 = universe $ TypeUniv 0

tPol :: DSLExpr
tPol = universe PolarityUniv

tLin :: DSLExpr
tLin = universe LinearityUniv

tUnit :: DSLExpr
tUnit = constructor Unit

tBool, tNat, tInt, tRat :: DSLExpr
tBool = constructor Bool
tNat = constructor Nat
tInt = constructor Int
tRat = constructor Rat

tAnnRat :: DSLExpr -> DSLExpr
tAnnRat linearity =
  app
    tRat
    [ (Implicit True, Irrelevant, linearity)
    ]

tAnnBool :: DSLExpr -> DSLExpr -> DSLExpr
tAnnBool linearity polarity =
  app
    tBool
    [ (Implicit True, Irrelevant, linearity),
      (Implicit True, Irrelevant, polarity)
    ]

tVector :: DSLExpr -> DSLExpr -> DSLExpr
tVector tElem dim = constructor Vector @@ [tElem, dim]

tListRaw :: DSLExpr
tListRaw = constructor List

tList :: DSLExpr -> DSLExpr
tList tElem = tListRaw @@ [tElem]

tIndex :: DSLExpr -> DSLExpr
tIndex n = constructor Index @@ [n]

tHole :: Name -> DSLExpr
tHole name = DSL $ \p _ -> Hole p name

--------------------------------------------------------------------------------
-- TypeClass

typeClass :: TypeClass -> NonEmpty DSLExpr -> DSLExpr
typeClass tc args = constructor (TypeClass tc) @@ args

hasEq :: EqualityOp -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasEq eq t1 t2 t3 = typeClass (HasEq eq) [t1, t2, t3]

hasOrd :: OrderOp -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasOrd ord t1 t2 t3 = typeClass (HasOrd ord) [t1, t2, t3]

hasNot :: DSLExpr -> DSLExpr -> DSLExpr
hasNot t1 t2 = typeClass HasNot [t1, t2]

hasAnd :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasAnd t1 t2 t3 = typeClass HasAnd [t1, t2, t3]

hasOr :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasOr t1 t2 t3 = typeClass HasOr [t1, t2, t3]

hasImplies :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasImplies t1 t2 t3 = typeClass HasImplies [t1, t2, t3]

hasQuantifier :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr
hasQuantifier q t1 t2 = typeClass (HasQuantifier q) [t1, t2]

hasAdd :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasAdd t1 t2 t3 = typeClass HasAdd [t1, t2, t3]

hasSub :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasSub t1 t2 t3 = typeClass HasSub [t1, t2, t3]

hasMul :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasMul t1 t2 t3 = typeClass HasMul [t1, t2, t3]

hasDiv :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasDiv t1 t2 t3 = typeClass HasDiv [t1, t2, t3]

hasNeg :: DSLExpr -> DSLExpr -> DSLExpr
hasNeg t1 t2 = typeClass HasNeg [t1, t2]

hasMap :: DSLExpr -> DSLExpr
hasMap tCont = typeClass HasMap [tCont]

hasFold :: DSLExpr -> DSLExpr -> DSLExpr
hasFold tCont tElem = typeClass HasFold [tCont, tElem]

hasIf :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasIf tCond tArg1 tArg2 tRes = typeClass HasIf [tCond, tArg1, tArg2, tRes]

hasQuantifierIn :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
hasQuantifierIn q tCont tElem tRes = typeClass (HasQuantifierIn q) [tCont, tElem, tRes]

hasNatLits :: Int -> DSLExpr -> DSLExpr
hasNatLits n t = typeClass (HasNatLits n) [t]

hasRatLits :: DSLExpr -> DSLExpr
hasRatLits t = typeClass HasRatLits [t]

hasVecLits :: Int -> DSLExpr -> DSLExpr -> DSLExpr
hasVecLits n t d = typeClass (HasVecLits n) [t, d]

--------------------------------------------------------------------------------
-- LinearityTypeClass

linearityTypeClass :: LinearityTypeClass -> NonEmpty DSLExpr -> DSLExpr
linearityTypeClass tc = typeClass (LinearityTypeClass tc)

maxLinearity :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
maxLinearity l1 l2 l3 = linearityTypeClass MaxLinearity [l1, l2, l3]

mulLinearity :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
mulLinearity l1 l2 l3 = linearityTypeClass MulLinearity [l1, l2, l3]

natInDomainConstraint :: Int -> DSLExpr -> DSLExpr
natInDomainConstraint n t = typeClass (NatInDomainConstraint n) [t]

--------------------------------------------------------------------------------
-- PolarityTypeClass

polarityTypeClass :: PolarityTypeClass -> NonEmpty DSLExpr -> DSLExpr
polarityTypeClass tc = typeClass (PolarityTypeClass tc)

addPolarity :: Quantifier -> DSLExpr -> DSLExpr -> DSLExpr
addPolarity q l1 l2 = polarityTypeClass (AddPolarity q) [l1, l2]

maxPolarity :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
maxPolarity l1 l2 l3 = polarityTypeClass MaxPolarity [l1, l2, l3]

eqPolarity :: EqualityOp -> DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
eqPolarity eq p1 p2 p3 = polarityTypeClass (EqPolarity eq) [p1, p2, p3]

impliesPolarity :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
impliesPolarity l1 l2 l3 = polarityTypeClass ImpliesPolarity [l1, l2, l3]

negPolarity :: DSLExpr -> DSLExpr -> DSLExpr
negPolarity l1 l2 = polarityTypeClass NegPolarity [l1, l2]

--------------------------------------------------------------------------------
-- Operations

nil :: DSLExpr -> DSLExpr
nil tElem = lseq tElem []

cons :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
cons tElem x xs =
  app
    (constructor Cons)
    [ (Implicit True, Relevant, tElem),
      (Explicit, Relevant, x),
      (Explicit, Relevant, xs)
    ]

--------------------------------------------------------------------------------
-- Literals

lit :: Literal -> DSLExpr
lit l = DSL $ \p _i -> Literal p l

natLit :: Int -> DSLExpr
natLit = lit . LNat

--------------------------------------------------------------------------------
-- Polarities

constant :: DSLExpr
constant = constructor (Linearity Constant)

linear :: DSLExpr
linear = DSL $ \p _ -> Builtin p (Constructor $ Linearity (Linear $ prov p ""))
  where
    prov = QuantifiedVariableProvenance

unquantified :: DSLExpr
unquantified = constructor (Polarity Unquantified)
