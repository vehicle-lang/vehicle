module Vehicle.Expr.DSL
  ( DSL(..)
  , DSLExpr(..)
  , fromDSL
  , type0
  , (~>)
  , (~~>)
  , (.~~>)
  , (~~~>)
  , (.~~~>)
  , forAll
  , forAllIrrelevant
  , forAllLinearityTriples
  , forAllPolarityTriples
  , tUnit
  , tBool
  , tNat
  , tInt
  , tRat
  , tAnnBool
  , tAnnRat
  , tList
  , tTensor
  , tIndex
  , tPol
  , tLin
  , tVector
  , hasEq
  , hasOrd
  , hasAdd
  , hasSub
  , hasMul
  , hasDiv
  , hasNeg
  , hasFold
  , hasQuantifierIn
  , hasNatLits
  , hasRatLits
  , hasVecLits
  , hasNot
  , hasAnd
  , hasOr
  , hasImplies
  , hasQuantifier
  , hasIf
  , natInDomainConstraint
  , maxLinearity
  , mulLinearity
  , addPolarity
  , maxPolarity
  , impliesPolarity
  , negPolarity
  , natLit
  , tMax
  , tHole
  , piType
  , nil
  , cons
  , unquantified
  , constant
  , linear
  ) where

import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (pi)

import Vehicle.Compile.Prelude
import Vehicle.Expr.DeBruijn

--------------------------------------------------------------------------------
-- Definition

class DSL expr where
  infixl 4 `app`

  hole :: expr
  app  :: expr -> NonEmpty (Visibility, Relevance, expr) -> expr
  pi   :: Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lam  :: Visibility -> Relevance -> expr -> (expr -> expr) -> expr
  lseq :: expr -> [expr] -> expr

newtype DSLExpr = DSL
  { unDSL :: Provenance -> BindingDepth -> DBExpr
  }

fromDSL :: Provenance -> DSLExpr -> DBExpr
fromDSL p e = unDSL e p 0

boundVar :: BindingDepth -> DSLExpr
boundVar i = DSL $ \p j -> Var p (Bound $ DBIndex (j - (i + 1)))

approxPiForm :: Visibility -> BinderForm
approxPiForm = \case
  Explicit -> BinderForm OnlyType False
  Implicit -> BinderForm OnlyName True
  Instance -> BinderForm OnlyType False

instance DSL DSLExpr where
  hole = DSL $ \p _i ->
    Hole p "_"

  pi v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var     = boundVar i
        form    = approxPiForm v
        binder  = Binder p form v r Nothing varType
        body    = unDSL (bodyFn var) p (i + 1)
    in Pi p binder body

  lam v r binderType bodyFn = DSL $ \p i ->
    let varType = unDSL binderType p i
        var     = boundVar i
        binder  = Binder p (BinderForm OnlyName True) v r Nothing varType
        body    = unDSL (bodyFn var) p (i + 1)
    in Lam p binder body

  app fun args = DSL $ \p i ->
    let fun' = unDSL fun p i
        args' = fmap (\(v, r, e) -> Arg p v r (unDSL e p i)) args
    in App p fun' args'

  lseq tElem args = DSL $ \p i ->
    App p (LVec p (fmap (\e -> unDSL e p i) args))
      [ ImplicitArg p (unDSL tElem p i)
      ]

piType :: DBExpr -> DBExpr -> DBExpr
piType t1 t2 = t1 `tMax` t2

universeLevel :: DBExpr -> UniverseLevel
universeLevel (Universe _ (TypeUniv l)) = l
universeLevel (Meta _ _ )               = 0 -- This is probably going to bite us, apologies.
universeLevel (App _ (Meta _ _) _)      = 0 -- This is probably going to bite us, apologies.
universeLevel (Pi _ _ r)                = universeLevel r -- This is probably going to bite us, apologies.
universeLevel t                         = developerError $
  "Expected argument of type Type. Found" <+> pretty (show t) <> "."

tMax :: DBExpr -> DBExpr -> DBExpr
tMax t1 t2  = if universeLevel t1 > universeLevel t2 then t1 else t2

builtin :: Builtin -> DSLExpr
builtin b = DSL $ \ann _ -> Builtin ann b

constructor :: BuiltinConstructor -> DSLExpr
constructor = builtin . Constructor

--------------------------------------------------------------------------------
-- Types

infix 4 @@
(@@) :: DSLExpr -> NonEmpty DSLExpr -> DSLExpr
(@@) f args = app f (fmap (Explicit, Relevant,) args)

-- | Explicit function type

infixr 4 ~>
(~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~> y = pi Explicit Relevant x (const y)

-- | Implicit function type
infixr 4 ~~>
(~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~~> y = pi Implicit Relevant x (const y)

-- | Irrelevant implicit function type
infixr 4 .~~>
(.~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x .~~> y = pi Implicit Irrelevant x (const y)

-- | Instance function type
infixr 4 ~~~>
(~~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x ~~~> y = pi Instance Relevant x (const y)

-- | Irrelevant instance function type
infixr 4 .~~~>
(.~~~>) :: DSLExpr -> DSLExpr -> DSLExpr
x .~~~> y = pi Instance Irrelevant x (const y)

forAll :: DSLExpr -> (DSLExpr -> DSLExpr) -> DSLExpr
forAll = pi Implicit Relevant

forAllIrrelevant :: DSLExpr -> (DSLExpr -> DSLExpr) -> DSLExpr
forAllIrrelevant = pi Implicit Irrelevant

forAllLinearityTriples :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
forAllLinearityTriples f =
  forAllIrrelevant tLin $ \l1 ->
    forAllIrrelevant tLin $ \l2 ->
      forAllIrrelevant tLin $ \l3 -> f l1 l2 l3

forAllPolarityTriples :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
forAllPolarityTriples f =
  forAllIrrelevant tPol $ \l1 ->
    forAllIrrelevant tPol $ \l2 ->
      forAllIrrelevant tPol $ \l3 -> f l1 l2 l3

universe :: Universe -> DSLExpr
universe u = DSL $ \ann _ -> Universe ann u

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
tNat  = constructor Nat
tInt  = constructor Int
tRat  = constructor Rat

tAnnRat :: DSLExpr ->  DSLExpr
tAnnRat linearity = app tRat
  [ (Implicit, Irrelevant, linearity)
  ]

tAnnBool :: DSLExpr -> DSLExpr ->  DSLExpr
tAnnBool linearity polarity = app tBool
  [ (Implicit, Irrelevant, linearity)
  , (Implicit, Irrelevant, polarity)
  ]

tVector :: DSLExpr -> DSLExpr -> DSLExpr
tVector tElem dim = constructor Vector @@ [tElem, dim]

tTensor :: DSLExpr -> DSLExpr -> DSLExpr
tTensor tElem dims = builtin Tensor @@ [tElem, dims]

tList :: DSLExpr -> DSLExpr
tList tElem = constructor List @@ [tElem]

tIndex :: DSLExpr -> DSLExpr
tIndex n = constructor Index @@ [n]

tHole :: Name -> DSLExpr
tHole name = DSL $ \ann _ -> Hole ann name

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
maxLinearity l1 l2 l3 = linearityTypeClass MulLinearity [l1, l2, l3]

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

impliesPolarity :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
impliesPolarity l1 l2 l3 = polarityTypeClass ImpliesPolarity [l1, l2, l3]

negPolarity :: DSLExpr -> DSLExpr -> DSLExpr
negPolarity l1 l2 = polarityTypeClass NegPolarity [l1, l2]

--------------------------------------------------------------------------------
-- Operations

nil :: DSLExpr -> DSLExpr
nil tElem = lseq tElem []

cons :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
cons tElem x xs = app (constructor Cons)
  [ (Implicit, Relevant, tElem)
  , (Explicit, Relevant, x)
  , (Explicit, Relevant, xs)
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
  where prov = QuantifiedVariableProvenance

unquantified :: DSLExpr
unquantified = constructor (Polarity Unquantified)
