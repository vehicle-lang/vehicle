module Vehicle.Compile.Type.Builtin
  ( typeOfBuiltin,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Expr.DSL

-- | Return the type of the provided builtin.
typeOfBuiltin :: Provenance -> Builtin -> CheckedType
typeOfBuiltin p b = fromDSL p $ case b of
  Constructor c -> typeOfConstructor c
  -- Type classes operations
  TypeClassOp tc -> typeOfTypeClassOp tc
  -- Types
  Tensor -> type0 ~> tList tNat ~> type0
  -- Boolean operations
  Not ->
    forAllIrrelevant tLin $ \l ->
      forAllIrrelevant tPol $ \p1 ->
        forAllIrrelevant tPol $ \p2 ->
          negPolarity p1 p2 .~~~> tAnnBool l p1 ~> tAnnBool l p2
  Implies -> typeOfBoolOp2 maxLinearity impliesPolarity
  And -> typeOfBoolOp2 maxLinearity maxPolarity
  Or -> typeOfBoolOp2 maxLinearity maxPolarity
  If -> typeOfIf
  -- Arithmetic operations
  Neg dom -> case dom of
    NegInt -> tInt ~> tInt
    NegRat -> tRat ~> tRat
  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddInt -> tInt ~> tInt ~> tInt
    AddRat -> forAllLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3
        .~~~> tAnnRat l1
        ~> tAnnRat l2
        ~> tAnnRat l3
  Sub dom -> case dom of
    SubInt -> tInt ~> tInt ~> tInt
    SubRat -> forAllLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3
        .~~~> tAnnRat l1
        ~> tAnnRat l2
        ~> tAnnRat l3
  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulInt -> tInt ~> tInt ~> tInt
    MulRat -> forAllLinearityTriples $ \l1 l2 l3 ->
      mulLinearity l1 l2 l3
        .~~~> tAnnRat l1
        ~> tAnnRat l2
        ~> tAnnRat l3
  Div dom -> case dom of
    DivRat -> forAllLinearityTriples $ \l1 l2 l3 ->
      mulLinearity l1 l2 l3
        .~~~> tAnnRat l1
        ~> tAnnRat l2
        ~> tAnnRat l3
  -- Comparisons
  Equals dom op -> typeOfEquals dom op
  Order dom op -> typeOfOrder dom op
  -- Conversion functions
  FromNat n dom -> case dom of
    FromNatToIndex -> forAll tNat $ \s -> typeOfFromNat n (tIndex s)
    FromNatToNat -> typeOfFromNat n tNat
    FromNatToInt -> typeOfFromNat n tInt
    FromNatToRat -> typeOfFromNat n (tAnnRat constant)
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tAnnRat constant)
  FromVec n dom -> case dom of
    FromVecToList -> forAll type0 $ \t -> tVector t (natLit n) ~> tList t
    FromVecToVec -> forAll type0 $ \t -> tVector t (natLit n) ~> tVector t (natLit n)
  -- Container functions
  Map dom -> case dom of
    MapList -> typeOfMap tListRaw
    MapVector -> forAll tNat $ \n -> typeOfMap (lam Explicit Relevant type0 (`tVector` n))
  Fold dom -> case dom of
    FoldList -> forAll type0 $ \tElem ->
      typeOfFold tElem (tList tElem)
    FoldVector -> forAll type0 $ \tElem ->
      forAll tNat $ \dim ->
        typeOfFold tElem (tVector tElem dim)
  At -> typeOfAt
  Foreach -> typeOfForeach

typeOfConstructor :: BuiltinConstructor -> DSLExpr
typeOfConstructor = \case
  Unit -> type0
  Nat -> type0
  Int -> type0
  Rat -> tLin .~~> type0
  Bool -> tLin .~~> tPol .~~> type0
  List -> type0 ~> type0
  Vector -> type0 ~> tNat ~> type0
  Index -> tNat ~> type0
  TypeClass tc -> typeOfTypeClass tc
  Polarity {} -> tPol
  Linearity {} -> tLin
  Nil -> typeOfNil
  Cons -> typeOfCons

typeOfTypeClass :: TypeClass -> DSLExpr
typeOfTypeClass tc = case tc of
  HasEq {} -> type0 ~> type0 ~> type0 ~> type0
  HasOrd {} -> type0 ~> type0 .~~> type0 .~~> type0
  HasNot -> type0 ~> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasQuantifier {} -> type0 ~> type0 ~> type0
  HasAdd -> type0 ~> type0 .~~> type0 .~~> type0
  HasSub -> type0 ~> type0 .~~> type0 .~~> type0
  HasMul -> type0 ~> type0 .~~> type0 .~~> type0
  HasDiv -> type0 ~> type0 ~> type0 ~> type0
  HasNeg -> type0 ~> type0 ~> type0
  HasMap -> (type0 ~> type0) ~> type0
  HasFold -> type0 ~> type0 ~> type0
  HasQuantifierIn {} -> type0 ~> type0 ~> type0
  HasIf -> type0 ~> type0 ~> type0 ~> type0 ~> type0
  HasNatLits {} -> type0 ~> type0
  HasRatLits -> type0 ~> type0
  HasVecLits {} -> type0 ~> type0 ~> type0
  AlmostEqualConstraint {} -> forAll type0 $ \t -> t ~> tList t ~> type0
  NatInDomainConstraint {} -> forAll type0 $ \t -> t ~> type0
  LinearityTypeClass t -> typeOfLinearityTypeClass t
  PolarityTypeClass t -> typeOfPolarityTypeClass t

typeOfTypeClassOp :: TypeClassOp -> DSLExpr
typeOfTypeClassOp b = case b of
  NotTC -> typeOfTCOp1 hasNot
  ImpliesTC -> typeOfTCOp2 hasImplies
  AndTC -> typeOfTCOp2 hasAnd
  OrTC -> typeOfTCOp2 hasOr
  NegTC -> typeOfTCOp1 hasNeg
  AddTC -> typeOfTCOp2 hasAdd
  SubTC -> typeOfTCOp2 hasSub
  MulTC -> typeOfTCOp2 hasMul
  DivTC -> typeOfTCOp2 hasDiv
  EqualsTC op -> typeOfTCOp2 $ hasEq op
  OrderTC op -> typeOfTCOp2 $ hasOrd op
  FromNatTC n -> forAll type0 $ \t -> hasNatLits n t ~~~> typeOfFromNat n t
  FromRatTC -> forAll type0 $ \t -> hasRatLits t ~~~> typeOfFromRat t
  FromVecTC n ->
    forAll type0 $ \t ->
      forAll type0 $ \e ->
        hasVecLits n e t ~~~> tVector e (natLit n) ~> t
  MapTC -> forAll (type0 ~> type0) $ \c -> hasMap c ~~~> typeOfMap c
  FoldTC -> typeOfFoldTC
  QuantifierTC q -> typeOfQuantifier q
  QuantifierInTC q -> typeOfQuantifierIn q

typeOfLinearityTypeClass :: LinearityTypeClass -> DSLExpr
typeOfLinearityTypeClass = \case
  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity -> tLin ~> tLin ~> tLin ~> type0
  FunctionLinearity {} -> tLin ~> tLin ~> type0
  IfCondLinearity -> tLin ~> type0

typeOfPolarityTypeClass :: PolarityTypeClass -> DSLExpr
typeOfPolarityTypeClass = \case
  NegPolarity {} -> tPol ~> tPol ~> type0
  AddPolarity {} -> tPol ~> tPol ~> type0
  MaxPolarity -> tPol ~> tPol ~> tPol ~> type0
  EqPolarity {} -> tPol ~> tPol ~> tPol ~> type0
  ImpliesPolarity {} -> tPol ~> tPol ~> tPol ~> type0
  FunctionPolarity {} -> tPol ~> tPol ~> type0
  IfCondPolarity -> tLin ~> type0

typeOfBoolOp2 ::
  (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) ->
  (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) ->
  DSLExpr
typeOfBoolOp2 linearityConstraint polarityConstraint =
  forAllLinearityTriples $ \l1 l2 l3 ->
    forAllPolarityTriples $ \p1 p2 p3 ->
      linearityConstraint l1 l2 l3
        .~~~> polarityConstraint p1 p2 p3
        .~~~> tAnnBool l1 p1
        ~> tAnnBool l2 p2
        ~> tAnnBool l3 p3

typeOfIf :: DSLExpr
typeOfIf =
  forAll type0 $ \tCond ->
    forAll type0 $ \tArg1 ->
      forAll type0 $ \tArg2 ->
        forAll type0 $ \tRes ->
          hasIf tCond tArg1 tArg2 tRes
            .~~~> tCond
            ~> tArg1
            ~> tArg2
            ~> tRes

typeOfEquals :: EqualityDomain -> EqualityOp -> DSLExpr
typeOfEquals domain _op = case domain of
  EqIndex {} ->
    forAll tNat $ \n1 ->
      forAll tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified
  EqNat {} ->
    tNat ~> tNat ~> tAnnBool constant unquantified
  EqInt {} ->
    tInt ~> tInt ~> tAnnBool constant unquantified
  EqRat {} ->
    forAllLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3
        .~~~> tAnnRat l1
        ~> tAnnRat l2
        ~> tAnnBool l3 unquantified

typeOfOrder :: OrderDomain -> OrderOp -> DSLExpr
typeOfOrder domain _op = case domain of
  OrderIndex {} ->
    forAll tNat $ \n1 ->
      forAll tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified
  OrderNat {} ->
    tNat ~> tNat ~> tAnnBool constant unquantified
  OrderInt {} ->
    tInt ~> tInt ~> tAnnBool constant unquantified
  OrderRat {} ->
    forAllLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~> tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

typeOfTCOp1 :: (DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp1 constraint =
  forAll type0 $ \t1 ->
    forAll type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp2 constraint =
  forAll type0 $ \t1 ->
    forAll type0 $ \t2 ->
      forAll type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfForeach :: DSLExpr
typeOfForeach =
  forAll type0 $ \tRes ->
    forAll tNat $ \d ->
      (tIndex d ~> tRes) ~> tVector tRes d

typeOfNil :: DSLExpr
typeOfNil =
  forAll type0 $ \tElem ->
    tList tElem

typeOfCons :: DSLExpr
typeOfCons =
  forAll type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: DSLExpr
typeOfAt =
  forAll type0 $ \tElem ->
    forAll tNat $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfMap :: DSLExpr -> DSLExpr
typeOfMap tCont =
  forAll type0 $ \tFrom ->
    forAll type0 $ \tTo ->
      (tFrom ~> tTo)
        ~> app tCont [(Explicit, Relevant, tFrom)]
        ~> app tCont [(Explicit, Relevant, tTo)]

typeOfFoldTC :: DSLExpr
typeOfFoldTC =
  forAll type0 $ \tElem ->
    forAll type0 $ \tCont ->
      hasFold tElem tCont
        ~~~> typeOfFold tElem tCont

typeOfFold :: DSLExpr -> DSLExpr -> DSLExpr
typeOfFold tElem tCont =
  forAll
    type0
    ( \tRes ->
        (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes
    )

typeOfQuantifier :: Quantifier -> DSLExpr
typeOfQuantifier q =
  forAll type0 $ \tLam ->
    forAll type0 $ \tRes ->
      hasQuantifier q tLam tRes ~~~> tLam ~> tRes

typeOfQuantifierIn :: Quantifier -> DSLExpr
typeOfQuantifierIn q =
  forAll type0 $ \tElem ->
    forAll type0 $ \tCont ->
      forAll type0 $ \tRes ->
        hasQuantifierIn q tElem tCont tRes ~~~> (tElem ~> tRes) ~> tCont ~> tRes

typeOfFromNat :: Int -> DSLExpr -> DSLExpr
typeOfFromNat n t = natInDomainConstraint n t .~~~> tNat ~> t

typeOfFromRat :: DSLExpr -> DSLExpr
typeOfFromRat t = tAnnRat constant ~> t
