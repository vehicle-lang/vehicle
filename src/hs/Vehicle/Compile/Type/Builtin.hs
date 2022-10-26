module Vehicle.Compile.Type.Builtin
  ( typeOfBuiltin
  ) where

import Vehicle.Compile.Prelude
import Vehicle.Language.DSL

-- | Return the type of the provided builtin.
typeOfBuiltin :: Provenance -> Builtin -> CheckedType
typeOfBuiltin p b = fromDSL p $ case b of
  -- Auxillary types
  Polarity{}    -> tPol
  Linearity{}   -> tLin

  -- Type classes
  TypeClass   tc -> typeOfTypeClass tc
  TypeClassOp tc -> typeOfTypeClassOp tc

  -- Types
  Unit   -> type0
  Nat    -> type0
  Int    -> type0
  Rat    -> tLin .~~> type0
  Bool   -> tLin .~~> tPol .~~> type0
  List   -> type0 ~> type0
  Vector -> type0 ~> tNat ~> type0
  Tensor -> type0 ~> tList tNat ~> type0
  Index  -> tNat ~> type0

  -- Boolean operations
  Not ->
    forallIrrelevant tLin $ \l ->
      forallIrrelevant tPol $ \p1 ->
        forallIrrelevant tPol $ \p2 ->
          negPolarity p1 p2 .~~~> tAnnBool l p1 ~> tAnnBool l p2

  Implies -> typeOfBoolOp2 maxLinearity impliesPolarity
  And     -> typeOfBoolOp2 maxLinearity maxPolarity
  Or      -> typeOfBoolOp2 maxLinearity maxPolarity
  If      -> typeOfIf

  -- Arithmetic operations
  Neg dom -> case dom of
    NegInt -> tInt ~> tInt
    NegRat -> tRat ~> tRat

  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddInt -> tInt ~> tInt ~> tInt
    AddRat -> forallLinearityTriples $ \l1 l2 l3 ->
                maxLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Sub dom -> case dom of
    SubInt -> tInt ~> tInt ~> tInt
    SubRat -> forallLinearityTriples $ \l1 l2 l3 ->
                maxLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulInt -> tInt ~> tInt ~> tInt
    MulRat -> forallLinearityTriples $ \l1 l2 l3 ->
                mulLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  Div dom -> case dom of
    DivRat -> forallLinearityTriples $ \l1 l2 l3 ->
                mulLinearity l1 l2 l3 .~~~>
                tAnnRat l1 ~> tAnnRat l2 ~> tAnnRat l3

  -- Comparisons
  Equals dom op -> typeOfEquals dom op
  Order  dom op -> typeOfOrder dom op

  -- Conversion functions
  FromNat n dom -> case dom of
    FromNatToIndex -> forall tNat $ \s -> typeOfFromNat n (tIndex s)
    FromNatToNat   -> typeOfFromNat n tNat
    FromNatToInt   -> typeOfFromNat n tInt
    FromNatToRat   -> typeOfFromNat n (tAnnRat constant)

  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tAnnRat constant)

  FromVec n dom -> case dom of
    FromVecToList -> forall type0 $ \t -> tVector t (natLit n) ~> tList t
    FromVecToVec  -> forall type0 $ \t -> tVector t (natLit n) ~> tVector t (natLit n)

  -- Container functions
  Map dom -> case dom of
    MapList   -> typeOfMap tList
    MapVector -> forall tNat $ \n -> typeOfMap (`tVector` n)

  Fold dom -> case dom of
    FoldList   -> forall type0 $ \tElem ->
                    forall type0 $ \tRes ->
                      (tElem ~> tRes ~> tRes) ~> tRes ~> tList tElem ~> tRes
    FoldVector -> forall type0 $ \tElem ->
                    forall type0 $ \tRes ->
                      forall tNat $ \dim ->
                        (tElem ~> tRes ~> tRes) ~> tRes ~> tVector tElem dim ~> tRes

  Nil     -> typeOfNil
  Cons    -> typeOfCons

  At      -> typeOfAt

  Foreach -> typeOfForeach

typeOfTypeClass :: TypeClass -> DSLExpr
typeOfTypeClass tc = case tc of
  HasEq{}                 -> type0 ~> type0 ~> type0 ~> type0
  HasOrd{}                -> type0 ~> type0 ~> type0 ~> type0
  HasNot                  -> type0 ~> type0 ~> type0
  HasAnd                  -> type0 ~> type0 ~> type0 ~> type0
  HasOr                   -> type0 ~> type0 ~> type0 ~> type0
  HasImplies              -> type0 ~> type0 ~> type0 ~> type0
  HasQuantifier{}         -> type0 ~> type0 ~> type0
  HasAdd                  -> type0 ~> type0 ~> type0 ~> type0
  HasSub                  -> type0 ~> type0 ~> type0 ~> type0
  HasMul                  -> type0 ~> type0 ~> type0 ~> type0
  HasDiv                  -> type0 ~> type0 ~> type0 ~> type0
  HasNeg                  -> type0 ~> type0 ~> type0
  HasFold                 -> type0 ~> type0 ~> type0
  HasQuantifierIn{}       -> type0 ~> type0 ~> type0
  HasIf                   -> type0 ~> type0 ~> type0 ~> type0 ~> type0

  HasNatLits{}            -> type0 ~> type0
  HasRatLits              -> type0 ~> type0
  HasVecLits{}            -> type0 ~> type0 ~> type0

  AlmostEqualConstraint{} -> forall type0 $ \t -> t ~> tList t ~> type0
  NatInDomainConstraint{} -> forall type0 $ \t -> t ~> type0

  LinearityTypeClass t    -> typeOfLinearityTypeClass t
  PolarityTypeClass  t    -> typeOfPolarityTypeClass t


typeOfTypeClassOp :: TypeClassOp -> DSLExpr
typeOfTypeClassOp b = case b of
  NotTC     -> typeOfTCOp1 hasNot
  ImpliesTC -> typeOfTCOp2 hasImplies
  AndTC     -> typeOfTCOp2 hasAnd
  OrTC      -> typeOfTCOp2 hasOr

  NegTC -> typeOfTCOp1 hasNeg
  AddTC -> typeOfTCOp2 hasAdd
  SubTC -> typeOfTCOp2 hasSub
  MulTC -> typeOfTCOp2 hasMul
  DivTC -> typeOfTCOp2 hasDiv

  EqualsTC op -> typeOfTCOp2 $ hasEq op
  OrderTC  op -> typeOfTCOp2 $ hasOrd op

  FromNatTC n -> forall type0 $ \t -> hasNatLits n t ~~~> typeOfFromNat n t
  FromRatTC   -> forall type0 $ \t -> hasRatLits t   ~~~> typeOfFromRat t
  FromVecTC n ->
    forall type0 $ \t ->
      forall type0 $ \e ->
        hasVecLits n e t  ~~~> tVector e (natLit n) ~> t

  MapTC  -> developerError "Unsure about type of MapTC"
  FoldTC -> typeOfFold

  QuantifierTC   q -> typeOfQuantifier   q
  QuantifierInTC q -> typeOfQuantifierIn q

typeOfLinearityTypeClass :: LinearityTypeClass -> DSLExpr
typeOfLinearityTypeClass = \case
  MaxLinearity        -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity        -> tLin ~> tLin ~> tLin ~> type0
  FunctionLinearity{} -> tLin ~> tLin ~> type0
  IfCondLinearity     -> tLin ~> type0

typeOfPolarityTypeClass :: PolarityTypeClass -> DSLExpr
typeOfPolarityTypeClass = \case
  NegPolarity{}      -> tPol ~> tPol ~> type0
  AddPolarity{}      -> tPol ~> tPol ~> type0
  MaxPolarity        -> tPol ~> tPol ~> tPol ~> type0
  EqPolarity{}       -> tPol ~> tPol ~> tPol ~> type0
  ImpliesPolarity{}  -> tPol ~> tPol ~> tPol ~> type0
  FunctionPolarity{} -> tPol ~> tPol ~> type0
  IfCondPolarity     -> tLin ~> type0

typeOfBoolOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr)
              -> (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr)
              -> DSLExpr
typeOfBoolOp2 linearityConstraint polarityConstraint =
  forallLinearityTriples $ \l1 l2 l3 ->
    forallPolarityTriples $ \p1 p2 p3 ->
      linearityConstraint l1 l2 l3 .~~~>
      polarityConstraint  p1 p2 p3 .~~~>
      tAnnBool l1 p1 ~> tAnnBool l2 p2 ~> tAnnBool l3 p3

typeOfIf :: DSLExpr
typeOfIf =
  forall type0 $ \tCond ->
    forall type0 $ \tArg1 ->
      forall type0 $ \tArg2 ->
        forall type0 $ \tRes ->
          hasIf tCond tArg1 tArg2 tRes .~~~>
            tCond ~> tArg1 ~> tArg2 ~> tRes

typeOfEquals :: EqualityDomain -> EqualityOp -> DSLExpr
typeOfEquals domain _op = case domain of
  EqIndex{} ->
    forall tNat $ \n1 ->
      forall tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified

  EqNat{} ->
    tNat ~> tNat ~> tAnnBool constant unquantified

  EqInt{} ->
    tInt ~> tInt ~> tAnnBool constant unquantified

  EqRat{} ->
    forallLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~>
      tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

typeOfOrder :: OrderDomain -> OrderOp -> DSLExpr
typeOfOrder domain _op = case domain of
  OrderIndex{} ->
    forall tNat $ \n1 ->
      forall tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified

  OrderNat{} ->
    tNat ~> tNat ~> tAnnBool constant unquantified

  OrderInt{} ->
    tInt ~> tInt ~> tAnnBool constant unquantified

  OrderRat{} ->
    forallLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~> tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

typeOfTCOp1 :: (DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp1 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp2 constraint =
  forall type0 $ \t1 ->
    forall type0 $ \t2 ->
      forall type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfForeach :: DSLExpr
typeOfForeach =
  forall type0 $ \tRes ->
    forall tNat $ \d ->
      (tIndex d ~> tRes) ~> tVector tRes d

typeOfNil :: DSLExpr
typeOfNil =
  forall type0 $ \tElem ->
    tList tElem

typeOfCons :: DSLExpr
typeOfCons =
  forall type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: DSLExpr
typeOfAt =
  forall type0 $ \tElem ->
    forall tNat $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfMap :: (DSLExpr -> DSLExpr) -> DSLExpr
typeOfMap tCont =
  forall type0 $ \tFrom ->
    forall type0 $ \tTo ->
      (tFrom ~> tTo) ~> tCont tFrom ~> tCont tTo

typeOfFold :: DSLExpr
typeOfFold =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        hasFold tElem tCont ~~~> (tElem ~> tRes ~> tRes) ~> tRes ~> tCont ~> tRes

typeOfQuantifier :: Quantifier -> DSLExpr
typeOfQuantifier q =
  forall type0 $ \tLam ->
    forall type0 $ \tRes ->
      hasQuantifier q tLam tRes ~~~> tLam ~> tRes

typeOfQuantifierIn :: Quantifier -> DSLExpr
typeOfQuantifierIn q =
  forall type0 $ \tElem ->
    forall type0 $ \tCont ->
      forall type0 $ \tRes ->
        hasQuantifierIn q tElem tCont tRes ~~~> (tElem ~> tRes) ~> tCont ~> tRes

typeOfFromNat :: Int -> DSLExpr -> DSLExpr
typeOfFromNat n t = natInDomainConstraint n t .~~~> tNat ~> t

typeOfFromRat :: DSLExpr -> DSLExpr
typeOfFromRat t = tAnnRat constant ~> t