module Vehicle.Compile.Type.Subsystem.Standard.Type
  ( typeStandardBuiltin,
    typeStandardTypingBuiltin,
    typeOfBuiltinConstructor,
    typeOfBuiltinFunction,
    typeOfBuiltinType,
    typeOfNatInDomainConstraint,
    typeOfAnn,
  )
where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.BuiltinInterface.DSL
import Vehicle.Data.DSL
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeStandardTypingBuiltin :: Provenance -> StandardTypingBuiltin -> Type Ix StandardTypingBuiltin
typeStandardTypingBuiltin p b = fromDSL p $ case b of
  StandardBuiltin d -> case d of
    BuiltinConstructor c -> typeOfBuiltinConstructor c
    BuiltinFunction f -> typeOfBuiltinFunction f
    TypeClassOp tcOp -> typeOfTypeClassOp tcOp
    TypeClass tc -> typeOfTypeClass tc
    BuiltinType s -> typeOfBuiltinType s
    NatInDomainConstraint {} -> typeOfNatInDomainConstraint

-- ResourceConstraint c -> _

typeStandardBuiltin :: Provenance -> Builtin -> Type Ix Builtin
typeStandardBuiltin p b = fromDSL p $ case b of
  BuiltinConstructor c -> typeOfBuiltinConstructor c
  BuiltinFunction f -> typeOfBuiltinFunction f
  TypeClassOp tcOp -> typeOfTypeClassOp tcOp
  TypeClass tc -> typeOfTypeClass tc
  BuiltinType s -> typeOfBuiltinType s
  NatInDomainConstraint {} -> typeOfNatInDomainConstraint

--------------------------------------------------------------------------------
-- Type classes

typeOfTypeClass :: (HasStandardTypes builtin) => TypeClass -> DSLExpr builtin
typeOfTypeClass tc = case tc of
  HasEq {} -> type0 ~> type0 ~> type0
  HasOrd {} -> type0 ~> type0 ~> type0
  HasQuantifier {} -> type0 ~> type0 ~> type0
  HasAdd -> type0 ~> type0 ~> type0 ~> type0
  HasSub -> type0 ~> type0 ~> type0 ~> type0
  HasMul -> type0 ~> type0 ~> type0 ~> type0
  HasDiv -> type0 ~> type0 ~> type0 ~> type0
  HasNeg -> type0 ~> type0 ~> type0
  HasMap -> (type0 ~> type0) ~> type0
  HasFold -> (type0 ~> type0) ~> type0
  HasQuantifierIn {} -> type0 ~> type0 ~> type0
  HasNatLits {} -> type0 ~> type0
  HasRatLits -> type0 ~> type0
  HasVecLits {} -> tNat ~> type0 ~> type0

typeOfTypeClassOp :: (HasStandardBuiltins builtin, HasStandardTypeClasses builtin) => TypeClassOp -> DSLExpr builtin
typeOfTypeClassOp b = case b of
  NegTC -> typeOfTCOp1 hasNeg
  AddTC -> typeOfTCOp2 hasAdd
  SubTC -> typeOfTCOp2 hasSub
  MulTC -> typeOfTCOp2 hasMul
  DivTC -> typeOfTCOp2 hasDiv
  EqualsTC op -> typeOfTCComparisonOp $ hasEq op
  OrderTC op -> typeOfTCComparisonOp $ hasOrd op
  FromNatTC -> forAll "A" type0 $ \t -> hasNatLits t ~~~> typeOfFromNat t
  FromRatTC -> forAll "A" type0 $ \t -> hasRatLits t ~~~> typeOfFromRat t
  FromVecTC -> forAllIrrelevantNat "n" $ \n -> forAll "f" (type0 ~> type0) $ \f -> hasVecLits n f ~~~> typeOfFromVec n f
  MapTC -> forAll "f" (type0 ~> type0) $ \f -> hasMap f ~~~> typeOfMap f
  FoldTC -> forAll "f" (type0 ~> type0) $ \f -> hasFold f ~~~> typeOfFold f
  QuantifierTC q ->
    forAll "A" (type0 ~> type0) $ \t ->
      hasQuantifier q t ~~~> typeOfQuantifier t

--------------------------------------------------------------------------------
-- Generic

typeOfBuiltinFunction :: (HasStandardBuiltins builtin) => BuiltinFunction -> DSLExpr builtin
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> tBool ~> tBool
  Implies -> tBool ~> tBool ~> tBool
  And -> tBool ~> tBool ~> tBool
  Or -> tBool ~> tBool ~> tBool
  Quantifier _ -> forAllExpl "A" type0 $ \a -> typeOfQuantifier a
  If -> typeOfIf
  -- Arithmetic operations
  Neg dom -> case dom of
    NegInt -> tInt ~> tInt
    NegRat -> tRat ~> tRat
  Add dom -> case dom of
    AddNat -> tNat ~> tNat ~> tNat
    AddInt -> tInt ~> tInt ~> tInt
    AddRat -> tRat ~> tRat ~> tRat
  Sub dom -> case dom of
    SubInt -> tInt ~> tInt ~> tInt
    SubRat -> tRat ~> tRat ~> tRat
  Mul dom -> case dom of
    MulNat -> tNat ~> tNat ~> tNat
    MulInt -> tInt ~> tInt ~> tInt
    MulRat -> tRat ~> tRat ~> tRat
  Div dom -> case dom of
    DivRat -> tRat ~> tRat ~> tRat
  PowRat -> tRat ~> tInt ~> tRat
  MinRat -> tRat ~> tRat ~> tRat
  MaxRat -> tRat ~> tRat ~> tRat
  -- Comparisons
  Equals dom _op -> case dom of
    EqIndex {} ->
      forAllIrrelevantNat "n1" $ \n1 ->
        forAllIrrelevantNat "n2" $ \n2 ->
          typeOfComparisonOp (tIndex n1) (tIndex n2)
    EqNat {} -> typeOfComparisonOp tNat tNat
    EqInt {} -> typeOfComparisonOp tInt tInt
    EqRat {} -> typeOfComparisonOp tRat tRat
  Order dom _op -> case dom of
    OrderIndex {} ->
      forAllIrrelevantNat "n1" $ \n1 ->
        forAllIrrelevantNat "n2" $ \n2 ->
          tIndex n1 ~> tIndex n2 ~> tBool
    OrderNat {} -> tNat ~> tNat ~> tBool
    OrderInt {} -> tInt ~> tInt ~> tBool
    OrderRat {} -> tInt ~> tInt ~> tBool
  -- Conversion functions
  FromNat dom -> case dom of
    FromNatToIndex -> forAllIrrelevantNat "n" $ \s -> typeOfFromNat (tIndex s)
    FromNatToNat -> typeOfFromNat tNat
    FromNatToInt -> typeOfFromNat tInt
    FromNatToRat -> typeOfFromNat tRat
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat tRat
  -- Container functions
  Fold dom -> case dom of
    FoldList -> typeOfFold tListRaw
    FoldVector -> forAllIrrelevantNat "n" $ \n -> typeOfFold (tVectorFunctor n)
  MapList -> typeOfMap tListRaw
  MapVector -> forAllIrrelevantNat "n" $ \n -> typeOfMap (tVectorFunctor n)
  At -> typeOfAt
  ZipWithVector -> typeOfZipWith
  Indices -> typeOfIndices
  Ann -> typeOfAnn
  -- TODO this needs to be extracted to loss builtins when merged
  Optimise {} -> forAll "t" type0 $ \t -> (tRat ~> tRat ~> tRat) ~> (t ~> tRat) ~> tRat

typeOfBuiltinType :: (HasStandardBuiltins builtin) => BuiltinType -> DSLExpr builtin
typeOfBuiltinType = \case
  Unit -> type0
  Nat -> type0
  Int -> type0
  Rat -> type0
  Bool -> type0
  List -> type0 ~> type0
  Vector -> type0 ~> tNat .~> type0
  Index -> tNat .~> type0

typeOfAnn :: DSLExpr builtin
typeOfAnn = forAllExpl "t" type0 $ \t -> t ~> t

typeOfBuiltinConstructor :: (HasStandardBuiltins builtin) => BuiltinConstructor -> DSLExpr builtin
typeOfBuiltinConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  LUnit -> tUnit
  LBool _ -> tBool
  LIndex x -> forAllIrrelevantNat "n" $ \n -> natInDomainConstraint (natLit x) n .~~~> tIndex n
  LNat {} -> tNat
  LInt {} -> tInt
  LRat {} -> tRat
  LVec n -> typeOfVectorLiteral n

typeOfIf :: (HasStandardTypes builtin) => DSLExpr builtin
typeOfIf =
  forAll "A" type0 $ \t ->
    tBool ~> t ~> t ~> t

typeOfTCOp1 :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
typeOfTCOp1 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
typeOfTCOp2 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      forAll "C" type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfTCComparisonOp :: (HasStandardTypes builtin) => (DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin) -> DSLExpr builtin
typeOfTCComparisonOp constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> typeOfComparisonOp t1 t2

typeOfComparisonOp :: (HasStandardTypes builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
typeOfComparisonOp t1 t2 = t1 ~> t2 ~> tBool

typeOfIndices :: (HasStandardTypes builtin) => DSLExpr builtin
typeOfIndices =
  pi (Just "n") Explicit Relevant tNat $ \n -> tVector (tIndex n) n

typeOfNil :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfNil =
  forAll "A" type0 $ \tElem ->
    tList tElem

typeOfCons :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfCons =
  forAll "A" type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfAt =
  forAll "A" type0 $ \tElem ->
    forAllIrrelevantNat "n" $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfZipWith :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfZipWith =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      forAll "C" type0 $ \t3 ->
        forAllIrrelevantNat "n" $ \tDim ->
          (t1 ~> t2 ~> t3) ~> tVector t1 tDim ~> tVector t2 tDim ~> tVector t3 tDim

typeOfMap :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfMap f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b) ~> f @@ [a] ~> f @@ [b]

typeOfFold :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfFold f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b ~> b) ~> b ~> f @@ [a] ~> b

typeOfQuantifier :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfQuantifier t = (t ~> tBool) ~> tBool

typeOfFromNat :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfFromNat t = forAllExpl "n" tNat $ \n -> natInDomainConstraint n t .~~~> t

typeOfFromRat :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin
typeOfFromRat t = tRat ~> t

typeOfFromVec :: (HasStandardBuiltins builtin) => DSLExpr builtin -> DSLExpr builtin -> DSLExpr builtin
typeOfFromVec n f =
  forAll "A" type0 $ \t ->
    tVector t n ~> f @@ [t]

typeOfVectorLiteral :: (HasStandardBuiltins builtin) => Int -> DSLExpr builtin
typeOfVectorLiteral n = do
  forAll "A" type0 $ \tElem ->
    naryFunc n tElem (tVector tElem (natLit n))

typeOfNatInDomainConstraint :: (HasStandardBuiltins builtin) => DSLExpr builtin
typeOfNatInDomainConstraint = forAll "A" type0 $ \t -> tNat ~> t ~> type0
