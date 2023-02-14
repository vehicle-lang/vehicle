module Vehicle.Compile.Type.Subsystem.Standard.Type
  ( typeStandardBuiltin,
    standardTypeOfLiteral,
    standardTypeOfVectorLiteral,
    handleStandardTypingError,
    getStandardPropertyInfo,
    relevanceOfTypeClass,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Monoid (Endo (..), appEndo)
import Data.Text (pack)
import Vehicle.Compile.Error (CompileError (..), MonadCompile)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Subsystem.Standard.Constraint.Core (getTypeClass)
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DSL
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeStandardBuiltin :: Provenance -> Builtin -> CheckedType Builtin
typeStandardBuiltin p b = fromDSL p $ case b of
  Constructor c -> typeOfConstructor c
  TypeClassOp tc -> typeOfTypeClassOp tc
  BuiltinFunction f -> typeOfBuiltinFunction f

typeOfBuiltinFunction :: BuiltinFunction -> DSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not ->
    forAllIrrelevant "l" tLin $ \l ->
      forAllIrrelevant "p1" tPol $ \p1 ->
        forAllIrrelevant "p2" tPol $ \p2 ->
          negPolarity p1 p2 .~~~> tAnnBool l p1 ~> tAnnBool l p2
  Implies -> typeOfBoolOp2 maxLinearity impliesPolarity
  And -> typeOfBoolOp2 maxLinearity maxPolarity
  Or -> typeOfBoolOp2 maxLinearity maxPolarity
  Quantifier {} -> developerError "types for base quantifiers not yet implemented"
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
  Equals dom _op -> case dom of
    EqIndex {} ->
      forAll "n1" tNat $ \n1 ->
        forAll "n2" tNat $ \n2 ->
          typeOfComparisonOp constant (tIndex n1) (tIndex n2)
    EqNat {} -> typeOfComparisonOp constant tNat tNat
    EqInt {} -> typeOfComparisonOp constant tInt tInt
    EqRat {} ->
      forAllLinearityTriples $ \l1 l2 l3 ->
        maxLinearity l1 l2 l3
          .~~~> typeOfComparisonOp l3 (tAnnRat l1) (tAnnRat l2)
  Order dom op -> typeOfOrder dom op
  {-
    OrderIndex {} ->
      forAll "n1" tNat $ \n1 ->
        forAll "n2" tNat $ \n2 ->
          typeOfComparisonOp (tIndex n1) (tIndex n2) constant
    OrderNat {} -> typeOfComparisonOp tNat tNat constant
    OrderInt {} -> typeOfComparisonOp tInt tInt constant
    OrderRat {} ->
      forAllLinearityTriples $ \l1 l2 l3 ->
        maxLinearity l1 l2 l3 .~~~>
          typeOfComparisonOp (tAnnRat l1) (tAnnRat l2) l3
          -}

  -- Conversion functions
  FromNat n dom -> case dom of
    FromNatToIndex -> forAllNat $ \s -> typeOfFromNat n (tIndex s)
    FromNatToNat -> typeOfFromNat n tNat
    FromNatToInt -> typeOfFromNat n tInt
    FromNatToRat -> typeOfFromNat n (tAnnRat constant)
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat (tAnnRat constant)
  FromVec n dom -> case dom of
    FromVecToList -> forAll "A" type0 $ \t -> tVector t (natLit n) ~> tList t
    FromVecToVec -> forAll "A" type0 $ \t -> tVector t (natLit n) ~> tVector t (natLit n)
  -- Container functions
  Map dom -> case dom of
    MapList -> typeOfMap tListRaw
    MapVector -> forAllNat $ \n -> typeOfMap (lam "n" Explicit Relevant type0 (`tVector` n))
  Fold dom -> case dom of
    FoldList -> typeOfFold tListRaw
    FoldVector -> forAllNat $ \n -> typeOfFold (lam "n" Explicit Relevant type0 (`tVector` n))
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
  HasEq {} -> type0 .~~> type0 ~> type0 ~> type0
  HasOrd {} -> type0 ~> type0 .~~> type0 .~~> type0
  HasNot -> type0 ~> type0 ~> type0
  HasAnd -> type0 ~> type0 ~> type0 ~> type0
  HasOr -> type0 ~> type0 ~> type0 ~> type0
  HasImplies -> type0 ~> type0 ~> type0 ~> type0
  HasQuantifier {} -> type0 ~> type0 ~> type0
  HasAdd -> type0 ~> type0 ~> type0 ~> type0
  HasSub -> type0 ~> type0 ~> type0 ~> type0
  HasMul -> type0 ~> type0 ~> type0 ~> type0
  HasDiv -> type0 ~> type0 ~> type0 ~> type0
  HasNeg -> type0 ~> type0 ~> type0
  HasMap -> (type0 ~> type0) ~> type0
  HasFold -> type0 ~> type0 ~> type0
  HasQuantifierIn {} -> type0 ~> type0 ~> type0
  HasIf -> type0 ~> type0 ~> type0 ~> type0 ~> type0
  HasNatLits {} -> type0 ~> type0
  HasRatLits -> type0 ~> type0
  HasVecLits {} -> type0 ~> type0 ~> type0
  AlmostEqualConstraint {} -> forAll "A" type0 $ \t -> t ~> tList t ~> type0
  NatInDomainConstraint {} -> forAll "A" type0 $ \t -> t ~> type0
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
  EqualsTC op -> typeOfTCComparisonOp $ hasEq op
  OrderTC op -> typeOfTCOp2 $ hasOrd op -- typeOfTCComparisonOp $ hasOrd op
  FromNatTC n -> forAll "A" type0 $ \t -> hasNatLits n t ~~~> typeOfFromNat n t
  FromRatTC -> forAll "A" type0 $ \t -> hasRatLits t ~~~> typeOfFromRat t
  FromVecTC n ->
    forAll "A" type0 $ \t ->
      forAll "B" type0 $ \e ->
        hasVecLits n e t ~~~> tVector e (natLit n) ~> t
  MapTC -> forAll "f" (type0 ~> type0) $ \f -> hasMap f ~~~> typeOfMap f
  FoldTC -> forAll "f" (type0 ~> type0) $ \f -> hasFold f ~~~> typeOfFold f
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
  forAllIrrelevant "A" type0 $ \tCond ->
    forAllIrrelevant "B" type0 $ \tArg1 ->
      forAllIrrelevant "C" type0 $ \tArg2 ->
        forAll "D" type0 $ \tRes ->
          hasIf tCond tArg1 tArg2 tRes
            .~~~> tCond
            ~> tArg1
            ~> tArg2
            ~> tRes

typeOfTCOp1 :: (DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp1 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCOp2 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      forAll "C" type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfTCComparisonOp :: (DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr) -> DSLExpr
typeOfTCComparisonOp constraint =
  forAllIrrelevant "l" tLin $ \l ->
    forAll "A" type0 $ \t1 ->
      forAll "B" type0 $ \t2 ->
        constraint l t1 t2 ~~~> typeOfComparisonOp l t1 t2

typeOfComparisonOp :: DSLExpr -> DSLExpr -> DSLExpr -> DSLExpr
typeOfComparisonOp l t1 t2 =
  t1 ~> t2 ~> tAnnBool l unquantified

typeOfForeach :: DSLExpr
typeOfForeach =
  forAll "A" type0 $ \tRes ->
    forAll "n" tNat $ \d ->
      (tIndex d ~> tRes) ~> tVector tRes d

typeOfNil :: DSLExpr
typeOfNil =
  forAll "A" type0 $ \tElem ->
    tList tElem

typeOfCons :: DSLExpr
typeOfCons =
  forAll "A" type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: DSLExpr
typeOfAt =
  forAll "A" type0 $ \tElem ->
    forAll "n" tNat $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfMap :: DSLExpr -> DSLExpr
typeOfMap f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b) ~> f @@ [a] ~> f @@ [b]

typeOfFold :: DSLExpr -> DSLExpr
typeOfFold f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b ~> b) ~> b ~> f @@ [a] ~> b

typeOfQuantifier :: Quantifier -> DSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      hasQuantifier q tLam tRes ~~~> tLam ~> tRes

typeOfQuantifierIn :: Quantifier -> DSLExpr
typeOfQuantifierIn q =
  forAll "A" type0 $ \tElem ->
    forAll "B" type0 $ \tCont ->
      forAll "C" type0 $ \tRes ->
        hasQuantifierIn q tElem tCont tRes ~~~> (tElem ~> tRes) ~> tCont ~> tRes

typeOfFromNat :: Int -> DSLExpr -> DSLExpr
typeOfFromNat n t = natInDomainConstraint n t .~~~> tNat ~> t

typeOfFromRat :: DSLExpr -> DSLExpr
typeOfFromRat t = tAnnRat constant ~> t

typeOfOrder :: OrderDomain -> OrderOp -> DSLExpr
typeOfOrder domain _op = case domain of
  OrderIndex {} ->
    forAll "n1" tNat $ \n1 ->
      forAll "n2" tNat $ \n2 ->
        tIndex n1 ~> tIndex n2 ~> tAnnBool constant unquantified
  OrderNat {} ->
    tNat ~> tNat ~> tAnnBool constant unquantified
  OrderInt {} ->
    tInt ~> tInt ~> tAnnBool constant unquantified
  OrderRat {} ->
    forAllLinearityTriples $ \l1 l2 l3 ->
      maxLinearity l1 l2 l3 .~~~> tAnnRat l1 ~> tAnnRat l2 ~> tAnnBool l3 unquantified

-- | Return the type of the provided literal,
standardTypeOfLiteral :: Provenance -> Literal -> CheckedType Builtin
standardTypeOfLiteral p l = fromDSL p $ case l of
  LUnit -> tUnit
  LBool _ -> tAnnBool constant unquantified
  LIndex n _ -> tIndex (natLit n)
  LNat {} -> tNat
  LInt {} -> tInt
  LRat {} -> tAnnRat constant

standardTypeOfVectorLiteral ::
  Provenance ->
  [TypeCheckedType] ->
  TypeCheckedType
standardTypeOfVectorLiteral p typesOfElems = do
  -- Create the new type.
  -- Roughly [x1, ..., xn] has type
  --  forall {A} .{{TypesEqual A [t1, ..., tn]}} . Vector tElem n
  let liftedTypesOfElems = liftDBIndices 1 <$> typesOfElems
  let typesOfElemsSeq = mkList p (TypeUniverse p 0) liftedTypesOfElems
  let tc = AlmostEqualConstraint
  let elemsTC tElem = BuiltinTypeClass p tc (ExplicitArg p <$> [tElem, typesOfElemsSeq])
  let typeOfContainer =
        Pi p (Binder p (BinderDisplayForm (OnlyName "A") False) (Implicit False) Relevant () (TypeUniverse p 0)) $
          Pi p (Binder p (BinderDisplayForm OnlyType False) (Instance False) Irrelevant () (elemsTC (Var p (Bound 0)))) $
            VectorType p (Var p (Bound 1)) (NatLiteral p (length typesOfElems))

  -- Return the result
  typeOfContainer

handleStandardTypingError :: MonadCompile m => TypingError Builtin -> m a
handleStandardTypingError = \case
  MissingExplicitArgument boundCtx expectedBinder actualArg ->
    throwError $ MissingExplicitArg (boundContextOf boundCtx) actualArg (typeOf expectedBinder)
  FunctionTypeMismatch boundCtx fun originalArgs nonPiType args -> do
    let p = provenanceOf fun
    let mkRes =
          [ Endo $ \tRes -> pi Nothing (visibilityOf arg) (relevanceOf arg) (tHole ("arg" <> pack (show i))) (const tRes)
            | (i, arg) <- zip [0 :: Int ..] args
          ]
    let expectedType = fromDSL p (appEndo (mconcat mkRes) (tHole "res"))
    let currentFun = normAppList p fun (take (length args) originalArgs)
    throwError $ FunTypeMismatch p (boundContextOf boundCtx) currentFun nonPiType expectedType
  FailedUnification constraints ->
    throwError (FailedUnificationConstraints constraints)
  UnsolvableConstraints constraints ->
    throwError $ UnsolvedConstraints constraints

getStandardPropertyInfo :: MonadCompile m => StandardTypedDecl -> m PropertyInfo
getStandardPropertyInfo property = do
  propertyInfo <- go (normalised (glued $ typeOf property))
  return propertyInfo
  where
    go :: MonadCompile m => StandardNormType -> m PropertyInfo
    go = \case
      VTensorType tElem _ -> go tElem
      VVectorType tElem _ -> go tElem
      VAnnBoolType (VLinearityExpr lin) (VPolarityExpr pol) ->
        return $ PropertyInfo lin pol
      _ -> do
        let declProv = (identifierOf property, provenanceOf property)
        throwError $ PropertyTypeUnsupported declProv (glued $ typeOf property)

relevanceOfTypeClass :: MonadCompile m => Builtin -> m Relevance
relevanceOfTypeClass b = do
  tc <- getTypeClass b
  return $ relevanceOf tc
