module Vehicle.Compile.Type.Subsystem.Standard.Type
  ( typeStandardBuiltin,
    handleStandardTypingError,
    relevanceOfTypeClass,
    typeOfTypeClassOp,
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
import Vehicle.Expr.Normalisable (NormalisableBuiltin (..))
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeStandardBuiltin :: Provenance -> StandardBuiltin -> StandardType
typeStandardBuiltin p b = fromDSL p $ case b of
  CConstructor c -> typeOfConstructor c
  CFunction f -> typeOfBuiltinFunction f
  CType t -> case t of
    StandardTypeClassOp tcOp -> typeOfTypeClassOp tcOp
    StandardTypeClass tc -> typeOfTypeClass tc
    StandardBuiltinType s -> typeOfBuiltinType s

typeOfBuiltinFunction :: BuiltinFunction -> StandardDSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> tBool ~> tBool
  Implies -> tBool ~> tBool ~> tBool
  And -> tBool ~> tBool ~> tBool
  Or -> tBool ~> tBool ~> tBool
  Quantifier _ dom -> case dom of
    QuantNat -> typeOfQuantifier (tNat ~> tBool)
    QuantInt -> typeOfQuantifier (tInt ~> tBool)
    QuantRat -> typeOfQuantifier (tRat ~> tBool)
    QuantVec -> developerError "Unsure about type of quantified vectors"
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
  -- Comparisons
  Equals dom _op -> case dom of
    EqIndex {} ->
      forAll "n1" tNat $ \n1 ->
        forAll "n2" tNat $ \n2 ->
          typeOfComparisonOp (tIndex n1) (tIndex n2)
    EqNat {} -> typeOfComparisonOp tNat tNat
    EqInt {} -> typeOfComparisonOp tInt tInt
    EqRat {} -> typeOfComparisonOp tRat tRat
  Order dom _op -> case dom of
    OrderIndex {} ->
      forAll "n1" tNat $ \n1 ->
        forAll "n2" tNat $ \n2 ->
          tIndex n1 ~> tIndex n2 ~> tBool
    OrderNat {} -> tNat ~> tNat ~> tBool
    OrderInt {} -> tInt ~> tInt ~> tBool
    OrderRat {} -> tInt ~> tInt ~> tBool
  -- Conversion functions
  FromNat dom -> case dom of
    FromNatToIndex -> forAllNat $ \s -> typeOfFromNat (tIndex s)
    FromNatToNat -> typeOfFromNat tNat
    FromNatToInt -> typeOfFromNat tInt
    FromNatToRat -> typeOfFromNat tRat
  FromRat dom -> case dom of
    FromRatToRat -> typeOfFromRat tRat
  -- Container functions
  ConsVector -> typeOfConsVector
  Fold dom -> case dom of
    FoldList -> typeOfFold tListRaw
    FoldVector -> typeOfFoldVector
  At -> typeOfAt
  Indices -> typeOfIndices

typeOfBuiltinType :: BuiltinType -> StandardDSLExpr
typeOfBuiltinType = \case
  Unit -> type0
  Nat -> type0
  Int -> type0
  Rat -> type0
  Bool -> type0
  List -> type0 ~> type0
  Vector -> type0 ~> tNat ~> type0
  Index -> tNat ~> type0

typeOfConstructor :: BuiltinConstructor -> StandardDSLExpr
typeOfConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  LUnit -> tUnit
  LBool _ -> tBool
  LIndex x -> forAllNat $ \n -> natInDomainConstraint (natLit x) n .~~~> tIndex n
  LNat {} -> tNat
  LInt {} -> tInt
  LRat {} -> tRat
  LVec n -> typeOfVectorLiteral n

typeOfTypeClass :: TypeClass -> StandardDSLExpr
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
  NatInDomainConstraint {} -> forAll "A" type0 $ \t -> tNat ~> t ~> type0

typeOfTypeClassOp :: TypeClassOp -> StandardDSLExpr
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
  FromVecTC -> forAll "n" tNat $ \n -> forAll "f" (type0 ~> type0) $ \f -> hasVecLits n f ~~~> typeOfFromVec n f
  MapTC -> forAll "f" (type0 ~> type0) $ \f -> hasMap f ~~~> typeOfMap f
  FoldTC -> forAll "f" (type0 ~> type0) $ \f -> hasFold f ~~~> typeOfFold f
  QuantifierTC q ->
    forAll "A" (type0 ~> type0) $ \t ->
      hasQuantifier q t ~~~> typeOfQuantifier t

typeOfIf :: StandardDSLExpr
typeOfIf =
  forAll "A" type0 $ \t ->
    tBool ~> t ~> t ~> t

typeOfTCOp1 :: (StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr) -> StandardDSLExpr
typeOfTCOp1 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> t1 ~> t2

typeOfTCOp2 :: (StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr) -> StandardDSLExpr
typeOfTCOp2 constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      forAll "C" type0 $ \t3 ->
        constraint t1 t2 t3 ~~~> t1 ~> t2 ~> t3

typeOfTCComparisonOp :: (StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr) -> StandardDSLExpr
typeOfTCComparisonOp constraint =
  forAll "A" type0 $ \t1 ->
    forAll "B" type0 $ \t2 ->
      constraint t1 t2 ~~~> typeOfComparisonOp t1 t2

typeOfComparisonOp :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
typeOfComparisonOp t1 t2 = t1 ~> t2 ~> tBool

typeOfIndices :: StandardDSLExpr
typeOfIndices =
  pi (Just "n") Explicit Relevant tNat $ \n -> tVector (tIndex n) n

typeOfNil :: StandardDSLExpr
typeOfNil =
  forAll "A" type0 $ \tElem ->
    tList tElem

typeOfCons :: StandardDSLExpr
typeOfCons =
  forAll "A" type0 $ \tElem ->
    tElem ~> tList tElem ~> tList tElem

typeOfAt :: StandardDSLExpr
typeOfAt =
  forAll "A" type0 $ \tElem ->
    forAll "n" tNat $ \tDim ->
      tVector tElem tDim ~> tIndex tDim ~> tElem

typeOfMap :: StandardDSLExpr -> StandardDSLExpr
typeOfMap f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b) ~> f @@ [a] ~> f @@ [b]

typeOfFold :: StandardDSLExpr -> StandardDSLExpr
typeOfFold f =
  forAll "A" type0 $ \a ->
    forAll "B" type0 $ \b ->
      (a ~> b ~> b) ~> b ~> f @@ [a] ~> b

typeOfConsVector :: StandardDSLExpr
typeOfConsVector =
  forAll "A" type0 $ \a ->
    forAll "n" tNat $ \n ->
      a ~> tVector a n ~> tVector a (addNat n (natLit 1))

typeOfFoldVector :: StandardDSLExpr
typeOfFoldVector =
  forAll "A" type0 $ \a ->
    forAll "n" tNat $ \n ->
      forAll "P" (tNat ~> type0) $ \p ->
        forAll "l" tNat (\l -> a ~> p @@ [l] ~> p @@ [addNat l (natLit 1)])
          ~> p
          @@ [natLit 0]
          ~> tVector a n
          ~> p
          @@ [n]

typeOfQuantifier :: StandardDSLExpr -> StandardDSLExpr
typeOfQuantifier t = t ~> tBool

typeOfFromNat :: StandardDSLExpr -> StandardDSLExpr
typeOfFromNat t = forAllExpl "n" tNat $ \n -> natInDomainConstraint n t .~~~> t

typeOfFromRat :: StandardDSLExpr -> StandardDSLExpr
typeOfFromRat t = tRat ~> t

typeOfFromVec :: StandardDSLExpr -> StandardDSLExpr -> StandardDSLExpr
typeOfFromVec n f =
  forAll "A" type0 $ \t ->
    tVector t n ~> f @@ [t]

typeOfVectorLiteral :: Int -> StandardDSLExpr
typeOfVectorLiteral n = do
  forAll "A" type0 $ \tElem ->
    naryFunc n tElem (tVector tElem (natLit n))

handleStandardTypingError :: (MonadCompile m) => TypingError StandardBuiltinType -> m a
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

relevanceOfTypeClass :: (MonadCompile m) => StandardBuiltinType -> m Relevance
relevanceOfTypeClass b = do
  tc <- getTypeClass b
  return $ relevanceOf tc
