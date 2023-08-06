module Vehicle.Compile.Type.Subsystem.Polarity.Type
  ( typePolarityBuiltin,
  )
where

import Data.Text qualified as Text
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Expr.DSL
import Vehicle.Expr.DeBruijn
import Vehicle.Prelude
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin hiding (Builtin (..))
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typePolarityBuiltin :: Provenance -> PolarityBuiltin -> Type Ix PolarityBuiltin
typePolarityBuiltin p b = fromDSL p $ case b of
  BuiltinConstructor c -> typeOfConstructor c
  BuiltinFunction f -> typeOfBuiltinFunction f
  Polarity {} -> tPol
  PolarityRelation r -> typeOfPolarityRelation r

typeOfBuiltinFunction :: BuiltinFunction -> PolarityDSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfOp1 negPolarity
  Implies -> typeOfOp2 impliesPolarity
  And -> typeOfOp2 maxPolarity
  Or -> typeOfOp2 maxPolarity
  Quantifier q -> typeOfQuantifier q
  If -> typeOfIf
  -- Comparisons
  Equals {} -> typeOfOp2 maxPolarity
  Order {} -> typeOfOp2 maxPolarity
  -- Arithmetic operations
  Neg {} -> unquantified ~> unquantified
  Add {} -> unquantified ~> unquantified ~> unquantified
  Sub {} -> unquantified ~> unquantified ~> unquantified
  Mul {} -> unquantified ~> unquantified ~> unquantified
  Div {} -> unquantified ~> unquantified ~> unquantified
  PowRat {} -> unquantified ~> unquantified ~> unquantified
  MinRat {} -> unquantified ~> unquantified ~> unquantified
  MaxRat {} -> unquantified ~> unquantified ~> unquantified
  -- Conversion functions
  FromNat {} -> unquantified ~> unquantified
  FromRat {} -> unquantified ~> unquantified
  -- Container functions
  ConsVector -> typeOfOp2 maxPolarity
  Fold {} -> typeOfFold
  MapList -> typeOfMap
  MapVector -> typeOfMap
  ZipWithVector -> typeOfZipWith
  At -> forAllPolarities $ \p -> p ~> unquantified ~> p
  Indices -> unquantified ~> unquantified
  b@Sample {} -> developerError $ "Should not be polarity typing" <+> pretty b

typeOfConstructor :: BuiltinConstructor -> PolarityDSLExpr
typeOfConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  LUnit {} -> unquantified
  LBool {} -> unquantified
  LIndex {} -> unquantified
  LNat {} -> unquantified
  LInt {} -> unquantified
  LRat {} -> unquantified
  LVec n -> typeOfVecLiteral n

typeOfPolarityRelation :: PolarityRelation -> PolarityDSLExpr
typeOfPolarityRelation = \case
  NegPolarity -> tPol ~> tPol ~> type0
  ImpliesPolarity -> tPol ~> tPol ~> tPol ~> type0
  EqPolarity {} -> tPol ~> tPol ~> tPol ~> type0
  IfPolarity -> tPol ~> tPol ~> tPol ~> tPol ~> type0
  MaxPolarity -> tPol ~> tPol ~> tPol ~> type0
  AddPolarity {} -> tPol ~> tPol ~> type0
  QuantifierPolarity {} -> (tPol ~> tPol) ~> tPol ~> type0
  FunctionPolarity {} -> tPol ~> tPol ~> type0

typeOfOp1 ::
  (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) ->
  PolarityDSLExpr
typeOfOp1 constraint =
  forAllPolarityPairs $ \p1 p2 ->
    constraint p1 p2 .~~~> p1 ~> p2

typeOfOp2 ::
  (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) ->
  PolarityDSLExpr
typeOfOp2 constraint =
  forAllPolarityTriples $ \l1 l2 l3 ->
    constraint l1 l2 l3 .~~~> l1 ~> l2 ~> l3

typeOfIf :: PolarityDSLExpr
typeOfIf =
  forAllPolarityTriples $ \pCond pArg1 pArg2 ->
    forAllPolarities $ \pRes ->
      ifPolarity pCond pArg1 pArg2 pRes
        .~~~> pCond
        ~> pArg1
        ~> pArg2
        ~> pRes

typeOfNil :: PolarityDSLExpr
typeOfNil = unquantified

typeOfCons :: PolarityDSLExpr
typeOfCons = typeOfOp2 maxPolarity

typeOfFold :: PolarityDSLExpr
typeOfFold =
  forAllPolarityTriples $ \p1 p2 p3 ->
    maxPolarity p1 p2 p3 .~~~> (p1 ~> p2 ~> p2) ~> p2 ~> p1 ~> p3

typeOfMap :: PolarityDSLExpr
typeOfMap =
  forAllPolarities $ \p1 ->
    forAllPolarities $ \p2 ->
      (p1 ~> p2) ~> p1 ~> p2

typeOfZipWith :: PolarityDSLExpr
typeOfZipWith =
  forAllPolarityTriples $ \p1 p2 p3 ->
    maxPolarity p1 p2 p3 .~~~> (p1 ~> p2 ~> p3) ~> p1 ~> p2 ~> p3

typeOfQuantifier :: Quantifier -> PolarityDSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantifierPolarity q tLam tRes
        .~~~> tLam
        ~> tRes

typeOfVecLiteral :: Int -> PolarityDSLExpr
typeOfVecLiteral n = go n unquantified
  where
    go :: Int -> PolarityDSLExpr -> PolarityDSLExpr
    go 0 maxSoFar = maxSoFar
    go i maxSoFar =
      let varName = "l" <> Text.pack (show i)
       in forAll varName tPol $ \li ->
            forAll (varName <> "_max") tPol $ \newMax ->
              maxPolarity maxSoFar li newMax
                .~~~> li
                ~> go (i - 1) newMax
