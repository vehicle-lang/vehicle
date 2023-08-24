{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Backend.Queries.Error.Linearity.Type
  ( typeLinearityBuiltin,
  )
where

import Data.Text qualified as Text
import Vehicle.Backend.Queries.Error.Linearity.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard.Type (typeOfAnn)
import Vehicle.Expr.DSL
import Vehicle.Syntax.Builtin hiding (Builtin (..))

-- | Return the type of the provided builtin.
typeLinearityBuiltin :: Provenance -> LinearityBuiltin -> Type Ix LinearityBuiltin
typeLinearityBuiltin p b = fromDSL p $ case b of
  BuiltinConstructor c -> typeOfConstructor c
  BuiltinFunction f -> typeOfBuiltinFunction f
  Linearity {} -> tLin
  LinearityRelation r -> typeOfLinearityRelation r

typeOfBuiltinFunction :: BuiltinFunction -> LinearityDSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfOp1
  Implies -> typeOfOp2 maxLinearity
  And -> typeOfOp2 maxLinearity
  Or -> typeOfOp2 maxLinearity
  Quantifier q -> typeOfQuantifier q
  If -> typeOfIf
  -- Arithmetic operations
  Neg {} -> typeOfOp1
  Add {} -> typeOfOp2 maxLinearity
  Sub {} -> typeOfOp2 maxLinearity
  Mul {} -> typeOfOp2 mulLinearity
  Div {} -> typeOfOp2 divLinearity
  PowRat {} -> typeOfOp2 powLinearity
  MinRat {} -> typeOfOp2 maxLinearity
  MaxRat {} -> typeOfOp2 maxLinearity
  -- Comparisons
  Equals {} -> typeOfOp2 maxLinearity
  Order {} -> typeOfOp2 maxLinearity
  -- Conversion functions
  FromNat {} -> constant ~> constant
  FromRat {} -> constant ~> constant
  -- Container functions
  ConsVector -> typeOfOp2 maxLinearity
  Fold {} -> typeOfFold
  MapList -> typeOfMap
  MapVector -> typeOfMap
  ZipWithVector -> typeOfZipWith
  At -> typeOfAt
  Indices -> constant ~> constant
  b@Optimise {} -> developerError $ "Should not be linearity typing" <+> pretty b
  Ann -> typeOfAnn

typeOfConstructor :: BuiltinConstructor -> LinearityDSLExpr
typeOfConstructor = \case
  Nil -> typeOfNil
  Cons -> typeOfCons
  LUnit {} -> constant
  LBool {} -> constant
  LIndex {} -> constant
  LNat {} -> constant
  LInt {} -> constant
  LRat {} -> constant
  LVec n -> typeOfVecLiteral n

typeOfLinearityRelation :: LinearityRelation -> LinearityDSLExpr
typeOfLinearityRelation = \case
  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity -> tLin ~> tLin ~> tLin ~> type0
  DivLinearity -> tLin ~> tLin ~> tLin ~> type0
  PowLinearity -> tLin ~> tLin ~> tLin ~> type0
  FunctionLinearity {} -> tLin ~> tLin ~> type0
  QuantifierLinearity {} -> (tLin ~> tLin) ~> tLin ~> type0

typeOfOp1 :: LinearityDSLExpr
typeOfOp1 = forAllLinearities $ \l -> l ~> l

typeOfOp2 ::
  (LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr) ->
  LinearityDSLExpr
typeOfOp2 constraint =
  forAllLinearityTriples $ \l1 l2 l3 ->
    constraint l1 l2 l3 .~~~> l1 ~> l2 ~> l3

typeOfIf :: LinearityDSLExpr
typeOfIf =
  forAllLinearityTriples $ \lCond lArg1 lArg2 ->
    forAllLinearities $ \lArgs ->
      forAllLinearities $ \lRes ->
        maxLinearity lCond lArgs lRes
          .~~~> maxLinearity lArg1 lArg2 lArgs
          .~~~> lCond
          ~> lArg1
          ~> lArg2
          ~> lRes

typeOfNil :: LinearityDSLExpr
typeOfNil = constant

typeOfCons :: LinearityDSLExpr
typeOfCons = typeOfOp2 maxLinearity

typeOfAt :: LinearityDSLExpr
typeOfAt = forAllLinearities $ \l -> l ~> constant ~> l

typeOfFold :: LinearityDSLExpr
typeOfFold =
  forAllLinearityTriples $ \l1 l2 l3 ->
    maxLinearity l1 l2 l3 .~~~> (l1 ~> l2 ~> l3) ~> l2 ~> l1 ~> l3

typeOfMap :: LinearityDSLExpr
typeOfMap =
  forAllLinearities $ \l1 ->
    forAllLinearities $ \l2 ->
      (l1 ~> l2) ~> l1 ~> l2

typeOfZipWith :: LinearityDSLExpr
typeOfZipWith =
  forAllLinearityTriples $ \l1 l2 l3 ->
    maxLinearity l1 l2 l3 .~~~> (l1 ~> l2 ~> l3) ~> l1 ~> l2 ~> l3

typeOfQuantifier :: Quantifier -> LinearityDSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantLinearity q tLam tRes .~~~> tLam ~> tRes

typeOfVecLiteral :: Int -> LinearityDSLExpr
typeOfVecLiteral n = go n constant
  where
    go :: Int -> LinearityDSLExpr -> LinearityDSLExpr
    go 0 maxSoFar = maxSoFar
    go i maxSoFar =
      let varName = "l" <> Text.pack (show i)
       in forAll varName tLin $ \li ->
            forAll (varName <> "_max") tLin $ \newMax ->
              maxLinearity maxSoFar li newMax
                .~~~> li
                ~> go (i - 1) newMax
