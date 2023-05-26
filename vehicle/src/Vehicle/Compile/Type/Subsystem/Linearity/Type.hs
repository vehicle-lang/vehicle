module Vehicle.Compile.Type.Subsystem.Linearity.Type
  ( typeLinearityBuiltin,
    handleLinearityTypingError,
    relevanceOfTypeClass,
    freshLinearityMeta,
    convertToLinearityTypes,
  )
where

import Data.Text qualified as Text
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class (MonadTypeChecker, freshMeta)
import Vehicle.Compile.Type.Subsystem.Linearity.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DSL
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typeLinearityBuiltin :: Provenance -> NormalisableBuiltin LinearityType -> NormalisableType LinearityType
typeLinearityBuiltin p b = fromDSL p $ case b of
  CConstructor c -> typeOfConstructor c
  CFunction f -> typeOfBuiltinFunction f
  CType t -> typeOfLinearityType t

typeOfBuiltinFunction :: BuiltinFunction -> LinearityDSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfOp1
  Implies -> typeOfOp2 maxLinearity
  And -> typeOfOp2 maxLinearity
  Or -> typeOfOp2 maxLinearity
  Quantifier q _dom -> typeOfQuantifier q
  If -> typeOfIf
  -- Arithmetic operations
  Neg {} -> typeOfOp1
  Add {} -> typeOfOp2 maxLinearity
  Sub {} -> typeOfOp2 maxLinearity
  Mul {} -> typeOfOp2 mulLinearity
  Div {} -> typeOfOp2 mulLinearity
  -- Comparisons
  Equals {} -> typeOfOp2 maxLinearity
  Order {} -> typeOfOp2 maxLinearity
  -- Conversion functions
  FromNat {} -> constant ~> constant
  FromRat {} -> constant ~> constant
  -- Container functions
  ConsVector -> typeOfOp2 maxLinearity
  Fold dom -> case dom of
    FoldVector -> typeOfFoldVector
    FoldList -> typeOfFoldList
  At -> typeOfAt
  Indices -> constant ~> constant

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

typeOfLinearityType :: LinearityType -> LinearityDSLExpr
typeOfLinearityType = \case
  Linearity {} -> tLin
  LinearityTypeClass tc -> typeOfLinearityTypeClass tc

typeOfLinearityTypeClass :: LinearityTypeClass -> LinearityDSLExpr
typeOfLinearityTypeClass = \case
  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity -> tLin ~> tLin ~> tLin ~> type0
  FunctionLinearity {} -> tLin ~> tLin ~> type0
  QuantifierLinearity {} -> (tLin ~> tLin) ~> tLin ~> type0

typeOfOp1 :: LinearityDSLExpr
typeOfOp1 = forAllLinearities $ \l -> l ~> l

typeOfOp2 ::
  (LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr -> LinearityDSLExpr) ->
  LinearityDSLExpr
typeOfOp2 constraint =
  forAllLinearityTriples $ \l1 l2 l3 ->
    constraint l1 l2 l3 ~~~> l1 ~> l2 ~> l3

typeOfIf :: LinearityDSLExpr
typeOfIf =
  forAllLinearityTriples $ \lCond lArg1 lArg2 ->
    forAllLinearities $ \lArgs ->
      forAllLinearities $ \lRes ->
        maxLinearity lCond lArgs lRes
          ~~~> maxLinearity lArg1 lArg2 lArgs
          ~~~> lCond
          ~> lArg1
          ~> lArg2
          ~> lRes

typeOfNil :: LinearityDSLExpr
typeOfNil = constant

typeOfCons :: LinearityDSLExpr
typeOfCons = typeOfOp2 maxLinearity

typeOfAt :: LinearityDSLExpr
typeOfAt = forAllLinearities $ \l -> l ~> constant ~> l

typeOfFoldList :: LinearityDSLExpr
typeOfFoldList =
  forAllLinearityTriples $ \l1 l2 l3 ->
    maxLinearity l1 l2 l3 ~~~> (l1 ~> l2 ~> l2) ~> l2 ~> l1 ~> l3

typeOfFoldVector :: LinearityDSLExpr
typeOfFoldVector =
  forAllLinearityTriples $ \l1 l2 l3 ->
    maxLinearity l1 l2 l3 ~~~> (constant ~~> l1 ~> l2 ~> l2) ~> l2 ~> l1 ~> l3

typeOfQuantifier :: Quantifier -> LinearityDSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantLinearity q tLam tRes ~~~> tLam ~> tRes

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
                ~~~> li
                ~> go (i - 1) newMax

handleLinearityTypingError :: (MonadCompile m) => TypingError LinearityType -> m a
handleLinearityTypingError b =
  compilerDeveloperError $ "Linearity type system should not be throwing error:" <+> pretty b

relevanceOfTypeClass :: (MonadCompile m) => LinearityType -> m Relevance
relevanceOfTypeClass _b = return Relevant

freshLinearityMeta :: (MonadTypeChecker LinearityType m) => Provenance -> m (GluedExpr LinearityType)
freshLinearityMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

convertToLinearityTypes ::
  forall m.
  (MonadTypeChecker LinearityType m) =>
  Provenance ->
  StandardBuiltinType ->
  [NormalisableArg LinearityType] ->
  m (NormalisableExpr LinearityType)
convertToLinearityTypes p b args = case b of
  StandardBuiltinType s -> case s of
    Unit -> return $ Builtin p $ CType $ Linearity Constant
    Bool -> unnormalised <$> freshLinearityMeta p
    Index -> unnormalised <$> freshLinearityMeta p
    Nat -> unnormalised <$> freshLinearityMeta p
    Int -> unnormalised <$> freshLinearityMeta p
    Rat -> unnormalised <$> freshLinearityMeta p
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem, _] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  StandardTypeClass {} -> monomorphisationError "TypeClass"
  StandardTypeClassOp {} ->
    compilerDeveloperError "Type class operations should have been resolved before converting to other type systems"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types."
