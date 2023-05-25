module Vehicle.Compile.Type.Subsystem.Polarity.Type
  ( typePolarityBuiltin,
    handlePolarityTypingError,
    relevanceOfTypeClass,
    freshPolarityMeta,
    convertToPolarityTypes,
  )
where

import Data.Text qualified as Text
import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad.Class (MonadTypeChecker, freshMeta)
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.DSL
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised
import Prelude hiding (pi)

-- | Return the type of the provided builtin.
typePolarityBuiltin :: Provenance -> PolarityBuiltin -> NormalisableType PolarityType
typePolarityBuiltin p b = fromDSL p $ case b of
  CConstructor c -> typeOfConstructor c
  CFunction f -> typeOfBuiltinFunction f
  CType t -> case t of
    Polarity {} -> tPol
    PolarityTypeClass tc -> typeOfPolarityTypeClass tc

typeOfBuiltinFunction :: BuiltinFunction -> PolarityDSLExpr
typeOfBuiltinFunction = \case
  -- Boolean operations
  Not -> typeOfOp1 negPolarity
  Implies -> typeOfOp2 impliesPolarity
  And -> typeOfOp2 maxPolarity
  Or -> typeOfOp2 maxPolarity
  Quantifier q _ -> typeOfQuantifier q
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
  -- Conversion functions
  FromNat {} -> unquantified ~> unquantified
  FromRat {} -> unquantified ~> unquantified
  -- Container functions
  ConsVector -> typeOfOp2 maxPolarity
  Fold dom -> case dom of
    FoldVector -> typeOfFoldVector
    FoldList -> typeOfFoldList
  At -> forAllPolarities $ \p -> p ~> unquantified ~> p
  Indices -> unquantified ~> unquantified

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

typeOfPolarityTypeClass :: PolarityTypeClass -> PolarityDSLExpr
typeOfPolarityTypeClass = \case
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
    constraint p1 p2 ~~~> p1 ~> p2

typeOfOp2 ::
  (PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr -> PolarityDSLExpr) ->
  PolarityDSLExpr
typeOfOp2 constraint =
  forAllPolarityTriples $ \l1 l2 l3 ->
    constraint l1 l2 l3 ~~~> l1 ~> l2 ~> l3

typeOfIf :: PolarityDSLExpr
typeOfIf =
  forAllPolarityTriples $ \pCond pArg1 pArg2 ->
    forAllPolarities $ \pRes ->
      ifPolarity pCond pArg1 pArg2 pRes
        ~~~> pCond
        ~> pArg1
        ~> pArg2
        ~> pRes

typeOfNil :: PolarityDSLExpr
typeOfNil = unquantified

typeOfCons :: PolarityDSLExpr
typeOfCons = typeOfOp2 maxPolarity

typeOfFoldVector :: PolarityDSLExpr
typeOfFoldVector =
  forAllPolarityTriples $ \p1 p2 p3 ->
    maxPolarity p1 p2 p3 ~~~> (unquantified ~~> p1 ~> p2 ~> p2) ~> p2 ~> p1 ~> p3

typeOfFoldList :: PolarityDSLExpr
typeOfFoldList =
  forAllPolarityTriples $ \p1 p2 p3 ->
    maxPolarity p1 p2 p3 ~~~> (p1 ~> p2 ~> p2) ~> p2 ~> p1 ~> p3

typeOfQuantifier :: Quantifier -> PolarityDSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantifierPolarity q tLam tRes
        ~~~> tLam
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
                ~~~> li
                ~> go (i - 1) newMax

handlePolarityTypingError :: (MonadCompile m) => TypingError PolarityType -> m a
handlePolarityTypingError b =
  compilerDeveloperError $ "Polarity type system should not be throwing error:" <+> pretty b

relevanceOfTypeClass :: (MonadCompile m) => PolarityType -> m Relevance
relevanceOfTypeClass _b = return Relevant

freshPolarityMeta :: (MonadTypeChecker PolarityType m) => Provenance -> m (GluedExpr PolarityType)
freshPolarityMeta p = snd <$> freshMeta p (TypeUniverse p 0) mempty

convertToPolarityTypes ::
  forall m.
  (MonadTypeChecker PolarityType m) =>
  Provenance ->
  StandardBuiltinType ->
  [NormalisableArg PolarityType] ->
  m (NormalisableExpr PolarityType)
convertToPolarityTypes p b args = case b of
  StandardBuiltinType s -> case s of
    Unit -> return $ PolarityExpr p Unquantified
    Bool -> unnormalised <$> freshPolarityMeta p
    Index -> return $ PolarityExpr p Unquantified
    Nat -> return $ PolarityExpr p Unquantified
    Int -> return $ PolarityExpr p Unquantified
    Rat -> unnormalised <$> freshPolarityMeta p
    List -> case args of
      [tElem] -> return $ argExpr tElem
      _ -> monomorphisationError "List"
    Vector -> case args of
      [tElem, _] -> return $ argExpr tElem
      _ -> monomorphisationError "Vector"
  StandardTypeClass {} ->
    monomorphisationError "TypeClass"
  StandardTypeClassOp {} ->
    compilerDeveloperError "Type class operations should have been resolved before converting to other type systems"
  where
    monomorphisationError :: Doc () -> m a
    monomorphisationError name =
      compilerDeveloperError $
        "Monomorphisation should have got rid of partially applied" <+> name <+> "types."
