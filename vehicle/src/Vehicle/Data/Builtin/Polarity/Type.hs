{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Polarity.Type
  ( typePolarityBuiltin,
    isPolarityBuiltinConstructor,
  )
where

import Vehicle.Compile.Context.Free (getFreeEnv)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Bidirectional (createFreshUnificationConstraint)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core hiding (Builtin (..))
import Vehicle.Data.Builtin.Interface.InputOutputInsertion
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Builtin.Polarity.Solver (solvePolarityConstraint)
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Code.DSL (iterate)
import Vehicle.Data.DSL
import Prelude hiding (iterate, pi)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

instance TypableBuiltin PolarityBuiltin where
  typeBuiltin p b = return (fromDSL p $ typePolarityBuiltin p b)
  useDependentMetas _ = False
  isConstructor = isPolarityBuiltinConstructor
  isCastConstraint _ = False

isPolarityBuiltinConstructor :: PolarityBuiltin -> Bool
isPolarityBuiltinConstructor = \case
  PolarityConstructor {} -> True
  PolarityFunction {} -> False
  Polarity {} -> True
  PolarityRelation {} -> True

-- | Return the type of the provided builtin.
typePolarityBuiltin :: Provenance -> PolarityBuiltin -> PolarityDSLExpr
typePolarityBuiltin p = \case
  PolarityConstructor c -> typeOfConstructor c
  PolarityFunction f -> typeOfBuiltinFunction p f
  Polarity {} -> tPol
  PolarityRelation r -> typeOfPolarityRelation r

typeOfBuiltinFunction :: Provenance -> BuiltinFunction -> PolarityDSLExpr
typeOfBuiltinFunction p = \case
  -- Boolean operations
  Not {} -> typeOfOp1 negPolarity
  Implies -> typeOfOp2 impliesPolarity
  And {} -> typeOfOp2 maxPolarity
  Or {} -> typeOfOp2 maxPolarity
  ReduceAndTensor -> typeOfOp2 maxPolarity
  ReduceOrTensor -> typeOfOp2 maxPolarity
  QuantifyRatTensor q -> typeOfQuantifier p q
  If -> typeOfIf
  -- Comparisons
  CompareNat {} -> typeOfOp2 maxPolarity
  CompareIndex {} -> typeOfOp2 maxPolarity
  CompareRatTensorPointwise {} -> typeOfOp2 maxPolarity
  -- Arithmetic operations
  Add {} -> typeOfUnquantifiedOp2
  Mul {} -> typeOfUnquantifiedOp2
  Neg {} -> typeOfUnquantifiedOp1
  Sub {} -> typeOfUnquantifiedOp2
  Div {} -> typeOfUnquantifiedOp2
  Min {} -> typeOfUnquantifiedOp2
  Max {} -> typeOfUnquantifiedOp2
  PowRat {} -> typeOfUnquantifiedOp2
  ReduceAddRatTensor -> typeOfUnquantifiedOp2
  ReduceMulRatTensor -> typeOfUnquantifiedOp2
  ReduceMinRatTensor -> typeOfUnquantifiedOp2
  ReduceMaxRatTensor -> typeOfUnquantifiedOp2
  -- Container functions
  FoldList -> typeOfFold
  MapList -> typeOfMap
  AtVector -> typeOfAt
  AtTensor -> typeOfAt
  StackTensor -> typeOfStack
  ConstTensor -> forAllPolarities $ \pol -> pol ~> unquantified ~> pol
  ForeachTensor -> typeOfForeach
  ForeachVector -> typeOfForeach
  Iterate -> typeOfIterate

typeOfConstructor :: BuiltinConstructor -> PolarityDSLExpr
typeOfConstructor = \case
  -- Interesting
  Nil -> typeOfNil
  Cons -> typeOfCons
  -- Uninteresting
  UnitLiteral {} -> unquantified
  IndexLiteral {} -> unquantified
  NatLiteral {} -> unquantified
  VectorLiteral {} -> typeOfVectorLiteral
  NatTensorLiteral {} -> unquantified
  BoolTensorLiteral {} -> unquantified
  RatTensorLiteral {} -> unquantified

typeOfPolarityRelation :: PolarityRelation -> PolarityDSLExpr
typeOfPolarityRelation = \case
  NegPolarity -> tPol ~> tPol ~> type0
  ImpliesPolarity -> tPol ~> tPol ~> tPol ~> type0
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

typeOfUnquantifiedOp1 :: PolarityDSLExpr
typeOfUnquantifiedOp1 = unquantified ~> unquantified

typeOfUnquantifiedOp2 :: PolarityDSLExpr
typeOfUnquantifiedOp2 = unquantified ~> unquantified ~> unquantified

typeOfIf :: PolarityDSLExpr
typeOfIf =
  forAllPolarityTriples $ \pCond pArg1 pArg2 ->
    forAllPolarities $ \pRes ->
      ifPolarity pCond pArg1 pArg2 pRes
        .~~~> pCond
        ~> pArg1
        ~> pArg2
        ~> pRes

typeOfAt :: PolarityDSLExpr
typeOfAt = typeOfOp2 maxPolarity

typeOfForeach :: PolarityDSLExpr
typeOfForeach = forAllPolarities $ \pol -> (unquantified ~> pol) ~> pol

typeOfNil :: PolarityDSLExpr
typeOfNil = unquantified

typeOfCons :: PolarityDSLExpr
typeOfCons = typeOfOp2 maxPolarity

typeOfFold :: PolarityDSLExpr
typeOfFold =
  forAllPolarityTriples $ \p1 p2 p3 ->
    maxPolarity p1 p2 p3 .~~~> (p1 ~> p2 ~> p3) ~> p2 ~> p1 ~> p3

typeOfMap :: PolarityDSLExpr
typeOfMap =
  forAllPolarities $ \p1 ->
    forAllPolarities $ \p2 ->
      (p1 ~> p2) ~> p1 ~> p2

typeOfQuantifier :: Provenance -> Quantifier -> PolarityDSLExpr
typeOfQuantifier p q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantifierPolarity p q tLam tRes
        .~~~> tLam
        ~> tRes

typeOfIterate :: PolarityDSLExpr
typeOfIterate = ((type0 ~> type0) ~> type0 ~> type0) ~> unquantified ~> type0

typeOfVectorLiteral :: PolarityDSLExpr
typeOfVectorLiteral =
  forAll "n" unquantified $ \n ->
    iterate
      type0
      ( \fn maxSoFar ->
          forAll "p" tPol $ \p ->
            forAll "p_max" tPol $ \newMax ->
              maxPolarity maxSoFar p newMax .~~~> p ~> fn @@ [newMax]
      )
      n
      unquantified

typeOfStack :: PolarityDSLExpr
typeOfStack = typeOfVectorLiteral

--------------------------------------------------------------------------------
-- Type system
--------------------------------------------------------------------------------

instance HasTypeSystem PolarityBuiltin where
  convertFromStandardBuiltins = traverseBuiltinsM convertToPolarityTypes
  restrictDeclType = restrictDeclPolarityType
  restrictRecordAnnotatedAsTensor = restrictPolarityRecordAnnotatedAsTensor
  isAuxiliaryConstraint _ = True
  solveAuxiliaryInstanceConstraint = solvePolarityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (PolarityRelation . FunctionPolarity)
  generateDefaultAuxiliaryConstraint _ = return False

pattern PolarityExpr :: Provenance -> Polarity -> Expr PolarityBuiltin
pattern PolarityExpr p pol = Builtin p (Polarity pol)

freshPolarityMeta :: (MonadTypeChecker PolarityBuiltin m) => Provenance -> m (Expr PolarityBuiltin)
freshPolarityMeta p = freshMetaExpr p (TypeUniverse p 0) mempty

convertToPolarityTypes ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  BuiltinUpdate m Builtin PolarityBuiltin
convertToPolarityTypes p b args = case b of
  BuiltinFunction f -> do
    let args' = case f of
          StackTensor -> implicit (Builtin p (PolarityConstructor (NatLiteral (length args)))) : args
          _ -> args
    return $ normAppList (Builtin p (PolarityFunction f)) args'
  BuiltinConstructor c -> return $ normAppList (Builtin p (PolarityConstructor c)) args
  BuiltinType s -> case s of
    UnitType -> return $ PolarityExpr p Unquantified
    RatType {} -> freshPolarityMeta p
    BoolType {} -> freshPolarityMeta p
    IndexType -> return $ PolarityExpr p Unquantified
    NatType -> return $ PolarityExpr p Unquantified
    ListType -> return $ extractElementType b args
    VectorType -> return $ extractElementType b args
    TensorType -> return $ extractElementType b args
  DerivedFunction f -> return $ normAppList (FreeVar p (identifierOf f)) args
  TypeClass {} -> monomorphisationError b args
  BuiltinCast {} -> monomorphisationError b args
  TypeClassOp {} -> monomorphisationError b args
  NatInDomainConstraint -> monomorphisationError b args

restrictDeclPolarityType ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type PolarityBuiltin ->
  m (Type PolarityBuiltin)
restrictDeclPolarityType rDecl declProv declType = do
  freeEnv <- getFreeEnv
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv declProv (Left rDecl) declType

  case rDecl of
    RestrictedNetwork -> restrictPolarityNetworkType origin declProv declType
    RestrictedDataset -> assertUnquantifiedPolarity origin declProv declType
    RestrictedParameter {} -> assertUnquantifiedPolarity origin declProv declType
    RestrictedProperty -> return declType

restrictPolarityNetworkType ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  InstanceConstraintOrigin PolarityBuiltin ->
  DeclProvenance ->
  Type PolarityBuiltin ->
  m (Type PolarityBuiltin)
restrictPolarityNetworkType origin (_, p) networkType = do
  let inputPol = PolarityExpr p Unquantified
  let outputPol = PolarityExpr p Unquantified

  let inputPolBinder = Binder p (BinderDisplayForm OnlyType False) Explicit Relevant inputPol
  let functionNetworkType = Pi p inputPolBinder outputPol
  createFreshUnificationConstraint p mempty (CheckingInstanceType origin) networkType functionNetworkType
  return networkType

assertUnquantifiedPolarity ::
  (MonadTypeChecker PolarityBuiltin m) =>
  InstanceConstraintOrigin PolarityBuiltin ->
  DeclProvenance ->
  Type PolarityBuiltin ->
  m (Type PolarityBuiltin)
assertUnquantifiedPolarity origin (_, p) t = do
  createFreshUnificationConstraint p mempty (CheckingInstanceType origin) (PolarityExpr p Unquantified) t
  return t

restrictPolarityRecordAnnotatedAsTensor ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  DeclProvenance ->
  [RecordField (Type PolarityBuiltin)] ->
  m ()
restrictPolarityRecordAnnotatedAsTensor (_ident, _p) _fields =
  return ()
