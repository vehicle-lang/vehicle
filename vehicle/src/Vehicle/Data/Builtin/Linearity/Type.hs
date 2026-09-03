{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Linearity.Type
  ( typeLinearityBuiltin,
    isLinearityBuiltinConstructor,
  )
where

import Data.Proxy (Proxy (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Bidirectional (createFreshUnificationConstraint)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface.InputOutputInsertion
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin (..))
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Builtin.Linearity.Solver
import Vehicle.Data.Builtin.Standard (Builtin (..))
import Vehicle.Data.Code.DSL (iterate)
import Vehicle.Data.DSL
import Vehicle.Data.Variable.Free.Context (MonadFreeContext (..))
import Prelude hiding (iterate)

--------------------------------------------------------------------------------
-- Typing
--------------------------------------------------------------------------------

instance TypableBuiltin LinearityBuiltin where
  typeBuiltin p b = return (fromDSL p $ typeLinearityBuiltin p b)
  useDependentMetas _ = False
  isConstructor = isLinearityBuiltinConstructor
  isCastConstraint _ = False

isLinearityBuiltinConstructor :: LinearityBuiltin -> Bool
isLinearityBuiltinConstructor = \case
  LinearityConstructor {} -> True
  LinearityFunction {} -> False
  Linearity {} -> True
  LinearityRelation {} -> True

-- | Return the type of the provided builtin.
typeLinearityBuiltin :: Provenance -> LinearityBuiltin -> LinearityDSLExpr
typeLinearityBuiltin p = \case
  LinearityConstructor c -> typeOfConstructor c
  LinearityFunction f -> typeOfBuiltinFunction p f
  Linearity {} -> tLin
  LinearityRelation r -> typeOfLinearityRelation r

typeOfBuiltinFunction :: Provenance -> BuiltinFunction -> LinearityDSLExpr
typeOfBuiltinFunction p = \case
  -- Boolean operations
  Not {} -> typeOfOp1
  Implies -> typeOfOp2 maxLinearity
  And {} -> typeOfOp2 maxLinearity
  Or {} -> typeOfOp2 maxLinearity
  QuantifyRatTensor q -> typeOfQuantifier q
  QuantifyRecord q -> typeOfQuantifier q
  If -> typeOfIf
  ReduceAndTensor -> typeOfOp1
  ReduceOrTensor -> typeOfOp1
  -- Arithmetic operations
  Add {} -> typeOfOp2 maxLinearity
  Mul {} -> typeOfOp2 (mulLinearity p)
  Neg {} -> typeOfOp1
  Sub {} -> typeOfOp2 maxLinearity
  Div {} -> typeOfOp2 (divLinearity p)
  Min {} -> typeOfOp2 maxLinearity
  Max {} -> typeOfOp2 maxLinearity
  Pow {} -> typeOfOp2 (powLinearity p)
  Exp {} -> typeOfOp1
  Log {} -> typeOfOp1
  ReduceAddRatTensor -> typeOfOp1
  ReduceMulRatTensor ->
    forAllLinearityPairs $ \l1 l2 ->
      mulLinearity p l1 l1 l2 .~~~> l1 ~> l2
  ReduceMinRatTensor -> typeOfOp1
  ReduceMaxRatTensor -> typeOfOp1
  -- Comparisons
  CompareNat {} -> typeOfOp2 maxLinearity
  CompareIndex {} -> typeOfOp2 maxLinearity
  CompareRatTensor {} -> typeOfOp2 maxLinearity
  -- Container functions
  FoldList -> typeOfFold
  MapList -> typeOfMap
  ReverseList -> typeOfOp1
  AppendList -> typeOfOp2 maxLinearity
  AtVector -> typeOfAt
  AtTensor -> typeOfAt
  StackTensor -> typeOfStack
  ConstTensor -> forAllLinearities $ \l -> l ~> constant ~> l
  ForeachTensor -> typeOfForeach
  ForeachVector -> typeOfForeach
  Iterate -> typeOfIterate
  Transpose -> typeOfOp1
  SearchRatTensor {} -> developerError "SearchRatTensor should not appear in linearity typing"
  WhereTensor {} -> developerError "WhereTensor should not appear in linearity typing"

typeOfConstructor :: BuiltinConstructor -> LinearityDSLExpr
typeOfConstructor = \case
  Nil -> constant
  Cons -> typeOfOp2 maxLinearity
  UnitLiteral {} -> constant
  IndexLiteral {} -> constant
  NatLiteral {} -> constant
  VectorLiteral {} -> typeOfVectorLiteral
  NatTensorLiteral {} -> constant
  BoolTensorLiteral {} -> constant
  RatTensorLiteral {} -> constant

typeOfLinearityRelation :: LinearityRelation -> LinearityDSLExpr
typeOfLinearityRelation = \case
  MaxLinearity -> tLin ~> tLin ~> tLin ~> type0
  MulLinearity {} -> tLin ~> tLin ~> tLin ~> type0
  DivLinearity {} -> tLin ~> tLin ~> tLin ~> type0
  PowLinearity {} -> tLin ~> tLin ~> tLin ~> type0
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

typeOfAt :: LinearityDSLExpr
typeOfAt = typeOfOp2 maxLinearity

typeOfForeach :: LinearityDSLExpr
typeOfForeach = forAllLinearities $ \l -> (constant ~> l) ~> l

typeOfFold :: LinearityDSLExpr
typeOfFold =
  forAllLinearityTriples $ \l1 l2 l3 ->
    maxLinearity l1 l2 l3 .~~~> (l1 ~> l2 ~> l3) ~> l1 ~> l2 ~> l3

typeOfMap :: LinearityDSLExpr
typeOfMap =
  forAllLinearities $ \l1 ->
    forAllLinearities $ \l2 ->
      (l1 ~> l2) ~> l1 ~> l2

typeOfQuantifier :: Quantifier -> LinearityDSLExpr
typeOfQuantifier q =
  forAll "f" type0 $ \tLam ->
    forAll "A" type0 $ \tRes ->
      quantLinearity q tLam tRes .~~~> tLam ~> tRes

typeOfIterate :: LinearityDSLExpr
typeOfIterate = ((type0 ~> type0) ~> type0 ~> type0) ~> constant ~> type0

typeOfVectorLiteral :: LinearityDSLExpr
typeOfVectorLiteral =
  forAll "n" constant $ \n ->
    iterate
      type0
      ( \fn maxSoFar ->
          forAll "l" tLin $ \li ->
            forAll "l_max" tLin $ \newMax ->
              maxLinearity maxSoFar li newMax .~~~> li ~> fn @@ [newMax]
      )
      n
      constant

typeOfStack :: LinearityDSLExpr
typeOfStack = typeOfVectorLiteral

--------------------------------------------------------------------------------
-- Type system
--------------------------------------------------------------------------------

instance HasTypeSystem LinearityBuiltin where
  convertFromStandardBuiltins = traverse $ traverseBuiltinsM convertToLinearityTypes
  restrictDeclType = restrictLinearityDeclType
  restrictRecordAnnotatedAsTensor = restrictLinearityRecordAnnotatedAsTensor
  isAuxiliaryConstraint _ = True
  solveAuxiliaryInstanceConstraint = solveLinearityConstraint
  addAuxiliaryInputOutputConstraints = addFunctionAuxiliaryInputOutputConstraints (LinearityRelation . FunctionLinearity)
  generateDefaultAuxiliaryConstraint _ = return False

pattern LinearityExpr :: Provenance -> Linearity -> Expr LinearityBuiltin
pattern LinearityExpr p lin = Builtin p (Linearity lin)

freshLinearityMeta :: (MonadTypeChecker LinearityBuiltin m) => Provenance -> m (Expr LinearityBuiltin)
freshLinearityMeta p = freshMetaExpr p (TypeUniverse p 0) Relevant mempty

convertToLinearityTypes ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  BuiltinUpdate m Builtin LinearityBuiltin
convertToLinearityTypes p b args = case b of
  BuiltinFunction f -> do
    let args' = case f of
          StackTensor -> implicit (Builtin p (LinearityConstructor (NatLiteral (length args)))) : args
          _ -> args
    return $ normAppList (Builtin p (LinearityFunction f)) args'
  BuiltinConstructor c ->
    return $ normAppList (Builtin p (LinearityConstructor c)) args
  BuiltinType s -> case s of
    UnitType -> return $ Builtin p $ Linearity Constant
    BoolType {} -> freshLinearityMeta p
    RatType {} -> freshLinearityMeta p
    IndexType -> freshLinearityMeta p
    NatType -> freshLinearityMeta p
    ListType -> return $ extractElementType b args
    VectorType -> return $ extractElementType b args
    TensorType -> return $ extractElementType b args
  DerivedFunction f -> return $ normAppList (FreeVar p (identifierOf f)) args
  TypeClass {} -> monomorphisationError b args
  TypeClassOp {} -> monomorphisationError b args
  NatInDomainConstraint -> monomorphisationError b args
  BuiltinCast {} -> monomorphisationError b args

restrictLinearityDeclType ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  RestrictedDecl ->
  DeclProvenance ->
  Type LinearityBuiltin ->
  m (Type LinearityBuiltin)
restrictLinearityDeclType rDecl declProv declType = do
  freeEnv <- getFreeCtx (Proxy @LinearityBuiltin)
  let origin = InstanceTypeRestrictionOrigin $ TypeRestrictionOrigin freeEnv declProv (Left rDecl) declType
  case rDecl of
    RestrictedNetwork -> restrictLinearityNetworkType origin declProv declType
    RestrictedDataset -> assertConstantLinearity origin declProv declType
    RestrictedParameter {} -> assertConstantLinearity origin declProv declType
    RestrictedProperty {} -> return declType

restrictLinearityNetworkType ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  InstanceConstraintOrigin LinearityBuiltin ->
  DeclProvenance ->
  Type LinearityBuiltin ->
  m (Type LinearityBuiltin)
restrictLinearityNetworkType origin (ident, p) networkType = do
  inputLin <- freshLinearityMeta p
  outputLin <- freshLinearityMeta p

  let inputLinBinder = Binder (BinderDisplayForm OnlyType False) Explicit Relevant inputLin
  let functionNetworkType = Pi p inputLinBinder outputLin
  createFreshUnificationConstraint p mempty (CheckingInstanceType origin) networkType functionNetworkType

  -- The linearity of the output of a network is the max of 1) Linear (as outputs
  -- are also variables) and 2) the linearity of its input. So prepend this
  -- constraint to the front of the type.
  logDebug MaxDetail "Appending `MaxLinearity` constraint to network type"
  let outputLinProvenance = Linear $ NetworkOutputProvenance p (nameOf ident)
  let linConstraintArgs = [LinearityExpr p outputLinProvenance, inputLin, outputLin]
  let linConstraint = App (Builtin p (LinearityRelation MaxLinearity)) (Arg Explicit Relevant <$> linConstraintArgs)
  let linConstraintBinder = Binder (BinderDisplayForm OnlyType False) (Instance True) Irrelevant linConstraint

  return $ Pi p linConstraintBinder functionNetworkType

assertConstantLinearity ::
  (MonadTypeChecker LinearityBuiltin m) =>
  InstanceConstraintOrigin LinearityBuiltin ->
  DeclProvenance ->
  Type LinearityBuiltin ->
  m (Type LinearityBuiltin)
assertConstantLinearity origin (_, p) t = do
  createFreshUnificationConstraint p mempty (CheckingInstanceType origin) (LinearityExpr p Constant) t
  return t

restrictLinearityRecordAnnotatedAsTensor ::
  forall m.
  (MonadTypeChecker LinearityBuiltin m) =>
  DeclProvenance ->
  [GenericRecordField (Type LinearityBuiltin)] ->
  m ()
restrictLinearityRecordAnnotatedAsTensor (_ident, _p) _fields =
  return ()
