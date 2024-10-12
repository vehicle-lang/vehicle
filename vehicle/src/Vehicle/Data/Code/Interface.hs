module Vehicle.Data.Code.Interface where

import Vehicle.Data.Builtin.Core
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Prelude
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

--------------------------------------------------------------------------------
-- Naturals

class HasBoolLits expr where
  mkBoolLit :: Provenance -> Bool -> expr
  getBoolLit :: expr -> Maybe (Provenance, Bool)

pattern IBoolLiteral :: (HasBoolLits expr) => Provenance -> Bool -> expr
pattern IBoolLiteral p n <- (getBoolLit -> Just (p, n))
  where
    IBoolLiteral p n = mkBoolLit p n

pattern ITrueExpr :: (HasBoolLits expr) => Provenance -> expr
pattern ITrueExpr p = IBoolLiteral p True

pattern IFalseExpr :: (HasBoolLits expr) => Provenance -> expr
pattern IFalseExpr p = IBoolLiteral p False

--------------------------------------------------------------------------------
-- Indices

class HasIndexLits expr where
  mkIndexLit :: Provenance -> Int -> expr
  getIndexLit :: expr -> Maybe (Provenance, Int)

pattern IIndexLiteral :: (HasIndexLits expr) => Provenance -> Int -> expr
pattern IIndexLiteral p n <- (getIndexLit -> Just (p, n))
  where
    IIndexLiteral p n = mkIndexLit p n

--------------------------------------------------------------------------------
-- Naturals

class HasNatLits expr where
  mkNatLit :: Provenance -> Int -> expr
  getNatLit :: expr -> Maybe (Provenance, Int)

pattern INatLiteral :: (HasNatLits expr) => Provenance -> Int -> expr
pattern INatLiteral p n <- (getNatLit -> Just (p, n))
  where
    INatLiteral p n = mkNatLit p n

--------------------------------------------------------------------------------
-- Rationals

class HasRatLits expr where
  mkRatLit :: Provenance -> Rational -> expr
  getRatLit :: expr -> Maybe (Provenance, Rational)

pattern IRatLiteral :: (HasRatLits expr) => Provenance -> Rational -> expr
pattern IRatLiteral p n <- (getRatLit -> Just (p, n))
  where
    IRatLiteral p n = mkRatLit p n

class (HasRatLits expr) => HasRatType expr where
  mkRatType :: Provenance -> expr
  getRatType :: expr -> Maybe Provenance

pattern IRatType :: (HasRatType expr) => Provenance -> expr
pattern IRatType p <- (getRatType -> Just p)
  where
    IRatType p = mkRatType p

--------------------------------------------------------------------------------
-- Lists

class HasStandardListLits expr where
  getNil :: expr -> Maybe (Provenance, GenericArg expr)
  mkNil :: GenericArg expr -> expr
  getCons :: expr -> Maybe (Provenance, GenericArg expr, GenericArg expr, GenericArg expr)
  mkCons :: GenericArg expr -> GenericArg expr -> GenericArg expr -> expr

pattern INil :: (HasStandardListLits expr) => GenericArg expr -> expr
pattern INil t <- (getNil -> Just (_, t))
  where
    INil t = mkNil t

pattern ICons :: (HasStandardListLits expr) => GenericArg expr -> GenericArg expr -> GenericArg expr -> expr
pattern ICons t x xs <- (getCons -> Just (_, t, x, xs))
  where
    ICons t x xs = mkCons t x xs

--------------------------------------------------------------------------------
-- Vectors

-- | Class for expressions that have vectors where all elements have a single
-- type and therefore the type is at the start
class HasStandardVecLits expr where
  mkHomoVector :: GenericArg expr -> [GenericArg expr] -> expr
  getHomoVector :: expr -> Maybe (GenericArg expr, [GenericArg expr])

pattern IVecLiteral :: (HasStandardVecLits expr) => GenericArg expr -> [GenericArg expr] -> expr
pattern IVecLiteral t xs <- (getHomoVector -> Just (t, xs))
  where
    IVecLiteral t xs = mkHomoVector t xs

class (HasStandardVecLits expr) => HasVecType expr where
  mkVectorType :: Provenance -> GenericArg expr -> GenericArg expr -> expr
  getVectorType :: expr -> Maybe (Provenance, GenericArg expr, GenericArg expr)

pattern IVectorType :: (HasVecType expr) => Provenance -> expr -> expr -> expr
pattern IVectorType p tElem tDim <-
  (getVectorType -> Just (p, RelevantExplicitArg _ tElem, IrrelevantExplicitArg _ tDim))
  where
    IVectorType p tElem tDim = mkVectorType p (Arg p Explicit Relevant tElem) (Arg p Explicit Irrelevant tDim)

--------------------------------------------------------------------------------
-- BuiltinHasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class HasStandardData expr where
  mkConstructor :: Provenance -> BuiltinConstructor -> [GenericArg expr] -> expr
  getConstructor :: expr -> Maybe (Provenance, BuiltinConstructor, [GenericArg expr])

  mkFunction :: Provenance -> BuiltinFunction -> [GenericArg expr] -> expr
  getFunction :: expr -> Maybe (Provenance, BuiltinFunction, [GenericArg expr])

  mkFreeVar :: Provenance -> Identifier -> [GenericArg expr] -> expr
  getFreeVar :: expr -> Maybe (Provenance, Identifier, [GenericArg expr])

  getTypeClassOp :: expr -> Maybe (Provenance, TypeClassOp, [GenericArg expr])

--------------------------------------------------------------------------------
-- BuiltinHasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypes expr where
  mkType :: Provenance -> BuiltinType -> [GenericArg expr] -> expr
  getType :: expr -> Maybe (Provenance, BuiltinType, [GenericArg expr])

--------------------------------------------------------------------------------
-- Constructors

pattern INullaryTypeExpr :: (HasStandardTypes expr) => Provenance -> BuiltinType -> expr
pattern INullaryTypeExpr p b <- (getType -> Just (p, b, []))
  where
    INullaryTypeExpr p b = mkType p b []

pattern IUnitType :: (HasStandardTypes expr) => Provenance -> expr
pattern IUnitType p = INullaryTypeExpr p Unit

pattern IBoolType :: (HasStandardTypes expr) => Provenance -> expr
pattern IBoolType p = INullaryTypeExpr p Bool

pattern IIndexType :: (HasStandardTypes expr) => Provenance -> expr -> expr
pattern IIndexType p size <- (getType -> Just (p, Index, [IrrelevantExplicitArg _ size]))

pattern INatType :: (HasStandardTypes expr) => Provenance -> expr
pattern INatType p = INullaryTypeExpr p Nat

pattern IListType :: (HasStandardTypes expr) => Provenance -> expr -> expr
pattern IListType p tElem <- (getType -> Just (p, List, [RelevantExplicitArg _ tElem]))

pattern IRawListType :: (HasStandardTypes expr) => Provenance -> expr
pattern IRawListType p = INullaryTypeExpr p List

--------------------------------------------------------------------------------
-- Constructors

-- Can't use `[]` in a bidrectional pattern synonym until GHC 9.4.3??
pattern INullaryConstructor :: (HasStandardData expr) => Provenance -> BuiltinConstructor -> expr
pattern INullaryConstructor p t <- (getConstructor -> Just (p, t, []))
  where
    INullaryConstructor p t = mkConstructor p t []

pattern IUnitLiteral :: (HasStandardData expr) => Provenance -> expr
pattern IUnitLiteral p = INullaryConstructor p LUnit

mkListExpr :: (HasStandardListLits expr) => expr -> [expr] -> expr
mkListExpr typ = foldr cons nil
  where
    mkImpl = Arg mempty (Implicit True) Relevant
    mkExpl = Arg mempty Explicit Relevant
    tArg = mkImpl typ
    nil = INil tArg
    cons y ys = ICons tArg (mkExpl y) (mkExpl ys)

mkVecExpr :: (HasStandardData expr) => [expr] -> expr
mkVecExpr xs =
  mkConstructor
    mempty
    (LVec (length xs))
    (Arg mempty (Implicit True) Relevant (IUnitLiteral mempty) : (Arg mempty Explicit Relevant <$> xs))

mkTensorLayer ::
  (HasStandardVecLits expr, HasStandardListLits expr, HasStandardTypes expr, HasNatLits expr) =>
  TensorShape ->
  [expr] ->
  expr
mkTensorLayer dims xs = do
  let dimsExpr = mkListExpr (INatType mempty) (fmap (INatLiteral mempty) dims)
  let elementType = Arg mempty (Implicit True) Relevant dimsExpr
  let elements = fmap (Arg mempty Explicit Relevant) xs
  mkHomoVector elementType elements

tensorLikeToExpr ::
  (HasStandardVecLits expr, HasStandardListLits expr, HasStandardTypes expr, HasNatLits expr) =>
  (a -> expr) ->
  TensorShape ->
  [a] ->
  expr
tensorLikeToExpr mkElem = foldMapTensorLike mkElem mkTensorLayer

tensorToExpr ::
  (HasStandardVecLits expr, HasStandardListLits expr, HasStandardTypes expr, HasNatLits expr) =>
  (a -> expr) ->
  Tensor a ->
  expr
tensorToExpr mkElem = foldMapTensor mkElem mkTensorLayer

--------------------------------------------------------------------------------
-- Functions

pattern BuiltinFunc :: (HasStandardData expr) => BuiltinFunction -> [GenericArg expr] -> expr
pattern BuiltinFunc f args <- (getFunction -> Just (_, f, args))
  where
    BuiltinFunc f args = mkFunction mempty f args

pattern IOp1 :: (HasStandardData expr) => BuiltinFunction -> expr -> expr
pattern IOp1 op x <- BuiltinFunc op [RelevantExplicitArg _ x]
  where
    IOp1 op x = BuiltinFunc op [Arg mempty Explicit Relevant x]

pattern IOp2 :: (HasStandardData expr) => BuiltinFunction -> expr -> expr -> expr
pattern IOp2 op x y <- BuiltinFunc op [RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    IOp2 op x y = BuiltinFunc op [Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern IAnd :: (HasStandardData expr) => expr -> expr -> expr
pattern IAnd x y = IOp2 And x y

pattern IOr :: (HasStandardData expr) => expr -> expr -> expr
pattern IOr x y = IOp2 Or x y

pattern INot :: (HasStandardData expr) => expr -> expr
pattern INot x = IOp1 Not x

pattern IIf :: (HasStandardData expr) => expr -> expr -> expr -> expr -> expr
pattern IIf t c x y <- BuiltinFunc If [RelevantImplicitArg _ t, RelevantExplicitArg _ c, RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    IIf t c x y = BuiltinFunc If [Arg mempty (Implicit True) Relevant t, Arg mempty Explicit Relevant c, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern IOrderOp :: (HasStandardData expr) => OrderDomain -> OrderOp -> expr -> expr -> [GenericArg expr] -> expr
pattern IOrderOp dom op x y args <- BuiltinFunc (Order dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : args)

pattern IOrder :: (HasStandardData expr) => OrderDomain -> OrderOp -> expr -> expr -> expr
pattern IOrder dom op x y <- IOrderOp dom op x y _

pattern IOrderRat :: (HasStandardData expr) => OrderOp -> expr -> expr -> expr
pattern IOrderRat op x y = IOp2 (Order OrderRat op) x y

pattern IEqualOp :: (HasStandardData expr) => EqualityDomain -> EqualityOp -> expr -> expr -> [GenericArg expr] -> expr
pattern IEqualOp dom op x y args <- BuiltinFunc (Equals dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : args)

pattern IEqual :: (HasStandardData expr) => EqualityDomain -> expr -> expr -> expr
pattern IEqual dom x y <- IEqualOp dom Eq x y _

pattern IEqualRatOp :: (HasStandardData expr) => EqualityOp -> expr -> expr -> expr
pattern IEqualRatOp op x y = IOp2 (Equals EqRat op) x y

pattern IEqualRat :: (HasStandardData expr) => expr -> expr -> expr
pattern IEqualRat x y = IEqualRatOp Eq x y

pattern INotEqual :: (HasStandardData expr) => EqualityDomain -> expr -> expr -> expr
pattern INotEqual dom x y <- IEqualOp dom Neq x y _

pattern INeg :: (HasStandardData expr) => NegDomain -> expr -> expr
pattern INeg dom x = IOp1 (Neg dom) x

pattern IAdd :: (HasStandardData expr) => AddDomain -> expr -> expr -> expr
pattern IAdd dom x y = IOp2 (Add dom) x y

pattern ISub :: (HasStandardData expr) => SubDomain -> expr -> expr -> expr
pattern ISub dom x y = IOp2 (Sub dom) x y

pattern IMul :: (HasStandardData expr) => MulDomain -> expr -> expr -> expr
pattern IMul dom x y = IOp2 (Mul dom) x y

pattern IDiv :: (HasStandardData expr) => DivDomain -> expr -> expr -> expr
pattern IDiv dom x y = IOp2 (Div dom) x y

pattern IMax :: (HasStandardData expr) => expr -> expr -> expr
pattern IMax x y = IOp2 MaxRat x y

pattern IMin :: (HasStandardData expr) => expr -> expr -> expr
pattern IMin x y = IOp2 MinRat x y

pattern VIndices ::
  (HasStandardData expr) =>
  expr ->
  expr
pattern VIndices n <- BuiltinFunc Indices [argExpr -> n]

pattern IAt ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IAt t n xs i <- BuiltinFunc At [t, n, argExpr -> xs, argExpr -> i]

pattern IFoldVector ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr ->
  expr
pattern IFoldVector n a b f e xs <- BuiltinFunc FoldVector [n, a, b, argExpr -> f, argExpr -> e, argExpr -> xs]
  where
    IFoldVector n a b f e xs = BuiltinFunc FoldVector [n, a, b, Arg mempty Explicit Relevant f, Arg mempty Explicit Relevant e, Arg mempty Explicit Relevant xs]

pattern IMapVector ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IMapVector n a b f xs <- BuiltinFunc MapVector [n, a, b, argExpr -> f, argExpr -> xs]
  where
    IMapVector n a b f xs = BuiltinFunc MapVector [n, a, b, explicit f, explicit xs]

pattern IZipWithVector ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr ->
  expr
pattern IZipWithVector a b c n f xs ys <- BuiltinFunc ZipWithVector [n, a, b, c, argExpr -> f, argExpr -> xs, argExpr -> ys]

pattern IInfiniteQuantifier ::
  (HasStandardData expr) =>
  Quantifier ->
  [GenericArg expr] ->
  expr ->
  expr
pattern IInfiniteQuantifier q args lam <-
  BuiltinFunc (Quantifier q) (reverse -> RelevantExplicitArg _ lam : args)
  where
    IInfiniteQuantifier q args lam =
      BuiltinFunc (Quantifier q) (reverse (Arg mempty Explicit Relevant lam : args))

pattern IForall ::
  (HasStandardData expr) =>
  [GenericArg expr] ->
  expr ->
  expr
pattern IForall args lam = IInfiniteQuantifier Forall args lam

pattern IExists ::
  (HasStandardData expr) =>
  [GenericArg expr] ->
  expr ->
  expr
pattern IExists args lam = IInfiniteQuantifier Exists args lam

--------------------------------------------------------------------------------
-- Iector operation patterns

pattern IFreeVar :: (HasStandardData expr) => Identifier -> [GenericArg expr] -> expr
pattern IFreeVar fn spine <- (getFreeVar -> Just (_, fn, spine))
  where
    IFreeVar fn spine = mkFreeVar mempty fn spine

pattern IStandardLib :: (HasStandardData expr) => StdLibFunction -> [GenericArg expr] -> expr
pattern IStandardLib fn spine <- IFreeVar (findStdLibFunction -> Just fn) spine
  where
    IStandardLib fn spine = IFreeVar (identifierOf fn) spine

pattern IVecEqSpine ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  [GenericArg expr]
pattern IVecEqSpine t1 t2 dim sol x y <- [t1, t2, dim, sol, argExpr -> x, argExpr -> y]
  where
    IVecEqSpine t1 t2 dim sol x y = [t1, t2, dim, sol, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern IVecOp2Spine ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  [GenericArg expr]
pattern IVecOp2Spine t1 t2 t3 dim sol x y <- [t1, t2, t3, dim, sol, argExpr -> x, argExpr -> y]

pattern IVecEqArgs ::
  (HasStandardData expr) =>
  expr ->
  expr ->
  [GenericArg expr]
pattern IVecEqArgs x y <- IVecEqSpine _ _ _ _ x y

pattern IVectorEqualFull :: (HasStandardData expr) => [GenericArg expr] -> expr
pattern IVectorEqualFull spine = IStandardLib StdEqualsVector spine

pattern IVectorNotEqualFull :: (HasStandardData expr) => [GenericArg expr] -> expr
pattern IVectorNotEqualFull spine = IStandardLib StdNotEqualsVector spine

pattern IVectorEqual :: (HasStandardData expr) => expr -> expr -> expr
pattern IVectorEqual x y <- IVectorEqualFull (IVecEqArgs x y)

pattern IVectorNotEqual :: (HasStandardData expr) => expr -> expr -> expr
pattern IVectorNotEqual x y <- IVectorNotEqualFull (IVecEqArgs x y)

pattern IVectorAdd ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IVectorAdd a b c n f x y <- IStandardLib StdAddVector [a, b, c, n, f, argExpr -> x, argExpr -> y]

pattern IVectorSub ::
  (HasStandardData expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IVectorSub a b c n f x y <- IStandardLib StdSubVector [a, b, c, n, f, argExpr -> x, argExpr -> y]

pattern IForeachIndex ::
  (HasStandardData expr) =>
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IForeachIndex t n fn <- IStandardLib StdForeachIndex [t, argExpr -> n, argExpr -> fn]
  where
    IForeachIndex t n fn = IStandardLib StdForeachIndex [t, Arg mempty Explicit Relevant n, Arg mempty Explicit Relevant fn]

--------------------------------------------------------------------------------
-- Rational tensors

class HasRatTensors expr where
  mkRatTensorOp :: RatTensorBuiltin -> [GenericArg expr] -> expr
  getRatTensorOp :: expr -> Maybe (RatTensorBuiltin, [GenericArg expr])

pattern IRatTensorOp :: (HasRatTensors expr) => RatTensorBuiltin -> [GenericArg expr] -> expr
pattern IRatTensorOp b args <- (getRatTensorOp -> Just (b, args))
  where
    IRatTensorOp b args = mkRatTensorOp b args

-- Can't be bidirectional as Haskell 8.10.7 doesn't support the empty list literal being bidirectional.
pattern INullaryRatTensorOp :: (HasRatTensors expr) => RatTensorBuiltin -> expr
pattern INullaryRatTensorOp b <- IRatTensorOp b []
  where
    INullaryRatTensorOp b = IRatTensorOp b []

pattern IRatElementType :: (HasRatTensors expr) => expr
pattern IRatElementType = INullaryRatTensorOp RatType

pattern IRatTensor :: (HasRatTensors expr) => Tensor Rational -> expr
pattern IRatTensor t = INullaryRatTensorOp (RatTensor t)

mkRatTensor :: (HasRatTensors expr) => Provenance -> Tensor Rational -> expr
mkRatTensor _p = IRatTensor

getRatTensor :: (HasRatTensors expr) => expr -> Maybe (Provenance, Tensor Rational)
getRatTensor (IRatTensor t) = Just (mempty, t)
getRatTensor _ = Nothing

getRatConstTensor :: (HasDimensionData expr, HasRatTensors expr) => expr -> Maybe Rational
getRatConstTensor (getDimensionDataOp -> Just (ConstTensor, [_, argExpr -> (getRatTensorOp -> (Just (RatLiteral b, _))), _])) = Just b
getRatConstTensor _ = Nothing

pattern IRatConstTensor :: (HasDimensionData expr, HasRatTensors expr) => Rational -> GenericArg expr -> expr
pattern IRatConstTensor b dims <- IConstTensor _ (argExpr -> INullaryRatTensorOp (RatLiteral b)) dims
  where
    IRatConstTensor b dims = IConstTensor (implicit IRatElementType) (explicit (INullaryRatTensorOp (RatLiteral b))) dims

--------------------------------------------------------------------------------
-- Bool tensors

class (HasRatTensors expr) => HasBoolTensors expr where
  mkBoolTensorOp :: BoolTensorBuiltin -> [GenericArg expr] -> expr
  getBoolTensorOp :: expr -> Maybe (BoolTensorBuiltin, [GenericArg expr])

pattern IBoolTensorOp :: (HasBoolTensors expr) => BoolTensorBuiltin -> [GenericArg expr] -> expr
pattern IBoolTensorOp b args <- (getBoolTensorOp -> Just (b, args))
  where
    IBoolTensorOp b args = mkBoolTensorOp b args

-- Can't be bidirectional as Haskell 8.10.7 doesn't support the empty list literal being bidirectional.
pattern INullaryBoolTensorOp :: (HasBoolTensors expr) => BoolTensorBuiltin -> expr
pattern INullaryBoolTensorOp b <- IBoolTensorOp b []
  where
    INullaryBoolTensorOp b = IBoolTensorOp b []

pattern IBoolTensor :: (HasBoolTensors expr) => Tensor Bool -> expr
pattern IBoolTensor t = INullaryBoolTensorOp (BoolTensor t)

pattern IBoolElementType :: (HasBoolTensors expr) => expr
pattern IBoolElementType = INullaryBoolTensorOp BoolType

pattern IBoolConstTensor :: (HasDimensionData expr, HasBoolTensors expr) => Bool -> GenericArg expr -> expr
pattern IBoolConstTensor b dims <- IConstTensor _ (argExpr -> INullaryBoolTensorOp (BoolLiteral b)) dims
  where
    IBoolConstTensor b dims = IConstTensor (implicit IBoolElementType) (explicit (INullaryBoolTensorOp (BoolLiteral b))) dims

mkBoolTensor :: (HasBoolTensors expr) => Provenance -> Tensor Bool -> expr
mkBoolTensor _p = IBoolTensor

getBoolTensor :: (HasBoolTensors expr) => expr -> Maybe (Provenance, Tensor Bool)
getBoolTensor (IBoolTensor t) = Just (mempty, t)
getBoolTensor _ = Nothing

getBoolConstTensor :: (HasDimensionData expr, HasBoolTensors expr) => expr -> Maybe Bool
getBoolConstTensor (IBoolConstTensor b _) = Just b
getBoolConstTensor _ = Nothing

--------------------------------------------------------------------------------
-- Tensor slices

class HasDimensionTypes expr where
  mkDimensionTypeOp :: DimensionTypeBuiltin -> [GenericArg expr] -> expr
  getDimensionTypeOp :: expr -> Maybe (DimensionTypeBuiltin, [GenericArg expr])

pattern IDimensionTypeOp :: (HasDimensionTypes expr) => DimensionTypeBuiltin -> [GenericArg expr] -> expr
pattern IDimensionTypeOp b args <- (getDimensionTypeOp -> Just (b, args))
  where
    IDimensionTypeOp b args = mkDimensionTypeOp b args

pattern ITensorType :: (HasDimensionTypes expr) => GenericArg expr -> GenericArg expr -> expr
pattern ITensorType tElem dims <- IDimensionTypeOp TensorType [tElem, dims]
  where
    ITensorType tElem dims = IDimensionTypeOp TensorType [tElem, dims]

pattern IDimType :: (HasDimensionTypes expr) => GenericArg expr -> expr
pattern IDimType size <- IDimensionTypeOp DimensionType [size]
  where
    IDimType size = IDimensionTypeOp DimensionType [size]

class HasDimensionData expr where
  mkDimensionDataOp :: DimensionDataBuiltin -> [GenericArg expr] -> expr
  getDimensionDataOp :: expr -> Maybe (DimensionDataBuiltin, [GenericArg expr])

pattern IDimensionDataOp :: (HasDimensionData expr) => DimensionDataBuiltin -> [GenericArg expr] -> expr
pattern IDimensionDataOp b args <- (getDimensionDataOp -> Just (b, args))
  where
    IDimensionDataOp b args = mkDimensionDataOp b args

-- Can't be bidirectional as Haskell 8.10.7 doesn't support the empty list literal being bidirectional.
pattern INullaryDimensionDataOp :: (HasDimensionData expr) => DimensionDataBuiltin -> expr
pattern INullaryDimensionDataOp b <- IDimensionDataOp b []
  where
    INullaryDimensionDataOp b = IDimensionDataOp b []

pattern IDim :: (HasDimensionData expr) => Int -> expr
pattern IDim t = INullaryDimensionDataOp (Dimension t)

pattern IDimNil :: (HasDimensionData expr) => expr
pattern IDimNil = INullaryDimensionDataOp DimensionNil

pattern IDimCons :: (HasDimensionData expr) => GenericArg expr -> GenericArg expr -> expr
pattern IDimCons x xs <- IDimensionDataOp DimensionCons [x, xs]
  where
    IDimCons x xs = IDimensionDataOp DimensionCons [x, xs]

pattern IDimIndex :: (HasDimensionData expr) => GenericArg expr -> Int -> expr
pattern IDimIndex dim t <- IDimensionDataOp (DimensionIndex t) [dim]
  where
    IDimIndex dim t = IDimensionDataOp (DimensionIndex t) [dim]

pattern IDimIndexTensor :: (HasDimensionData expr) => GenericArg expr -> Tensor Int -> expr
pattern IDimIndexTensor dim t <- IDimensionDataOp (DimensionIndexTensor t) [dim]
  where
    IDimIndexTensor dim t = IDimensionDataOp (DimensionIndexTensor t) [dim]

pattern IConstTensor :: (HasDimensionData expr) => GenericArg expr -> GenericArg expr -> GenericArg expr -> expr
pattern IConstTensor typ value dims <- IDimensionDataOp ConstTensor [typ, value, dims]
  where
    IConstTensor typ value dims = IDimensionDataOp ConstTensor [typ, value, dims]

pattern IDimIndexConstTensor :: (HasDimensionTypes expr, HasDimensionData expr) => GenericArg expr -> Int -> GenericArg expr -> expr
pattern IDimIndexConstTensor dim b dims <- IConstTensor _ (argExpr -> IDimIndex dim b) dims
  where
    IDimIndexConstTensor dim b dims = IConstTensor (implicit (IDimType dim)) (explicit (IDimIndex dim b)) dims

dimSingleton :: (HasDimensionData expr) => Int -> expr
dimSingleton n = IDimCons (explicit $ IDim n) (explicit IDimNil)

getDimIndexTensor :: (HasDimensionData expr) => expr -> Maybe (Provenance, Tensor Int)
getDimIndexTensor (IDimIndexTensor _ t) = Just (mempty, t)
getDimIndexTensor _ = Nothing

getDimension :: (HasDimensionData expr) => expr -> Maybe (Provenance, Int)
getDimension (IDim t) = Just (mempty, t)
getDimension _ = Nothing

getDimIndexConstTensor :: (HasDimensionData expr) => expr -> Maybe Int
getDimIndexConstTensor (getDimensionDataOp -> Just (ConstTensor, [_, argExpr -> (getDimensionDataOp -> (Just (DimensionIndex b, _))), _])) = Just b
getDimIndexConstTensor _ = Nothing

getDimensions :: (HasDimensionData expr) => expr -> Maybe TensorShape
getDimensions = \case
  IDimNil -> Just []
  IDimCons (argExpr -> IDim n) (argExpr -> xs) -> (n :) <$> getDimensions xs
  _ -> Nothing
