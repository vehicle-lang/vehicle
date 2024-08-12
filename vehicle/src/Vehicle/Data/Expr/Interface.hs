module Vehicle.Data.Expr.Interface where

import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Expr.Normalised
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
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

--------------------------------------------------------------------------------
-- Lists

class HasStandardListLits expr where
  getNil :: expr -> Maybe (Provenance, GenericArg expr)
  mkNil :: GenericArg expr -> expr
  getCons :: expr -> Maybe (GenericArg expr, GenericArg expr, GenericArg expr)
  mkCons :: GenericArg expr -> GenericArg expr -> GenericArg expr -> expr

pattern INil :: (HasStandardListLits expr) => GenericArg expr -> expr
pattern INil t <- (getNil -> Just (_, t))
  where
    INil t = mkNil t

pattern ICons :: (HasStandardListLits expr) => GenericArg expr -> GenericArg expr -> GenericArg expr -> expr
pattern ICons t x xs <- (getCons -> Just (t, x, xs))
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

--------------------------------------------------------------------------------
-- HasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class HasStandardDataExpr expr where
  mkConstructor :: Provenance -> BuiltinConstructor -> [GenericArg expr] -> expr
  getConstructor :: expr -> Maybe (Provenance, BuiltinConstructor, [GenericArg expr])

  mkFunction :: Provenance -> BuiltinFunction -> [GenericArg expr] -> expr
  getFunction :: expr -> Maybe (Provenance, BuiltinFunction, [GenericArg expr])

  mkFreeVar :: Provenance -> Identifier -> [GenericArg expr] -> expr
  getFreeVar :: expr -> Maybe (Provenance, Identifier, [GenericArg expr])

  getTypeClassOp :: expr -> Maybe (Provenance, TypeClassOp, [GenericArg expr])

instance (HasStandardData builtin) => HasStandardDataExpr (Expr var builtin) where
  mkFunction p b = normAppList (Builtin p (mkBuiltinFunction b))
  getFunction e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinFunction b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkConstructor p b = normAppList (Builtin p (mkBuiltinConstructor b))
  getConstructor e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinConstructor b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

  mkFreeVar p ident = normAppList (FreeVar p ident)
  getFreeVar e = case getFreeVarApp e of
    Just (p, ident, args) -> Just (p, ident, args)
    _ -> Nothing

  getTypeClassOp e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinTypeClassOp b of
      Just f -> Just (p, f, args)
      Nothing -> Nothing
    _ -> Nothing

instance (HasStandardData builtin) => HasStandardDataExpr (Value closure builtin) where
  mkFunction _p b = VBuiltin (mkBuiltinFunction b)
  getFunction e = case e of
    VBuiltin b args -> case getBuiltinFunction b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

  mkConstructor _p b = VBuiltin (mkBuiltinConstructor b)
  getConstructor e = case e of
    VBuiltin b args -> case getBuiltinConstructor b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

  mkFreeVar _p = VFreeVar
  getFreeVar = \case
    VFreeVar ident args -> Just (mempty, ident, args)
    _ -> Nothing

  getTypeClassOp e = case e of
    VBuiltin b args -> case getBuiltinTypeClassOp b of
      Just op -> Just (mempty, op, args)
      Nothing -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- HasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypesExpr expr where
  mkType :: Provenance -> BuiltinType -> [GenericArg expr] -> expr
  getType :: expr -> Maybe (Provenance, BuiltinType, [GenericArg expr])

instance (HasStandardTypes builtin) => HasStandardTypesExpr (Expr var builtin) where
  mkType p b = normAppList (Builtin p (mkBuiltinType b))
  getType e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinType b of
      Just t -> Just (p, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (HasStandardTypes builtin) => HasStandardTypesExpr (Value closure builtin) where
  mkType _p b = VBuiltin (mkBuiltinType b)
  getType e = case e of
    VBuiltin b args -> case getBuiltinType b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- Constructors

pattern INullaryTypeExpr :: (HasStandardTypesExpr expr) => Provenance -> BuiltinType -> expr
pattern INullaryTypeExpr p b <- (getType -> Just (p, b, []))
  where
    INullaryTypeExpr p b = mkType p b []

pattern IUnitType :: (HasStandardTypesExpr expr) => Provenance -> expr
pattern IUnitType p = INullaryTypeExpr p Unit

pattern IBoolType :: (HasStandardTypesExpr expr) => Provenance -> expr
pattern IBoolType p = INullaryTypeExpr p Bool

pattern IIndexType :: (HasStandardTypesExpr expr) => Provenance -> expr -> expr
pattern IIndexType p size <- (getType -> Just (p, Index, [IrrelevantExplicitArg _ size]))

pattern INatType :: (HasStandardTypesExpr expr) => Provenance -> expr
pattern INatType p = INullaryTypeExpr p Nat

pattern IRatType :: (HasStandardTypesExpr expr) => Provenance -> expr
pattern IRatType p = INullaryTypeExpr p Rat

pattern IListType :: (HasStandardTypesExpr expr) => Provenance -> expr -> expr
pattern IListType p tElem <- (getType -> Just (p, List, [RelevantExplicitArg _ tElem]))

pattern IVectorType ::
  (HasStandardTypesExpr expr) =>
  Provenance ->
  expr ->
  expr ->
  expr
pattern IVectorType p tElem tDim <-
  (getType -> Just (p, Vector, [RelevantExplicitArg _ tElem, IrrelevantExplicitArg _ tDim]))
  where
    IVectorType p tElem tDim = mkType p Vector [Arg p Explicit Relevant tElem, Arg p Explicit Irrelevant tDim]

pattern IRawListType :: (HasStandardTypesExpr expr) => Provenance -> expr
pattern IRawListType p = INullaryTypeExpr p List

--------------------------------------------------------------------------------
-- Constructors

-- Can't use `[]` in a bidrectional pattern synonym until GHC 9.4.3??
pattern INullaryConstructor :: (HasStandardDataExpr expr) => Provenance -> BuiltinConstructor -> expr
pattern INullaryConstructor p t <- (getConstructor -> Just (p, t, []))
  where
    INullaryConstructor p t = mkConstructor p t []

pattern IUnitLiteral :: (HasStandardDataExpr expr) => Provenance -> expr
pattern IUnitLiteral p = INullaryConstructor p LUnit

mkListExpr :: (HasStandardListLits expr) => expr -> [expr] -> expr
mkListExpr typ = foldr cons nil
  where
    mkImpl = Arg mempty (Implicit True) Relevant
    mkExpl = Arg mempty Explicit Relevant
    tArg = mkImpl typ
    nil = INil tArg
    cons y ys = ICons tArg (mkExpl y) (mkExpl ys)

mkVecExpr :: (HasStandardDataExpr expr) => [expr] -> expr
mkVecExpr xs =
  mkConstructor
    mempty
    (LVec (length xs))
    (Arg mempty (Implicit True) Relevant (IUnitLiteral mempty) : (Arg mempty Explicit Relevant <$> xs))

tensorToExpr :: (HasStandardDataExpr expr) => (a -> expr) -> Tensor a -> expr
tensorToExpr mkElem =
  foldMapTensor
    mkElem
    ( \xs ->
        mkConstructor mempty (LVec (length xs)) (Arg mempty (Implicit True) Relevant (IUnitLiteral mempty) : fmap (Arg mempty Explicit Relevant) xs)
    )

--------------------------------------------------------------------------------
-- Functions

pattern BuiltinFunc :: (HasStandardDataExpr expr) => BuiltinFunction -> [GenericArg expr] -> expr
pattern BuiltinFunc f args <- (getFunction -> Just (_, f, args))
  where
    BuiltinFunc f args = mkFunction mempty f args

pattern IOp1 :: (HasStandardDataExpr expr) => BuiltinFunction -> expr -> expr
pattern IOp1 op x <- BuiltinFunc op [RelevantExplicitArg _ x]
  where
    IOp1 op x = BuiltinFunc op [Arg mempty Explicit Relevant x]

pattern IOp2 :: (HasStandardDataExpr expr) => BuiltinFunction -> expr -> expr -> expr
pattern IOp2 op x y <- BuiltinFunc op [RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    IOp2 op x y = BuiltinFunc op [Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern IAnd :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IAnd x y = IOp2 And x y

pattern IOr :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IOr x y = IOp2 Or x y

pattern INot :: (HasStandardDataExpr expr) => expr -> expr
pattern INot x = IOp1 Not x

pattern IIf :: (HasStandardDataExpr expr) => expr -> expr -> expr -> expr -> expr
pattern IIf t c x y <- BuiltinFunc If [RelevantImplicitArg _ t, RelevantExplicitArg _ c, RelevantExplicitArg _ x, RelevantExplicitArg _ y]
  where
    IIf t c x y = BuiltinFunc If [Arg mempty (Implicit True) Relevant t, Arg mempty Explicit Relevant c, Arg mempty Explicit Relevant x, Arg mempty Explicit Relevant y]

pattern IOrderOp :: (HasStandardDataExpr expr) => OrderDomain -> OrderOp -> expr -> expr -> [GenericArg expr] -> expr
pattern IOrderOp dom op x y args <- BuiltinFunc (Order dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : args)

pattern IOrder :: (HasStandardDataExpr expr) => OrderDomain -> OrderOp -> expr -> expr -> expr
pattern IOrder dom op x y <- IOrderOp dom op x y _

pattern IOrderRat :: (HasStandardDataExpr expr) => OrderOp -> expr -> expr -> expr
pattern IOrderRat op x y = IOp2 (Order OrderRat op) x y

pattern IEqualOp :: (HasStandardDataExpr expr) => EqualityDomain -> EqualityOp -> expr -> expr -> [GenericArg expr] -> expr
pattern IEqualOp dom op x y args <- BuiltinFunc (Equals dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : args)

pattern IEqual :: (HasStandardDataExpr expr) => EqualityDomain -> expr -> expr -> expr
pattern IEqual dom x y <- IEqualOp dom Eq x y _

pattern IEqualRatOp :: (HasStandardDataExpr expr) => EqualityOp -> expr -> expr -> expr
pattern IEqualRatOp op x y = IOp2 (Equals EqRat op) x y

pattern IEqualRat :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IEqualRat x y = IEqualRatOp Eq x y

pattern INotEqual :: (HasStandardDataExpr expr) => EqualityDomain -> expr -> expr -> expr
pattern INotEqual dom x y <- IEqualOp dom Neq x y _

pattern INeg :: (HasStandardDataExpr expr) => NegDomain -> expr -> expr
pattern INeg dom x = IOp1 (Neg dom) x

pattern IAdd :: (HasStandardDataExpr expr) => AddDomain -> expr -> expr -> expr
pattern IAdd dom x y = IOp2 (Add dom) x y

pattern ISub :: (HasStandardDataExpr expr) => SubDomain -> expr -> expr -> expr
pattern ISub dom x y = IOp2 (Sub dom) x y

pattern IMul :: (HasStandardDataExpr expr) => MulDomain -> expr -> expr -> expr
pattern IMul dom x y = IOp2 (Mul dom) x y

pattern IDiv :: (HasStandardDataExpr expr) => DivDomain -> expr -> expr -> expr
pattern IDiv dom x y = IOp2 (Div dom) x y

pattern IMax :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IMax x y = IOp2 MaxRat x y

pattern IMin :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IMin x y = IOp2 MinRat x y

pattern VIndices ::
  (HasStandardDataExpr expr) =>
  expr ->
  expr
pattern VIndices n <- BuiltinFunc Indices [argExpr -> n]

pattern IAt ::
  (HasStandardDataExpr expr) =>
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IAt t n xs i <- BuiltinFunc At [t, n, argExpr -> xs, argExpr -> i]

pattern IFoldVector ::
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IMapVector n a b f xs <- BuiltinFunc MapVector [n, a, b, argExpr -> f, argExpr -> xs]

pattern IZipWithVector ::
  (HasStandardDataExpr expr) =>
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  GenericArg expr ->
  expr ->
  expr ->
  expr ->
  expr
pattern IZipWithVector a b c n f xs ys <- BuiltinFunc ZipWithVector [a, b, c, n, argExpr -> f, argExpr -> xs, argExpr -> ys]

pattern IInfiniteQuantifier ::
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
  [GenericArg expr] ->
  expr ->
  expr
pattern IForall args lam = IInfiniteQuantifier Forall args lam

pattern IExists ::
  (HasStandardDataExpr expr) =>
  [GenericArg expr] ->
  expr ->
  expr
pattern IExists args lam = IInfiniteQuantifier Exists args lam

--------------------------------------------------------------------------------
-- Iector operation patterns

pattern IFreeVar :: (HasStandardDataExpr expr) => Identifier -> [GenericArg expr] -> expr
pattern IFreeVar fn spine <- (getFreeVar -> Just (_, fn, spine))
  where
    IFreeVar fn spine = mkFreeVar mempty fn spine

pattern IStandardLib :: (HasStandardDataExpr expr) => StdLibFunction -> [GenericArg expr] -> expr
pattern IStandardLib fn spine <- IFreeVar (findStdLibFunction -> Just fn) spine
  where
    IStandardLib fn spine = IFreeVar (identifierOf fn) spine

pattern IVecEqSpine ::
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
  expr ->
  expr ->
  [GenericArg expr]
pattern IVecEqArgs x y <- IVecEqSpine _ _ _ _ x y

pattern IVectorEqualFull :: (HasStandardDataExpr expr) => [GenericArg expr] -> expr
pattern IVectorEqualFull spine = IStandardLib StdEqualsVector spine

pattern IVectorNotEqualFull :: (HasStandardDataExpr expr) => [GenericArg expr] -> expr
pattern IVectorNotEqualFull spine = IStandardLib StdNotEqualsVector spine

pattern IVectorEqual :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IVectorEqual x y <- IVectorEqualFull (IVecEqArgs x y)

pattern IVectorNotEqual :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IVectorNotEqual x y <- IVectorNotEqualFull (IVecEqArgs x y)

pattern IVectorAdd ::
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
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
  (HasStandardDataExpr expr) =>
  GenericArg expr ->
  expr ->
  expr ->
  expr
pattern IForeachIndex t n fn <- IStandardLib StdForeachIndex [t, argExpr -> n, argExpr -> fn]
  where
    IForeachIndex t n fn = IStandardLib StdForeachIndex [t, Arg mempty Explicit Relevant n, Arg mempty Explicit Relevant fn]

--------------------------------------------------------------------------------
-- WHNFValue Function patterns

-- TODO this should really be removed.
pattern VBuiltinFunction :: (HasStandardData builtin) => BuiltinFunction -> Spine closure builtin -> Value closure builtin
pattern VBuiltinFunction f args <- VBuiltin (getBuiltinFunction -> Just f) args
  where
    VBuiltinFunction f args = VBuiltin (mkBuiltinFunction f) args
