module Vehicle.Data.BuiltinInterface.ASTInterface where

import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
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
-- HasStandardData

-- | Indicates that this set of builtins has the standard builtin constructors
-- and functions.
class (Show expr) => HasStandardDataExpr expr where
  mkConstructor :: Provenance -> BuiltinConstructor -> [GenericArg expr] -> expr
  getConstructor :: expr -> Maybe (Provenance, BuiltinConstructor, [GenericArg expr])

  mkFunction :: Provenance -> BuiltinFunction -> [GenericArg expr] -> expr
  getFunction :: expr -> Maybe (Provenance, BuiltinFunction, [GenericArg expr])

  mkFreeVar :: Provenance -> Identifier -> [GenericArg expr] -> expr
  getFreeVar :: expr -> Maybe (Provenance, Identifier, [GenericArg expr])

instance (Show var, HasStandardData builtin) => HasStandardDataExpr (Expr var builtin) where
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

instance (HasStandardData builtin) => HasStandardDataExpr (Value strategy builtin) where
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

  getFreeVar = \case
    VFreeVar ident args -> Just (mempty, ident, args)
    _ -> Nothing

  mkFreeVar _p = VFreeVar

instance (HasStandardTypes builtin) => HasStandardTypesExpr (Expr var builtin) where
  mkType p b = normAppList (Builtin p (mkBuiltinType b))
  getType e = case getBuiltinApp e of
    Just (p, b, args) -> case getBuiltinType b of
      Just t -> Just (p, t, args)
      Nothing -> Nothing
    _ -> Nothing

instance (HasStandardTypes builtin) => HasStandardTypesExpr (Value strategy builtin) where
  mkType _p b = VBuiltin (mkBuiltinType b)
  getType e = case e of
    VBuiltin b args -> case getBuiltinType b of
      Just t -> Just (mempty, t, args)
      Nothing -> Nothing
    _ -> Nothing

--------------------------------------------------------------------------------
-- HasStandardTypes

-- | Indicates that this set of builtins has the standard set of types.
class HasStandardTypesExpr expr where
  mkType :: Provenance -> BuiltinType -> [GenericArg expr] -> expr
  getType :: expr -> Maybe (Provenance, BuiltinType, [GenericArg expr])

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
pattern IRawBuiltinConstructor :: (HasStandardDataExpr expr) => Provenance -> BuiltinConstructor -> expr
pattern IRawBuiltinConstructor p t <- (getConstructor -> Just (p, t, []))
  where
    IRawBuiltinConstructor p t = mkConstructor p t []

pattern IUnitLiteral :: (HasStandardDataExpr expr) => Provenance -> expr
pattern IUnitLiteral p = IRawBuiltinConstructor p LUnit

pattern IBoolLiteral :: (HasStandardDataExpr expr) => Provenance -> Bool -> expr
pattern IBoolLiteral p n = IRawBuiltinConstructor p (LBool n)

pattern ITrueExpr :: (HasStandardDataExpr expr) => Provenance -> expr
pattern ITrueExpr p = IBoolLiteral p True

pattern IFalseExpr :: (HasStandardDataExpr expr) => Provenance -> expr
pattern IFalseExpr p = IBoolLiteral p False

pattern IIndexLiteral :: (HasStandardDataExpr expr) => Provenance -> Int -> expr
pattern IIndexLiteral p n = IRawBuiltinConstructor p (LIndex n)

pattern INatLiteral :: (HasStandardDataExpr expr) => Provenance -> Int -> expr
pattern INatLiteral p n = IRawBuiltinConstructor p (LNat n)

pattern IRatLiteral :: (HasStandardDataExpr expr) => Provenance -> Rational -> expr
pattern IRatLiteral p n = IRawBuiltinConstructor p (LRat n)

pattern INil :: (HasStandardDataExpr expr) => expr
pattern INil <- (getConstructor -> Just (_, Nil, _))

pattern ICons :: (HasStandardDataExpr expr) => GenericArg expr -> GenericArg expr -> expr
pattern ICons x xs <- (getConstructor -> Just (_, Cons, filter isExplicit -> [x, xs]))

pattern IVecLiteral :: (HasStandardDataExpr expr) => [GenericArg expr] -> expr
pattern IVecLiteral xs <- (getConstructor -> Just (_, LVec _, filter isExplicit -> xs))

{-
mkVList :: (HasStandardDataExpr builtin) => [expr] -> expr
mkVList = foldr mkCons mkNil
  where
    mkNil = IBuiltin (mkBuiltinConstructor Nil) []
    mkCons y ys = IBuiltin (mkBuiltinConstructor Cons) (Arg mempty Explicit Relevant <$> [y, ys])

-}
mkVLVec :: (HasStandardDataExpr expr) => [expr] -> expr
mkVLVec xs =
  mkConstructor
    mempty
    (LVec (length xs))
    (Arg mempty (Implicit True) Relevant (IUnitLiteral mempty) : (Arg mempty Explicit Relevant <$> xs))

{-
pattern IecLiteral ::
  (HasStandardData expr) =>
  Provenance ->
  Type Iar builtin ->
  [Arg Iar builtin] ->
  Expr Iar builtin
pattern IecLiteral p tElem xs <-
  BuiltinExpr p (getBuiltinConstructor -> Just (LVec _)) (RelevantImplicitArg _ tElem :| xs)

mkList ::
  forall Iar builtin.
  (HasStandardData expr) =>
  Provenance ->
  Expr Iar builtin ->
  [Expr Iar builtin] ->
  Expr Iar builtin
mkList p elemType = foldr mkCons mkNil
  where
    mkNil :: Expr Iar builtin
    mkNil = BuiltinExpr p (mkBuiltinConstructor Nil) [Arg p (Implicit True) Relevant elemType]

    mkCons ::
      (HasStandardData expr) =>
      Expr Iar builtin ->
      Expr Iar builtin ->
      Expr Iar builtin
    mkCons x xs =
      BuiltinExpr
        p
        (mkBuiltinConstructor Cons)
        ( Arg p (Implicit True) Relevant elemType
            :| [ Arg p Explicit Relevant x,
                 Arg p Explicit Relevant xs
               ]
        )
        -}

getNatLiteral :: (HasStandardDataExpr expr) => expr -> Maybe Int
getNatLiteral = \case
  INatLiteral _ d -> Just d
  _ -> Nothing

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

pattern IOrder :: (HasStandardDataExpr expr) => OrderDomain -> OrderOp -> expr -> expr -> expr
pattern IOrder dom op x y <- BuiltinFunc (Order dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : _)

pattern IOrderRat :: (HasStandardDataExpr expr) => OrderOp -> expr -> expr -> expr
pattern IOrderRat op x y = IOp2 (Order OrderRat op) x y

pattern IEqualOp :: (HasStandardDataExpr expr) => EqualityDomain -> EqualityOp -> expr -> expr -> expr
pattern IEqualOp dom op x y <- BuiltinFunc (Equals dom op) (reverse -> (argExpr -> y) : (argExpr -> x) : _)

pattern IEqual :: (HasStandardDataExpr expr) => EqualityDomain -> expr -> expr -> expr
pattern IEqual dom x y <- IEqualOp dom Eq x y

pattern IEqualRat :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IEqualRat x y = IOp2 (Equals EqRat Eq) x y

pattern INotEqual :: (HasStandardDataExpr expr) => EqualityDomain -> expr -> expr -> expr
pattern INotEqual dom x y <- IEqualOp dom Neq x y

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

{-
pattern IFoldList :: (HasStandardDataExpr expr) => expr -> expr -> expr -> expr
pattern IFoldList <- BuiltinFunc FoldList [_, _, _, _, argExpr -> f, xs, ys]
-}
pattern IZipWithVectorArgs ::
  (HasStandardDataExpr expr) =>
  expr ->
  GenericArg expr ->
  GenericArg expr ->
  [GenericArg expr]
pattern IZipWithVectorArgs f xs ys <- [_, _, _, _, argExpr -> f, xs, ys]

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

{-
pattern ITensorType :: (HasStandardTypes builtin) => WHNFType builtin -> expr -> WHNFType builtin
pattern ITensorType tElem dims <-
  IFreeVar TensorIdent [RelevantExplicitArg _ tElem, RelevantExplicitArg _ dims]

-}

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

pattern IVectorAdd :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IVectorAdd x y <- IStandardLib StdAddVector [_, _, _, _, _, argExpr -> x, argExpr -> y]

pattern IVectorSub :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IVectorSub x y <- IStandardLib StdSubVector [_, _, _, _, _, argExpr -> x, argExpr -> y]

pattern IAt :: (HasStandardDataExpr expr) => expr -> expr -> expr
pattern IAt xs i <- BuiltinFunc At [_t, _n, argExpr -> xs, argExpr -> i]

pattern IFoldVector ::
  (HasStandardDataExpr expr) =>
  expr ->
  expr ->
  expr ->
  expr
pattern IFoldVector f e xs <- BuiltinFunc FoldVector [_n, _a, _b, argExpr -> f, argExpr -> e, argExpr -> xs]

pattern IMapVector ::
  (HasStandardDataExpr expr) =>
  expr ->
  expr ->
  expr
pattern IMapVector f xs <- BuiltinFunc MapVector [_n, _a, _b, argExpr -> f, argExpr -> xs]
