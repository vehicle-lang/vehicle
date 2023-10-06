module Vehicle.Data.BuiltinInterface.Expr where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.Tensor
import Vehicle.Libraries.StandardLibrary.Definitions
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Prelude hiding (pi)

--------------------------------------------------------------------------------
-- Type exprs

pattern NullaryTypeExpr :: (HasStandardTypes builtin) => Provenance -> BuiltinType -> Expr var builtin
pattern NullaryTypeExpr p b <- Builtin p (getBuiltinType -> Just b)
  where
    NullaryTypeExpr p b = Builtin p (mkBuiltinType b)

pattern TypeExpr ::
  (HasStandardTypes builtin) =>
  Provenance ->
  BuiltinType ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern TypeExpr p b args <- BuiltinExpr p (getBuiltinType -> Just b) args
  where
    TypeExpr p b args = BuiltinExpr p (mkBuiltinType b) args

pattern BoolType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern BoolType p = NullaryTypeExpr p Bool

pattern NatType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern NatType p = NullaryTypeExpr p Nat

pattern IntType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern IntType p = NullaryTypeExpr p Int

pattern RatType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern RatType p = NullaryTypeExpr p Rat

pattern ListType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin -> Expr var builtin
pattern ListType p tElem <- TypeExpr p List [RelevantExplicitArg _ tElem]

pattern RawListType :: (HasStandardTypes builtin) => Provenance -> Expr var builtin
pattern RawListType p = NullaryTypeExpr p List

pattern VectorType ::
  (HasStandardTypes builtin) =>
  Provenance ->
  Expr var builtin ->
  Expr var builtin ->
  Expr var builtin
pattern VectorType p tElem tDim <-
  TypeExpr
    p
    Vector
    [ RelevantExplicitArg _ tElem,
      IrrelevantExplicitArg _ tDim
      ]

--------------------------------------------------------------------------------
-- Expr constructors patterns

pattern UnitLiteral :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern UnitLiteral p <- Builtin p (getBuiltinConstructor -> Just LUnit)
  where
    UnitLiteral p = Builtin p (mkBuiltinConstructor LUnit)

pattern BoolLiteral :: (HasStandardData builtin) => Provenance -> Bool -> Expr var builtin
pattern BoolLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LBool n))
  where
    BoolLiteral p n = Builtin p (mkBuiltinConstructor (LBool n))

pattern TrueExpr :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: (HasStandardData builtin) => Provenance -> Expr var builtin
pattern FalseExpr p = BoolLiteral p False

pattern IndexLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern IndexLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LIndex n))
  where
    IndexLiteral p n = Builtin p (mkBuiltinConstructor (LIndex n))

pattern NatLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern NatLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LNat n))
  where
    NatLiteral p n = Builtin p (mkBuiltinConstructor (LNat n))

pattern IntLiteral :: (HasStandardData builtin) => Provenance -> Int -> Expr var builtin
pattern IntLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LInt n))
  where
    IntLiteral p n = Builtin p (mkBuiltinConstructor (LInt n))

pattern RatLiteral :: (HasStandardData builtin) => Provenance -> Rational -> Expr var builtin
pattern RatLiteral p n <- Builtin p (getBuiltinConstructor -> Just (LRat n))
  where
    RatLiteral p n = Builtin p (mkBuiltinConstructor (LRat n))

pattern VecLiteral ::
  (HasStandardData builtin) =>
  Provenance ->
  Type var builtin ->
  [Arg var builtin] ->
  Expr var builtin
pattern VecLiteral p tElem xs <-
  BuiltinExpr p (getBuiltinConstructor -> Just (LVec _)) (RelevantImplicitArg _ tElem :| xs)

mkList ::
  forall var builtin.
  (HasStandardData builtin) =>
  Provenance ->
  Expr var builtin ->
  [Expr var builtin] ->
  Expr var builtin
mkList p elemType = foldr mkCons mkNil
  where
    mkNil :: Expr var builtin
    mkNil = BuiltinExpr p (mkBuiltinConstructor Nil) [Arg p (Implicit True) Relevant elemType]

    mkCons ::
      (HasStandardData builtin) =>
      Expr var builtin ->
      Expr var builtin ->
      Expr var builtin
    mkCons x xs =
      BuiltinExpr
        p
        (mkBuiltinConstructor Cons)
        ( Arg p (Implicit True) Relevant elemType
            :| [ Arg p Explicit Relevant x,
                 Arg p Explicit Relevant xs
               ]
        )

--------------------------------------------------------------------------------
-- Boolean operations

pattern BuiltinFunctionExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern BuiltinFunctionExpr p b args <- BuiltinExpr p (getBuiltinFunction -> Just b) args
  where
    BuiltinFunctionExpr p b args = BuiltinExpr p (mkBuiltinFunction b) args

pattern QuantifierExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  Quantifier ->
  Binder var builtin ->
  Expr var builtin ->
  Expr var builtin
pattern QuantifierExpr p q binder body <-
  BuiltinFunctionExpr
    p
    (Quantifier q)
    [ RelevantExplicitArg _ (Lam _ binder body)
      ]
  where
    QuantifierExpr p q binder body =
      BuiltinFunctionExpr
        p
        (Quantifier q)
        [ Arg p Explicit Relevant (Lam p binder body)
        ]

pattern ExistsExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  Binder var builtin ->
  Expr var builtin ->
  Expr var builtin
pattern ExistsExpr p binder body =
  QuantifierExpr p Exists binder body

pattern ForallExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  Binder var builtin ->
  Expr var builtin ->
  Expr var builtin
pattern ForallExpr p binder body =
  QuantifierExpr p Forall binder body

pattern BooleanOp2Expr ::
  (HasStandardData builtin) =>
  BuiltinFunction ->
  Provenance ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern BooleanOp2Expr op p explicitArgs = BuiltinFunctionExpr p op explicitArgs

pattern AndExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr ::
  (HasStandardData builtin) =>
  Provenance ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern NotExpr p explicitArgs = BuiltinFunctionExpr p Not explicitArgs

pattern EqualityExpr ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  NonEmpty (Arg var Builtin) ->
  Expr var Builtin
pattern EqualityExpr p dom op args = BuiltinFunctionExpr p (Equals dom op) args

pattern OrderExpr ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  NonEmpty (Arg var Builtin) ->
  Expr var Builtin
pattern OrderExpr p dom op args = BuiltinFunctionExpr p (Order dom op) args

pattern IfExpr ::
  Provenance ->
  Expr var Builtin ->
  [Arg var Builtin] ->
  Expr var Builtin
pattern IfExpr p tRes args <-
  BuiltinFunctionExpr
    p
    If
    ( RelevantImplicitArg _ tRes
        :| args
      )
  where
    IfExpr p tRes args =
      BuiltinFunctionExpr
        p
        If
        ( Arg p (Implicit True) Relevant tRes
            :| args
        )

negExpr :: Expr var Builtin -> Expr var Builtin
negExpr body = NotExpr mempty [Arg mempty Explicit Relevant body]

pattern StandardLib :: StdLibFunction -> NonEmpty (Arg var builtin) -> Expr var builtin
pattern StandardLib fn spine <- App _ (FreeVar _ (findStdLibFunction -> Just fn)) spine
  where
    StandardLib fn spine = App mempty (FreeVar mempty (identifierOf fn)) spine

tensorToExpr :: (a -> Expr var Builtin) -> Tensor a -> Expr var Builtin
tensorToExpr mkElem = foldMapTensor mkElem (\xs -> BuiltinExpr mempty (BuiltinConstructor (LVec (length xs))) (Arg mempty (Implicit True) Relevant (mkHole mempty "_t") :| fmap (Arg mempty Explicit Relevant) xs))
