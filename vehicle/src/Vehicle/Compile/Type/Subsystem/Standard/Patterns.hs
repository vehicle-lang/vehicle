module Vehicle.Compile.Type.Subsystem.Standard.Patterns where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

pattern NullaryTypeExpr :: Provenance -> BuiltinType -> Expr binder var StandardBuiltin
pattern NullaryTypeExpr p b = Builtin p (CType (StandardBuiltinType b))

pattern TypeExpr ::
  Provenance ->
  BuiltinType ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern TypeExpr p b args = BuiltinExpr p (CType (StandardBuiltinType b)) args

pattern BoolType :: Provenance -> Expr binder var StandardBuiltin
pattern BoolType p = NullaryTypeExpr p Bool

pattern NatType :: Provenance -> Expr binder var StandardBuiltin
pattern NatType p = NullaryTypeExpr p Nat

pattern IntType :: Provenance -> Expr binder var StandardBuiltin
pattern IntType p = NullaryTypeExpr p Int

pattern RatType :: Provenance -> Expr binder var StandardBuiltin
pattern RatType p = NullaryTypeExpr p Rat

pattern ListType :: Provenance -> Expr binder var StandardBuiltin -> Expr binder var StandardBuiltin
pattern ListType p tElem <- TypeExpr p List [ExplicitArg _ tElem]

pattern RawListType :: Provenance -> Expr binder var StandardBuiltin
pattern RawListType p = NullaryTypeExpr p List

pattern VectorType ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern VectorType p tElem tDim <-
  TypeExpr
    p
    Vector
    [ ExplicitArg _ tElem,
      ExplicitArg _ tDim
      ]
  where
    VectorType p tElem tDim =
      TypeExpr
        p
        Vector
        [ ExplicitArg p tElem,
          ExplicitArg p tDim
        ]

pattern TensorType ::
  Provenance ->
  Expr binder var builtin ->
  Expr binder var builtin ->
  Expr binder var builtin
pattern TensorType p tElem tDims <-
  App
    p
    (FreeVar _ TensorIdent)
    [ ExplicitArg _ tElem,
      ExplicitArg _ tDims
      ]

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass ::
  Provenance ->
  TypeClass ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern BuiltinTypeClass p tc args <- BuiltinExpr p (CType (StandardTypeClass tc)) args
  where
    BuiltinTypeClass p tc args = BuiltinExpr p (CType (StandardTypeClass tc)) args

pattern HasVecLitsExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasVecLitsExpr p tElem n tCont <-
  BuiltinTypeClass
    p
    HasVecLits
    [ ExplicitArg _ tElem,
      ExplicitArg _ n,
      ExplicitArg _ tCont
      ]

pattern HasFoldExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasFoldExpr p tElem tCont <-
  BuiltinTypeClass
    p
    HasFold
    [ ExplicitArg _ tElem,
      ExplicitArg _ tCont
      ]

pattern HasQuantifierInExpr ::
  Provenance ->
  Quantifier ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasQuantifierInExpr p q tElem tCont <-
  BuiltinTypeClass
    p
    (HasQuantifierIn q)
    [ ExplicitArg _ tElem,
      ExplicitArg _ tCont
      ]

pattern HasNatLitsExpr ::
  Provenance ->
  Int ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasNatLitsExpr p n t <-
  BuiltinTypeClass
    p
    (HasNatLits n)
    [ ExplicitArg _ t
      ]

pattern HasRatLitsExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasRatLitsExpr p t <-
  BuiltinTypeClass
    p
    HasRatLits
    [ ExplicitArg _ t
      ]

pattern HasArithOp2Expr ::
  Provenance ->
  TypeClass ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasArithOp2Expr p tc t1 t2 t3 <-
  BuiltinTypeClass
    p
    tc
    [ ExplicitArg _ t1,
      ExplicitArg _ t2,
      ExplicitArg _ t3
      ]

pattern HasAddExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasAddExpr p t1 t2 t3 <- HasArithOp2Expr p HasAdd t1 t2 t3

pattern HasSubExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasSubExpr p t1 t2 t3 <- HasArithOp2Expr p HasSub t1 t2 t3

pattern HasMulExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasMulExpr p t1 t2 t3 <- HasArithOp2Expr p HasMul t1 t2 t3

pattern HasDivExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasDivExpr p t1 t2 t3 <- HasArithOp2Expr p HasDiv t1 t2 t3

pattern HasNegExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasNegExpr p argType resType <-
  BuiltinTypeClass
    p
    HasNeg
    [ ExplicitArg _ argType,
      ExplicitArg _ resType
      ]

pattern HasOrdExpr ::
  Provenance ->
  OrderOp ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasOrdExpr p ord arg1Type arg2Type resType <-
  BuiltinTypeClass
    p
    (HasOrd ord)
    [ ExplicitArg _ arg1Type,
      ExplicitArg _ arg2Type,
      ExplicitArg _ resType
      ]

pattern HasEqExpr ::
  Provenance ->
  EqualityOp ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern HasEqExpr p eq arg1Type arg2Type resType <-
  BuiltinTypeClass
    p
    (HasEq eq)
    [ ExplicitArg _ arg1Type,
      ExplicitArg _ arg2Type,
      ExplicitArg _ resType
      ]

--------------------------------------------------------------------------------
-- Type class ops
--------------------------------------------------------------------------------

pattern BuiltinTypeClassOp ::
  Provenance ->
  TypeClassOp ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern BuiltinTypeClassOp p tc args <- BuiltinExpr p (CType (StandardTypeClassOp tc)) args
  where
    BuiltinTypeClassOp p tc args = BuiltinExpr p (CType (StandardTypeClassOp tc)) args

-- | Matches on `forall` and `exists`, but not `foreach`
pattern QuantifierTCExpr ::
  Provenance ->
  Quantifier ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern QuantifierTCExpr p q binder body <-
  BuiltinTypeClassOp
    p
    (QuantifierTC q)
    [ ImplicitArg _ _,
      InstanceArg _ _,
      ExplicitArg _ (Lam _ binder body)
      ]

pattern ForallTCExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ForallTCExpr p binder body <- QuantifierTCExpr p Forall binder body

pattern ExistsTCExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ExistsTCExpr p binder body <- QuantifierTCExpr p Exists binder body

pattern AddTCExpr ::
  Provenance ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern AddTCExpr p args <-
  BuiltinTypeClassOp
    p
    AddTC
    ( ImplicitArg _ _
        :| ImplicitArg _ _
        : ImplicitArg _ _
        : InstanceArg _ _
        : args
      )

pattern SubTCExpr ::
  Provenance ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern SubTCExpr p args <-
  BuiltinTypeClassOp
    p
    SubTC
    ( ImplicitArg _ _
        :| ImplicitArg _ _
        : ImplicitArg _ _
        : InstanceArg _ _
        : args
      )

pattern EqualityTCExpr ::
  Provenance ->
  EqualityOp ->
  Type binder var StandardBuiltin ->
  Type binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern EqualityTCExpr p op t1 t2 solution explicitArgs <-
  BuiltinTypeClassOp
    p
    (EqualsTC op)
    ( ImplicitArg _ t1
        :| ImplicitArg _ t2
        : InstanceArg _ solution
        : explicitArgs
      )

pattern OrderTCExpr ::
  Provenance ->
  OrderOp ->
  Type binder var StandardBuiltin ->
  Type binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern OrderTCExpr p op t1 t2 solution explicitArgs <-
  BuiltinTypeClassOp
    p
    (OrderTC op)
    ( ImplicitArg _ t1
        :| ImplicitArg _ t2
        : InstanceArg _ solution
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

pattern NullaryConstructorExpr :: Provenance -> BuiltinConstructor -> Expr binder var StandardBuiltin
pattern NullaryConstructorExpr p b = Builtin p (CConstructor b)

pattern ConstructorExpr ::
  Provenance ->
  BuiltinConstructor ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern ConstructorExpr p b args = BuiltinExpr p (CConstructor b) args

pattern UnitLiteral :: Provenance -> Expr binder var StandardBuiltin
pattern UnitLiteral p = NullaryConstructorExpr p LUnit

pattern BoolLiteral :: Provenance -> Bool -> Expr binder var StandardBuiltin
pattern BoolLiteral p n = NullaryConstructorExpr p (LBool n)

pattern IndexLiteral :: Provenance -> Int -> Expr binder var StandardBuiltin
pattern IndexLiteral p x = NullaryConstructorExpr p (LIndex x)

pattern NatLiteral :: Provenance -> Int -> Expr binder var StandardBuiltin
pattern NatLiteral p n = NullaryConstructorExpr p (LNat n)

pattern IntLiteral :: Provenance -> Int -> Expr binder var StandardBuiltin
pattern IntLiteral p n = NullaryConstructorExpr p (LInt n)

pattern RatLiteral :: Provenance -> Rational -> Expr binder var StandardBuiltin
pattern RatLiteral p n = NullaryConstructorExpr p (LRat n)

pattern VecLiteral ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern VecLiteral p tElem xs <-
  ConstructorExpr p (LVec _) (ImplicitArg _ tElem :| xs)

pattern TrueExpr :: Provenance -> Expr binder var StandardBuiltin
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: Provenance -> Expr binder var StandardBuiltin
pattern FalseExpr p = BoolLiteral p False

pattern NilExpr :: Provenance -> Type binder var StandardBuiltin -> Expr binder var StandardBuiltin
pattern NilExpr p tElem <- ConstructorExpr p Nil [ImplicitArg _ tElem]
  where
    NilExpr p tElem = ConstructorExpr p Nil [ImplicitArg p tElem]

pattern ConsExpr ::
  Provenance ->
  Type binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern ConsExpr p tElem explicitArgs <-
  ConstructorExpr
    p
    Cons
    ( ImplicitArg _ tElem
        :| explicitArgs
      )
  where
    ConsExpr p tElem explicitArgs =
      ConstructorExpr
        p
        Cons
        ( ImplicitArg p tElem
            :| explicitArgs
        )

pattern AppConsExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern AppConsExpr p tElem x xs <-
  ConsExpr
    p
    tElem
    [ ExplicitArg _ x,
      ExplicitArg _ xs
      ]

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern NullaryBuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  Expr binder var (NormalisableBuiltin types)
pattern NullaryBuiltinFunctionExpr p b = Builtin p (CFunction b)

pattern BuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg binder var (NormalisableBuiltin types)) ->
  Expr binder var (NormalisableBuiltin types)
pattern BuiltinFunctionExpr p b args = BuiltinExpr p (CFunction b) args

pattern QuantifierExpr ::
  Provenance ->
  Quantifier ->
  QuantifierDomain ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern QuantifierExpr p q dom binder body <-
  BuiltinFunctionExpr
    p
    (Quantifier q dom)
    [ ExplicitArg _ (Lam _ binder body)
      ]
  where
    QuantifierExpr p q dom binder body =
      BuiltinFunctionExpr
        p
        (Quantifier q dom)
        [ ExplicitArg p (Lam p binder body)
        ]

pattern ExistsNatExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ExistsNatExpr p binder body =
  QuantifierExpr p Exists QuantNat binder body

pattern ForallNatExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ForallNatExpr p binder body =
  QuantifierExpr p Forall QuantNat binder body

pattern ExistsIntExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ExistsIntExpr p binder body =
  QuantifierExpr p Exists QuantInt binder body

pattern ForallIntExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ForallIntExpr p binder body =
  QuantifierExpr p Forall QuantInt binder body

pattern ExistsRatExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ExistsRatExpr p binder body =
  QuantifierExpr p Exists QuantRat binder body

pattern ForallRatExpr ::
  Provenance ->
  Binder binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin
pattern ForallRatExpr p binder body =
  QuantifierExpr p Forall QuantRat binder body

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern IfExpr p tRes args <-
  BuiltinFunctionExpr
    p
    If
    ( ImplicitArg _ tRes
        :| args
      )
  where
    IfExpr p tRes args =
      BuiltinFunctionExpr
        p
        If
        ( ImplicitArg p tRes
            :| args
        )

--------------------------------------------------------------------------------
-- Conversion operations

pattern FromNatExpr ::
  Provenance ->
  Int ->
  FromNatDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern FromNatExpr p n dom args <- BuiltinFunctionExpr p (FromNat n dom) args

pattern FromRatExpr ::
  Provenance ->
  FromRatDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern FromRatExpr p dom args <- BuiltinFunctionExpr p (FromRat dom) args

{-
pattern FromVecExpr ::
  Provenance ->
  FromVecDomain ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern FromVecExpr p dom explicitArgs <-
  FreeVar
    p
    (FromVec dom)
    ( ImplicitArg _ _
        :| ImplicitArg _ _
        : explicitArgs
      )
-}
--------------------------------------------------------------------------------
-- Boolean operations

pattern BooleanOp2Expr ::
  BuiltinFunction ->
  Provenance ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern BooleanOp2Expr op p explicitArgs = BuiltinFunctionExpr p op explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg binder var StandardBuiltin) -> Expr binder var StandardBuiltin
pattern AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg binder var StandardBuiltin) -> Expr binder var StandardBuiltin
pattern OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr :: Provenance -> NonEmpty (Arg binder var StandardBuiltin) -> Expr binder var StandardBuiltin
pattern ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr :: Provenance -> NonEmpty (Arg binder var StandardBuiltin) -> Expr binder var StandardBuiltin
pattern NotExpr p explicitArgs = BuiltinFunctionExpr p Not explicitArgs

pattern AppliedAndExpr :: Provenance -> Expr binder var StandardBuiltin -> Expr binder var StandardBuiltin -> Expr binder var StandardBuiltin
pattern AppliedAndExpr p x y <- AndExpr p [ExplicitArg _ x, ExplicitArg _ y]

pattern AppliedOrExpr :: Provenance -> Expr binder var StandardBuiltin -> Expr binder var StandardBuiltin -> Expr binder var StandardBuiltin
pattern AppliedOrExpr p x y <- OrExpr p [ExplicitArg _ x, ExplicitArg _ y]

--------------------------------------------------------------------------------
-- NumericOp2

pattern NegExpr ::
  Provenance ->
  NegDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern NegExpr p dom args = BuiltinFunctionExpr p (Neg dom) args

pattern AddExpr ::
  Provenance ->
  AddDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern AddExpr p dom args = BuiltinFunctionExpr p (Add dom) args

pattern SubExpr ::
  Provenance ->
  SubDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern SubExpr p dom args = BuiltinFunctionExpr p (Sub dom) args

pattern MulExpr ::
  Provenance ->
  MulDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern MulExpr p dom args = BuiltinFunctionExpr p (Mul dom) args

pattern DivExpr ::
  Provenance ->
  DivDomain ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern DivExpr p dom args = BuiltinFunctionExpr p (Div dom) args

--------------------------------------------------------------------------------
-- EqualityOp

pattern EqualityExpr ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern EqualityExpr p dom op args = BuiltinFunctionExpr p (Equals dom op) args

--------------------------------------------------------------------------------
-- OrderOp

pattern OrderExpr ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  NonEmpty (Arg binder var StandardBuiltin) ->
  Expr binder var StandardBuiltin
pattern OrderExpr p dom op args = BuiltinFunctionExpr p (Order dom op) args

--------------------------------------------------------------------------------
-- At

pattern AtExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern AtExpr p tElem tDim explicitArgs <-
  BuiltinFunctionExpr
    p
    At
    ( ImplicitArg _ tElem
        :| ImplicitArg _ tDim
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Vector

pattern FoldVectorExpr ::
  Provenance ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  Expr binder var StandardBuiltin ->
  [Arg binder var StandardBuiltin] ->
  Expr binder var StandardBuiltin
pattern FoldVectorExpr p tElem size tRes explicitArgs <-
  BuiltinFunctionExpr
    p
    (Fold FoldVector)
    ( ImplicitArg _ tElem
        :| ImplicitArg _ size
        : ImplicitArg _ tRes
        : explicitArgs
      )

-----------------------------------------------------------------------------
-- Constructors

mkList ::
  Provenance ->
  Expr var binder StandardBuiltin ->
  [Expr var binder StandardBuiltin] ->
  Expr var binder StandardBuiltin
mkList p elemType = foldr cons nil
  where
    nil = NilExpr p elemType
    cons x xs =
      ConsExpr
        p
        elemType
        [ ExplicitArg p x,
          ExplicitArg p xs
        ]
