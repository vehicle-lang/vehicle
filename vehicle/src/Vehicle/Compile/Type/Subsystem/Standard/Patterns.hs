module Vehicle.Compile.Type.Subsystem.Standard.Patterns where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalisable
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

pattern NullaryTypeExpr :: Provenance -> BuiltinType -> Expr var StandardBuiltin
pattern NullaryTypeExpr p b = Builtin p (CType (StandardBuiltinType b))

pattern TypeExpr ::
  Provenance ->
  BuiltinType ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern TypeExpr p b args = BuiltinExpr p (CType (StandardBuiltinType b)) args

pattern BoolType :: Provenance -> Expr var StandardBuiltin
pattern BoolType p = NullaryTypeExpr p Bool

pattern NatType :: Provenance -> Expr var StandardBuiltin
pattern NatType p = NullaryTypeExpr p Nat

pattern IntType :: Provenance -> Expr var StandardBuiltin
pattern IntType p = NullaryTypeExpr p Int

pattern RatType :: Provenance -> Expr var StandardBuiltin
pattern RatType p = NullaryTypeExpr p Rat

pattern ListType :: Provenance -> Expr var StandardBuiltin -> Expr var StandardBuiltin
pattern ListType p tElem <- TypeExpr p List [RelevantExplicitArg _ tElem]

pattern RawListType :: Provenance -> Expr var StandardBuiltin
pattern RawListType p = NullaryTypeExpr p List

pattern VectorType ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern VectorType p tElem tDim <-
  TypeExpr
    p
    Vector
    [ RelevantExplicitArg _ tElem,
      IrrelevantExplicitArg _ tDim
      ]
  where
    VectorType p tElem tDim =
      TypeExpr
        p
        Vector
        [ RelevantExplicitArg p tElem,
          IrrelevantExplicitArg p tDim
        ]

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass ::
  Provenance ->
  TypeClass ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BuiltinTypeClass p tc args <- BuiltinExpr p (CType (StandardTypeClass tc)) args
  where
    BuiltinTypeClass p tc args = BuiltinExpr p (CType (StandardTypeClass tc)) args

pattern HasVecLitsExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasVecLitsExpr p tElem n tCont <-
  BuiltinTypeClass
    p
    HasVecLits
    [ RelevantExplicitArg _ tElem,
      IrrelevantExplicitArg _ n,
      RelevantExplicitArg _ tCont
      ]

pattern HasFoldExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasFoldExpr p tElem tCont <-
  BuiltinTypeClass
    p
    HasFold
    [ RelevantExplicitArg _ tElem,
      RelevantExplicitArg _ tCont
      ]

pattern HasQuantifierInExpr ::
  Provenance ->
  Quantifier ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasQuantifierInExpr p q tElem tCont <-
  BuiltinTypeClass
    p
    (HasQuantifierIn q)
    [ RelevantExplicitArg _ tElem,
      RelevantExplicitArg _ tCont
      ]

pattern HasNatLitsExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasNatLitsExpr p t <-
  BuiltinTypeClass
    p
    HasNatLits
    [ RelevantExplicitArg _ t
      ]

pattern HasRatLitsExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasRatLitsExpr p t <-
  BuiltinTypeClass
    p
    HasRatLits
    [ RelevantExplicitArg _ t
      ]

pattern HasArithOp2Expr ::
  Provenance ->
  TypeClass ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasArithOp2Expr p tc t1 t2 t3 <-
  BuiltinTypeClass
    p
    tc
    [ RelevantExplicitArg _ t1,
      RelevantExplicitArg _ t2,
      RelevantExplicitArg _ t3
      ]

pattern HasAddExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasAddExpr p t1 t2 t3 <- HasArithOp2Expr p HasAdd t1 t2 t3

pattern HasSubExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasSubExpr p t1 t2 t3 <- HasArithOp2Expr p HasSub t1 t2 t3

pattern HasMulExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasMulExpr p t1 t2 t3 <- HasArithOp2Expr p HasMul t1 t2 t3

pattern HasDivExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasDivExpr p t1 t2 t3 <- HasArithOp2Expr p HasDiv t1 t2 t3

pattern HasNegExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasNegExpr p argType resType <-
  BuiltinTypeClass
    p
    HasNeg
    [ RelevantExplicitArg _ argType,
      RelevantExplicitArg _ resType
      ]

pattern HasOrdExpr ::
  Provenance ->
  OrderOp ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasOrdExpr p ord arg1Type arg2Type resType <-
  BuiltinTypeClass
    p
    (HasOrd ord)
    [ RelevantExplicitArg _ arg1Type,
      RelevantExplicitArg _ arg2Type,
      RelevantExplicitArg _ resType
      ]

pattern HasEqExpr ::
  Provenance ->
  EqualityOp ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern HasEqExpr p eq arg1Type arg2Type resType <-
  BuiltinTypeClass
    p
    (HasEq eq)
    [ RelevantExplicitArg _ arg1Type,
      RelevantExplicitArg _ arg2Type,
      RelevantExplicitArg _ resType
      ]

--------------------------------------------------------------------------------
-- Type class ops
--------------------------------------------------------------------------------

pattern BuiltinTypeClassOp ::
  Provenance ->
  TypeClassOp ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BuiltinTypeClassOp p tc args <- BuiltinExpr p (CType (StandardTypeClassOp tc)) args
  where
    BuiltinTypeClassOp p tc args = BuiltinExpr p (CType (StandardTypeClassOp tc)) args

-- | Matches on `forall` and `exists`, but not `foreach`
pattern QuantifierTCExpr ::
  Provenance ->
  Quantifier ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern QuantifierTCExpr p q binder body <-
  BuiltinTypeClassOp
    p
    (QuantifierTC q)
    [ RelevantImplicitArg _ _,
      RelevantInstanceArg _ _,
      RelevantExplicitArg _ (Lam _ binder body)
      ]

pattern ForallTCExpr ::
  Provenance ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern ForallTCExpr p binder body <- QuantifierTCExpr p Forall binder body

pattern ExistsTCExpr ::
  Provenance ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern ExistsTCExpr p binder body <- QuantifierTCExpr p Exists binder body

pattern AddTCExpr ::
  Provenance ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern AddTCExpr p args <-
  BuiltinTypeClassOp
    p
    AddTC
    ( RelevantImplicitArg _ _
        :| RelevantImplicitArg _ _
        : RelevantImplicitArg _ _
        : RelevantInstanceArg _ _
        : args
      )

pattern SubTCExpr ::
  Provenance ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern SubTCExpr p args <-
  BuiltinTypeClassOp
    p
    SubTC
    ( RelevantImplicitArg _ _
        :| RelevantImplicitArg _ _
        : RelevantImplicitArg _ _
        : RelevantInstanceArg _ _
        : args
      )

pattern EqualityTCExpr ::
  Provenance ->
  EqualityOp ->
  Type var StandardBuiltin ->
  Type var StandardBuiltin ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern EqualityTCExpr p op t1 t2 solution explicitArgs <-
  BuiltinTypeClassOp
    p
    (EqualsTC op)
    ( RelevantImplicitArg _ t1
        :| RelevantImplicitArg _ t2
        : RelevantInstanceArg _ solution
        : explicitArgs
      )
  where
    EqualityTCExpr p op t1 t2 solution explicitArgs =
      BuiltinTypeClassOp
        p
        (EqualsTC op)
        ( RelevantImplicitArg p t1
            :| RelevantImplicitArg p t2
            : RelevantInstanceArg p solution
            : explicitArgs
        )

pattern OrderTCExpr ::
  Provenance ->
  OrderOp ->
  Type var StandardBuiltin ->
  Type var StandardBuiltin ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern OrderTCExpr p op t1 t2 solution explicitArgs <-
  BuiltinTypeClassOp
    p
    (OrderTC op)
    ( RelevantImplicitArg _ t1
        :| RelevantImplicitArg _ t2
        : RelevantInstanceArg _ solution
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

pattern NullaryConstructorExpr :: Provenance -> BuiltinConstructor -> Expr var StandardBuiltin
pattern NullaryConstructorExpr p b = Builtin p (CConstructor b)

pattern ConstructorExpr ::
  Provenance ->
  BuiltinConstructor ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern ConstructorExpr p b args = BuiltinExpr p (CConstructor b) args

pattern UnitLiteral :: Provenance -> Expr var StandardBuiltin
pattern UnitLiteral p = NullaryConstructorExpr p LUnit

pattern BoolLiteral :: Provenance -> Bool -> Expr var StandardBuiltin
pattern BoolLiteral p n = NullaryConstructorExpr p (LBool n)

pattern IndexLiteral :: Provenance -> Int -> Expr var StandardBuiltin
pattern IndexLiteral p x = NullaryConstructorExpr p (LIndex x)

pattern NatLiteral :: Provenance -> Int -> Expr var StandardBuiltin
pattern NatLiteral p n = NullaryConstructorExpr p (LNat n)

pattern IntLiteral :: Provenance -> Int -> Expr var StandardBuiltin
pattern IntLiteral p n = NullaryConstructorExpr p (LInt n)

pattern RatLiteral :: Provenance -> Rational -> Expr var StandardBuiltin
pattern RatLiteral p n = NullaryConstructorExpr p (LRat n)

pattern VecLiteral ::
  Provenance ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern VecLiteral p tElem xs <-
  ConstructorExpr p (LVec _) (RelevantImplicitArg _ tElem :| xs)
  where
    VecLiteral p tElem xs =
      ConstructorExpr p (LVec (length xs)) (RelevantImplicitArg p tElem :| xs)

pattern TrueExpr :: Provenance -> Expr var StandardBuiltin
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: Provenance -> Expr var StandardBuiltin
pattern FalseExpr p = BoolLiteral p False

pattern NilExpr :: Provenance -> Type var StandardBuiltin -> Expr var StandardBuiltin
pattern NilExpr p tElem <- ConstructorExpr p Nil [RelevantImplicitArg _ tElem]
  where
    NilExpr p tElem = ConstructorExpr p Nil [RelevantImplicitArg p tElem]

pattern ConsExpr ::
  Provenance ->
  Type var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern ConsExpr p tElem explicitArgs <-
  ConstructorExpr
    p
    Cons
    ( RelevantImplicitArg _ tElem
        :| explicitArgs
      )
  where
    ConsExpr p tElem explicitArgs =
      ConstructorExpr
        p
        Cons
        ( RelevantImplicitArg p tElem
            :| explicitArgs
        )

pattern AppConsExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern AppConsExpr p tElem x xs <-
  ConsExpr
    p
    tElem
    [ RelevantExplicitArg _ x,
      RelevantExplicitArg _ xs
      ]

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern NullaryBuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  Expr var StandardBuiltin
pattern NullaryBuiltinFunctionExpr p b = Builtin p (CFunction b)

pattern BuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BuiltinFunctionExpr p b args = BuiltinExpr p (CFunction b) args

pattern QuantifierExpr ::
  Provenance ->
  Quantifier ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
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
        [ RelevantExplicitArg p (Lam p binder body)
        ]

pattern ExistsExpr ::
  Provenance ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern ExistsExpr p binder body =
  QuantifierExpr p Exists binder body

pattern ForallExpr ::
  Provenance ->
  Binder var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin
pattern ForallExpr p binder body =
  QuantifierExpr p Forall binder body

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
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
        ( RelevantImplicitArg p tRes
            :| args
        )

--------------------------------------------------------------------------------
-- Conversion operations

pattern FromNatExpr ::
  Provenance ->
  FromNatDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern FromNatExpr p dom args <- BuiltinFunctionExpr p (FromNat dom) args

pattern FromRatExpr ::
  Provenance ->
  FromRatDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern FromRatExpr p dom args <- BuiltinFunctionExpr p (FromRat dom) args

{-
pattern FromVecExpr ::
  Provenance ->
  FromVecDomain ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
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
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BooleanOp2Expr op p explicitArgs = BuiltinFunctionExpr p op explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg var StandardBuiltin) -> Expr var StandardBuiltin
pattern AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg var StandardBuiltin) -> Expr var StandardBuiltin
pattern OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr :: Provenance -> NonEmpty (Arg var StandardBuiltin) -> Expr var StandardBuiltin
pattern ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr :: Provenance -> NonEmpty (Arg var StandardBuiltin) -> Expr var StandardBuiltin
pattern NotExpr p explicitArgs = BuiltinFunctionExpr p Not explicitArgs

--------------------------------------------------------------------------------
-- NumericOp2

pattern NegExpr ::
  Provenance ->
  NegDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern NegExpr p dom args = BuiltinFunctionExpr p (Neg dom) args

pattern AddExpr ::
  Provenance ->
  AddDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern AddExpr p dom args = BuiltinFunctionExpr p (Add dom) args

pattern SubExpr ::
  Provenance ->
  SubDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern SubExpr p dom args = BuiltinFunctionExpr p (Sub dom) args

pattern MulExpr ::
  Provenance ->
  MulDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern MulExpr p dom args = BuiltinFunctionExpr p (Mul dom) args

pattern DivExpr ::
  Provenance ->
  DivDomain ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern DivExpr p dom args = BuiltinFunctionExpr p (Div dom) args

--------------------------------------------------------------------------------
-- EqualityOp

pattern EqualityExpr ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern EqualityExpr p dom op args = BuiltinFunctionExpr p (Equals dom op) args

--------------------------------------------------------------------------------
-- OrderOp

pattern OrderExpr ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern OrderExpr p dom op args = BuiltinFunctionExpr p (Order dom op) args

--------------------------------------------------------------------------------
-- At

pattern AtExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern AtExpr p tElem tDim explicitArgs <-
  BuiltinFunctionExpr
    p
    At
    ( RelevantImplicitArg _ tElem
        :| IrrelevantImplicitArg _ tDim
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Vector

pattern FoldVectorExpr ::
  Provenance ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  Expr var StandardBuiltin ->
  [Arg var StandardBuiltin] ->
  Expr var StandardBuiltin
pattern FoldVectorExpr p tElem size tRes explicitArgs <-
  BuiltinFunctionExpr
    p
    (Fold FoldVector)
    ( RelevantImplicitArg _ tElem
        :| IrrelevantImplicitArg _ size
        : RelevantImplicitArg _ tRes
        : explicitArgs
      )

-----------------------------------------------------------------------------
-- Constructors

mkList ::
  Provenance ->
  Expr var StandardBuiltin ->
  [Expr var StandardBuiltin] ->
  Expr var StandardBuiltin
mkList p elemType = foldr cons nil
  where
    nil = NilExpr p elemType
    cons x xs =
      ConsExpr
        p
        elemType
        [ RelevantExplicitArg p x,
          RelevantExplicitArg p xs
        ]
