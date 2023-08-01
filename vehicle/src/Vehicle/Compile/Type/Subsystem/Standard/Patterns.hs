module Vehicle.Compile.Type.Subsystem.Standard.Patterns where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass ::
  Provenance ->
  TypeClass ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BuiltinTypeClass p tc args = BuiltinExpr p (TypeClass tc) args

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
pattern BuiltinTypeClassOp p tc args = BuiltinExpr p (TypeClassOp tc) args

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

-- See `Interface`

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

pattern NullaryBuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  Expr var StandardBuiltin
pattern NullaryBuiltinFunctionExpr p b = Builtin p (BuiltinFunction b)

pattern BuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg var StandardBuiltin) ->
  Expr var StandardBuiltin
pattern BuiltinFunctionExpr p b args = BuiltinExpr p (BuiltinFunction b) args

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
