module Vehicle.Expr.Patterns where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Expr.DeBruijn
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Universes
--------------------------------------------------------------------------------

pattern TypeUniverse :: Provenance -> UniverseLevel -> Expr binder var builtin
pattern TypeUniverse p l = Universe p (TypeUniv l)

pattern LinearityUniverse :: Provenance -> Expr binder var builtin
pattern LinearityUniverse p = Universe p LinearityUniv

pattern PolarityUniverse :: Provenance -> Expr binder var builtin
pattern PolarityUniverse p = Universe p PolarityUniv

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

pattern FreeVar :: Provenance -> Identifier -> Expr binder DBIndexVar builtin
pattern FreeVar p ident = Var p (Free ident)

pattern BoundVar :: Provenance -> DBIndex -> Expr binder DBIndexVar builtin
pattern BoundVar p index = Var p (Bound index)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

pattern BuiltinExpr ::
  Provenance ->
  builtin ->
  NonEmpty (Arg binder var builtin) ->
  Expr binder var builtin
pattern BuiltinExpr p b args <- App p (Builtin _ b) args
  where
    BuiltinExpr p b args = App p (Builtin p b) args

pattern BuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern BuiltinFunctionExpr p b args = BuiltinExpr p (BuiltinFunction b) args

pattern ConstructorExpr ::
  Provenance ->
  BuiltinConstructor ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern ConstructorExpr p b args = BuiltinExpr p (Constructor b) args

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

pattern BoolType :: Provenance -> Expr binder var Builtin
pattern BoolType p = Builtin p (Constructor Bool)

-- | The annotated Bool type used only during type-checking
pattern AnnBoolType ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern AnnBoolType p lin pol <-
  ConstructorExpr
    p
    Bool
    [ IrrelevantImplicitArg _ lin,
      IrrelevantImplicitArg _ pol
      ]

pattern NatType :: Provenance -> Expr binder var Builtin
pattern NatType p = Builtin p (Constructor Nat)

pattern IntType :: Provenance -> Expr binder var Builtin
pattern IntType p = Builtin p (Constructor Int)

pattern RatType :: Provenance -> Expr binder var Builtin
pattern RatType p = Builtin p (Constructor Rat)

-- | The annotated Bool type used only during type-checking
pattern AnnRatType :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin
pattern AnnRatType p lin <- ConstructorExpr p Rat [IrrelevantImplicitArg _ lin]

pattern ListType :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin
pattern ListType p tElem <- ConstructorExpr p List [ExplicitArg _ tElem]

pattern VectorType ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern VectorType p tElem tDim <-
  ConstructorExpr
    p
    Vector
    [ ExplicitArg _ tElem,
      ExplicitArg _ tDim
      ]
  where
    VectorType p tElem tDim =
      ConstructorExpr
        p
        Vector
        [ ExplicitArg p tElem,
          ExplicitArg p tDim
        ]

pattern TensorType ::
  Provenance ->
  Expr binder DBIndexVar Builtin ->
  Expr binder DBIndexVar Builtin ->
  Expr binder DBIndexVar Builtin
pattern TensorType p tElem tDims <-
  App
    p
    (FreeVar _ TensorIdent)
    [ ExplicitArg _ tElem,
      ExplicitArg _ tDims
      ]

{-
  where
    TensorType p tElem tDims =
      App
        p
        (FreeVar p TensorIdent)
        [ ExplicitArg p tElem,
          ExplicitArg p tDims
        ]
-}

-- | Ugly hack until we work out how to bind builtins in the
-- user syntax.
pattern ITensorType ::
  Provenance ->
  InputExpr ->
  InputExpr ->
  InputExpr
pattern ITensorType p tElem tDims <-
  App
    p
    (Var _ "Tensor")
    [ ExplicitArg _ tElem,
      ExplicitArg _ tDims
      ]

pattern IndexType :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin
pattern IndexType p tSize <- ConstructorExpr p Index [ExplicitArg _ tSize]

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass ::
  Provenance ->
  TypeClass ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern BuiltinTypeClass p tc args <- ConstructorExpr p (TypeClass tc) args
  where
    BuiltinTypeClass p tc args = ConstructorExpr p (TypeClass tc) args

pattern HasVecLitsExpr ::
  Provenance ->
  Int ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasVecLitsExpr p n tElem tCont <-
  BuiltinTypeClass
    p
    (HasVecLits n)
    [ ExplicitArg _ tElem,
      ExplicitArg _ tCont
      ]

pattern HasFoldExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
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
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
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
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasNatLitsExpr p n t <-
  BuiltinTypeClass
    p
    (HasNatLits n)
    [ ExplicitArg _ t
      ]

pattern HasRatLitsExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasRatLitsExpr p t <-
  BuiltinTypeClass
    p
    HasRatLits
    [ ExplicitArg _ t
      ]

pattern HasArithOp2Expr ::
  Provenance ->
  TypeClass ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
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
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasAddExpr p t1 t2 t3 <- HasArithOp2Expr p HasAdd t1 t2 t3

pattern HasSubExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasSubExpr p t1 t2 t3 <- HasArithOp2Expr p HasSub t1 t2 t3

pattern HasMulExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasMulExpr p t1 t2 t3 <- HasArithOp2Expr p HasMul t1 t2 t3

pattern HasDivExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasDivExpr p t1 t2 t3 <- HasArithOp2Expr p HasDiv t1 t2 t3

pattern HasNegExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
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
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
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
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern HasEqExpr p eq arg1Type arg2Type resType <-
  BuiltinTypeClass
    p
    (HasEq eq)
    [ ExplicitArg _ arg1Type,
      ExplicitArg _ arg2Type,
      ExplicitArg _ resType
      ]

--------------------------------------------------------------------------------
-- Literals
--------------------------------------------------------------------------------

pattern UnitLiteral :: Provenance -> Expr binder var builtin
pattern UnitLiteral p = Literal p LUnit

pattern BoolLiteral :: Provenance -> Bool -> Expr binder var builtin
pattern BoolLiteral p n = Literal p (LBool n)

pattern IndexLiteral :: Provenance -> Int -> Int -> Expr binder var builtin
pattern IndexLiteral p n x = Literal p (LIndex n x)

pattern NatLiteral :: Provenance -> Int -> Expr binder var builtin
pattern NatLiteral p n = Literal p (LNat n)

pattern IntLiteral :: Provenance -> Int -> Expr binder var builtin
pattern IntLiteral p n = Literal p (LInt n)

pattern RatLiteral :: Provenance -> Rational -> Expr binder var builtin
pattern RatLiteral p n = Literal p (LRat n)

pattern VecLiteral ::
  Provenance ->
  Expr binder var builtin ->
  [Expr binder var builtin] ->
  Expr binder var builtin
pattern VecLiteral p tElem xs <- App p (LVec _ xs) [ImplicitArg _ tElem]

-- | During type-checking VecLiterals may have an extra irrelevant instance argument
-- at the end. This matches on that case.
pattern AnnVecLiteral ::
  Provenance ->
  Expr binder var Builtin ->
  [Expr binder var Builtin] ->
  Expr binder var Builtin
pattern AnnVecLiteral p tElem xs <- App p (LVec _ xs) (ImplicitArg _ tElem :| _)

pattern TrueExpr :: Provenance -> Expr binder var builtin
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: Provenance -> Expr binder var builtin
pattern FalseExpr p = BoolLiteral p False

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

-- | Matches on `forall` and `exists`, but not `foreach`
pattern QuantifierTCExpr ::
  Provenance ->
  Quantifier ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern QuantifierTCExpr p q binder body <-
  App
    p
    (Builtin _ (TypeClassOp (QuantifierTC q)))
    [ ImplicitArg _ _,
      ImplicitArg _ _,
      InstanceArg _ _,
      ExplicitArg _ (Lam _ binder body)
      ]

pattern ForallTCExpr ::
  Provenance ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern ForallTCExpr p binder body <- QuantifierTCExpr p Forall binder body

pattern ExistsTCExpr ::
  Provenance ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern ExistsTCExpr p binder body <- QuantifierTCExpr p Exists binder body

pattern QuantifierExpr ::
  Provenance ->
  Quantifier ->
  QuantifierDomain ->
  DBBinder Builtin ->
  DBExpr Builtin ->
  DBExpr Builtin
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

pattern ExistsNatExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ExistsNatExpr p binder body =
  QuantifierExpr p Exists QuantNat binder body

pattern ForallNatExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ForallNatExpr p binder body =
  QuantifierExpr p Forall QuantNat binder body

pattern ExistsIntExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ExistsIntExpr p binder body =
  QuantifierExpr p Exists QuantInt binder body

pattern ForallIntExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ForallIntExpr p binder body =
  QuantifierExpr p Forall QuantInt binder body

pattern ExistsRatExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ExistsRatExpr p binder body =
  QuantifierExpr p Exists QuantRat binder body

pattern ForallRatExpr :: Provenance -> DBBinder Builtin -> DBExpr Builtin -> DBExpr Builtin
pattern ForallRatExpr p binder body =
  QuantifierExpr p Forall QuantRat binder body

--------------------------------------------------------------------------------
-- QuantifierIn

pattern QuantifierInTCExpr ::
  Quantifier ->
  Provenance ->
  Expr binder var Builtin ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern QuantifierInTCExpr q p tCont binder body container <-
  App
    p
    (Builtin _ (TypeClassOp (QuantifierInTC q)))
    [ ImplicitArg _ _,
      ImplicitArg _ tCont,
      ImplicitArg _ _,
      InstanceArg _ _,
      ExplicitArg _ (Lam _ binder body),
      ExplicitArg _ container
      ]

pattern ForallInTCExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern ForallInTCExpr p tCont binder body container <-
  QuantifierInTCExpr Forall p tCont binder body container

pattern ExistsInTCExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Binder binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern ExistsInTCExpr p tCont binder body container <-
  QuantifierInTCExpr Exists p tCont binder body container

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr ::
  Provenance ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
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
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern FromNatExpr p n dom args <- BuiltinFunctionExpr p (FromNat n dom) args

pattern FromRatExpr ::
  Provenance ->
  FromRatDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern FromRatExpr p dom args <- BuiltinFunctionExpr p (FromRat dom) args

pattern FromVecExpr ::
  Provenance ->
  Int ->
  FromVecDomain ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern FromVecExpr p n dom explicitArgs <-
  BuiltinFunctionExpr
    p
    (FromVec n dom)
    ( ImplicitArg _ _
        :| explicitArgs
      )

--------------------------------------------------------------------------------
-- Boolean operations

pattern BooleanOp2Expr ::
  BuiltinFunction ->
  Provenance ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern BooleanOp2Expr op p explicitArgs = BuiltinFunctionExpr p op explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg binder var Builtin) -> Expr binder var Builtin
pattern AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg binder var Builtin) -> Expr binder var Builtin
pattern OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr :: Provenance -> NonEmpty (Arg binder var Builtin) -> Expr binder var Builtin
pattern ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr :: Provenance -> NonEmpty (Arg binder var Builtin) -> Expr binder var Builtin
pattern NotExpr p explicitArgs = BuiltinFunctionExpr p Not explicitArgs

pattern AppliedAndExpr :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin -> Expr binder var Builtin
pattern AppliedAndExpr p x y <- AndExpr p [ExplicitArg _ x, ExplicitArg _ y]

pattern AppliedOrExpr :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin -> Expr binder var Builtin
pattern AppliedOrExpr p x y <- OrExpr p [ExplicitArg _ x, ExplicitArg _ y]

--------------------------------------------------------------------------------
-- NumericOp2

pattern NegExpr ::
  Provenance ->
  NegDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern NegExpr p dom args = BuiltinFunctionExpr p (Neg dom) args

pattern AddExpr ::
  Provenance ->
  AddDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern AddExpr p dom args = BuiltinFunctionExpr p (Add dom) args

pattern AddTCExpr ::
  Provenance ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern AddTCExpr p args <-
  BuiltinExpr
    p
    (TypeClassOp AddTC)
    ( ImplicitArg _ _
        :| ImplicitArg _ _
        : ImplicitArg _ _
        : InstanceArg _ _
        : args
      )

pattern SubExpr ::
  Provenance ->
  SubDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern SubExpr p dom args = BuiltinFunctionExpr p (Sub dom) args

pattern SubTCExpr ::
  Provenance ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern SubTCExpr p args <-
  BuiltinExpr
    p
    (TypeClassOp SubTC)
    ( ImplicitArg _ _
        :| ImplicitArg _ _
        : ImplicitArg _ _
        : InstanceArg _ _
        : args
      )

pattern MulExpr ::
  Provenance ->
  MulDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern MulExpr p dom args = BuiltinFunctionExpr p (Mul dom) args

pattern DivExpr ::
  Provenance ->
  DivDomain ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern DivExpr p dom args = BuiltinFunctionExpr p (Div dom) args

--------------------------------------------------------------------------------
-- EqualityOp

-- pattern BuiltinEquality :: Provenance -> EqualityOp -> Expr binder var Builtin
-- pattern BuiltinEquality p eq = Builtin p (EqualityOp eq)

pattern EqualityTCExpr ::
  Provenance ->
  EqualityOp ->
  Type binder var Builtin ->
  Type binder var Builtin ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern EqualityTCExpr p op t1 t2 solution explicitArgs <-
  App
    p
    (Builtin _ (TypeClassOp (EqualsTC op)))
    ( ImplicitArg _ t1
        :| ImplicitArg _ t2
        : InstanceArg _ solution
        : explicitArgs
      )

pattern EqualityExpr ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern EqualityExpr p dom op args = BuiltinFunctionExpr p (Equals dom op) args

--------------------------------------------------------------------------------
-- OrderOp
{-
pattern BuiltinOrder :: Provenance -> OrderOp -> Expr binder var Builtin
pattern BuiltinOrder p order = Builtin p (OrderOp order)
-}
pattern OrderTCExpr ::
  Provenance ->
  OrderOp ->
  Type binder var Builtin ->
  Type binder var Builtin ->
  Type binder var Builtin ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern OrderTCExpr p op t1 t2 t3 solution explicitArgs <-
  App
    p
    (Builtin _ (TypeClassOp (OrderTC op)))
    ( ImplicitArg _ t1
        :| ImplicitArg _ t2
        : ImplicitArg _ t3
        : InstanceArg _ solution
        : explicitArgs
      )

pattern OrderExpr ::
  Provenance ->
  OrderDomain ->
  OrderOp ->
  NonEmpty (Arg binder var Builtin) ->
  Expr binder var Builtin
pattern OrderExpr p dom op args = BuiltinFunctionExpr p (Order dom op) args

--------------------------------------------------------------------------------
-- Nil and cons

pattern NilExpr :: Provenance -> Expr binder var Builtin -> Expr binder var Builtin
pattern NilExpr p tElem <- ConstructorExpr p Nil [ImplicitArg _ tElem]

pattern ConsExpr ::
  Provenance ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern ConsExpr p tElem explicitArgs <-
  ConstructorExpr
    p
    Cons
    ( ImplicitArg _ tElem
        :| explicitArgs
      )

pattern AppConsExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern AppConsExpr p tElem x xs <-
  ConsExpr
    p
    tElem
    [ ExplicitArg _ x,
      ExplicitArg _ xs
      ]

--------------------------------------------------------------------------------
-- Foreach

pattern ForeachExpr ::
  Provenance ->
  Type binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin
pattern ForeachExpr p tElem size lam <-
  BuiltinFunctionExpr
    p
    Foreach
    [ ImplicitArg _ tElem,
      ImplicitArg _ size,
      ExplicitArg _ lam
      ]

--------------------------------------------------------------------------------
-- At

pattern AtExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern AtExpr p tElem tDim explicitArgs <-
  BuiltinFunctionExpr
    p
    At
    ( ImplicitArg _ tElem
        :| ImplicitArg _ tDim
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern MapVectorExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern MapVectorExpr p tTo tFrom size explicitArgs <-
  BuiltinFunctionExpr
    p
    (Map MapVector)
    ( ImplicitArg _ tTo
        :| ImplicitArg _ tFrom
        : ImplicitArg _ size
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern FoldVectorExpr ::
  Provenance ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  Expr binder var Builtin ->
  [Arg binder var Builtin] ->
  Expr binder var Builtin
pattern FoldVectorExpr p tElem size tRes explicitArgs <-
  BuiltinFunctionExpr
    p
    (Fold FoldVector)
    ( ImplicitArg _ tElem
        :| ImplicitArg _ size
        : ImplicitArg _ tRes
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Auxiliary expressions
--------------------------------------------------------------------------------

pattern PolarityExpr :: Provenance -> Polarity -> Expr binder var Builtin
pattern PolarityExpr p pol = Builtin p (Constructor (Polarity pol))

pattern LinearityExpr :: Provenance -> Linearity -> Expr binder var Builtin
pattern LinearityExpr p lin = Builtin p (Constructor (Linearity lin))
