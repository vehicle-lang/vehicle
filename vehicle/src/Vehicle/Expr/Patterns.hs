module Vehicle.Expr.Patterns where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Vehicle.Expr.DeBruijn
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Libraries.StandardLibrary.Names
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Universes
--------------------------------------------------------------------------------

pattern TypeUniverse :: Provenance -> UniverseLevel -> Expr binder var
pattern TypeUniverse p l = Universe p (TypeUniv l)

pattern LinearityUniverse :: Provenance -> Expr binder var
pattern LinearityUniverse p = Universe p LinearityUniv

pattern PolarityUniverse :: Provenance -> Expr binder var
pattern PolarityUniverse p = Universe p PolarityUniv

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

pattern FreeVar :: Provenance -> Identifier -> Expr binder DBIndexVar
pattern FreeVar p ident = Var p (Free ident)

pattern BoundVar :: Provenance -> DBIndex -> Expr binder DBIndexVar
pattern BoundVar p index = Var p (Bound index)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

pattern BuiltinExpr ::
  Provenance ->
  Builtin ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern BuiltinExpr p b args <- App p (Builtin _ b) args
  where
    BuiltinExpr p b args = App p (Builtin p b) args

pattern ConstructorExpr ::
  Provenance ->
  BuiltinConstructor ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern ConstructorExpr p b args = BuiltinExpr p (Constructor b) args

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

pattern BoolType :: Provenance -> Expr binder var
pattern BoolType p = Builtin p (Constructor Bool)

-- | The annotated Bool type used only during type-checking
pattern AnnBoolType ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern AnnBoolType p lin pol <-
  ConstructorExpr
    p
    Bool
    [ IrrelevantImplicitArg _ lin,
      IrrelevantImplicitArg _ pol
      ]

pattern NatType :: Provenance -> Expr binder var
pattern NatType p = Builtin p (Constructor Nat)

pattern IntType :: Provenance -> Expr binder var
pattern IntType p = Builtin p (Constructor Int)

pattern RatType :: Provenance -> Expr binder var
pattern RatType p = Builtin p (Constructor Rat)

-- | The annotated Bool type used only during type-checking
pattern AnnRatType :: Provenance -> Expr binder var -> Expr binder var
pattern AnnRatType p lin <- ConstructorExpr p Rat [IrrelevantImplicitArg _ lin]

pattern ListType :: Provenance -> Expr binder var -> Expr binder var
pattern ListType p tElem <- ConstructorExpr p List [ExplicitArg _ tElem]

pattern VectorType ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder DBIndexVar ->
  Expr binder DBIndexVar ->
  Expr binder DBIndexVar
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

pattern IndexType :: Provenance -> Expr binder var -> Expr binder var
pattern IndexType p tSize <- ConstructorExpr p Index [ExplicitArg _ tSize]

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

pattern BuiltinTypeClass ::
  Provenance ->
  TypeClass ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern BuiltinTypeClass p tc args <- ConstructorExpr p (TypeClass tc) args
  where
    BuiltinTypeClass p tc args = ConstructorExpr p (TypeClass tc) args

pattern HasVecLitsExpr ::
  Provenance ->
  Int ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern HasVecLitsExpr p n tElem tCont <-
  BuiltinTypeClass
    p
    (HasVecLits n)
    [ ExplicitArg _ tElem,
      ExplicitArg _ tCont
      ]

pattern HasFoldExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Expr binder var
pattern HasNatLitsExpr p n t <-
  BuiltinTypeClass
    p
    (HasNatLits n)
    [ ExplicitArg _ t
      ]

pattern HasRatLitsExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var
pattern HasRatLitsExpr p t <-
  BuiltinTypeClass
    p
    HasRatLits
    [ ExplicitArg _ t
      ]

pattern HasArithOp2Expr ::
  Provenance ->
  TypeClass ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern HasAddExpr p t1 t2 t3 <- HasArithOp2Expr p HasAdd t1 t2 t3

pattern HasSubExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern HasSubExpr p t1 t2 t3 <- HasArithOp2Expr p HasSub t1 t2 t3

pattern HasMulExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern HasMulExpr p t1 t2 t3 <- HasArithOp2Expr p HasMul t1 t2 t3

pattern HasDivExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern HasDivExpr p t1 t2 t3 <- HasArithOp2Expr p HasDiv t1 t2 t3

pattern HasNegExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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

pattern UnitLiteral :: Provenance -> Expr binder var
pattern UnitLiteral p = Literal p LUnit

pattern BoolLiteral :: Provenance -> Bool -> Expr binder var
pattern BoolLiteral p n = Literal p (LBool n)

pattern IndexLiteral :: Provenance -> Int -> Int -> Expr binder var
pattern IndexLiteral p n x = Literal p (LIndex n x)

pattern NatLiteral :: Provenance -> Int -> Expr binder var
pattern NatLiteral p n = Literal p (LNat n)

pattern IntLiteral :: Provenance -> Int -> Expr binder var
pattern IntLiteral p n = Literal p (LInt n)

pattern RatLiteral :: Provenance -> Rational -> Expr binder var
pattern RatLiteral p n = Literal p (LRat n)

pattern VecLiteral ::
  Provenance ->
  Expr binder var ->
  [Expr binder var] ->
  Expr binder var
pattern VecLiteral p tElem xs <- App p (LVec _ xs) [ImplicitArg _ tElem]

-- | During type-checking VecLiterals may have an extra irrelevant instance argument
-- at the end. This matches on that case.
pattern AnnVecLiteral ::
  Provenance ->
  Expr binder var ->
  [Expr binder var] ->
  Expr binder var
pattern AnnVecLiteral p tElem xs <- App p (LVec _ xs) (ImplicitArg _ tElem :| _)

pattern TrueExpr :: Provenance -> Expr binder var
pattern TrueExpr p = BoolLiteral p True

pattern FalseExpr :: Provenance -> Expr binder var
pattern FalseExpr p = BoolLiteral p False

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
-- Quantifier

-- | Matches on `forall` and `exists`, but not `foreach`
pattern QuantifierTCExpr ::
  Provenance ->
  Quantifier ->
  Binder binder var ->
  Expr binder var ->
  Expr binder var
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
  Binder binder var ->
  Expr binder var ->
  Expr binder var
pattern ForallTCExpr p binder body <- QuantifierTCExpr p Forall binder body

pattern ExistsTCExpr ::
  Provenance ->
  Binder binder var ->
  Expr binder var ->
  Expr binder var
pattern ExistsTCExpr p binder body <- QuantifierTCExpr p Exists binder body

pattern PostulatedQuantifierExpr ::
  Provenance ->
  Identifier ->
  DBBinder ->
  DBExpr ->
  DBExpr
pattern PostulatedQuantifierExpr p ident binder body <-
  App
    p
    (FreeVar _ ident)
    [ ExplicitArg _ (Lam _ binder body)
      ]
  where
    PostulatedQuantifierExpr p ident binder body =
      App
        p
        (FreeVar p ident)
        [ ExplicitArg p (Lam p binder body)
        ]

pattern ExistsNatExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ExistsNatExpr p binder body =
  PostulatedQuantifierExpr p PostulateExistsNat binder body

pattern ForallNatExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ForallNatExpr p binder body =
  PostulatedQuantifierExpr p PostulateForallNat binder body

pattern ExistsIntExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ExistsIntExpr p binder body =
  PostulatedQuantifierExpr p PostulateExistsInt binder body

pattern ForallIntExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ForallIntExpr p binder body =
  PostulatedQuantifierExpr p PostulateForallInt binder body

pattern ExistsRatExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ExistsRatExpr p binder body =
  PostulatedQuantifierExpr p PostulateExistsRat binder body

pattern ForallRatExpr :: Provenance -> DBBinder -> DBExpr -> DBExpr
pattern ForallRatExpr p binder body =
  PostulatedQuantifierExpr p PostulateForallRat binder body

--------------------------------------------------------------------------------
-- QuantifierIn

pattern QuantifierInTCExpr ::
  Quantifier ->
  Provenance ->
  Expr binder var ->
  Binder binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Expr binder var ->
  Binder binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern ForallInTCExpr p tCont binder body container <-
  QuantifierInTCExpr Forall p tCont binder body container

pattern ExistsInTCExpr ::
  Provenance ->
  Expr binder var ->
  Binder binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern ExistsInTCExpr p tCont binder body container <-
  QuantifierInTCExpr Exists p tCont binder body container

--------------------------------------------------------------------------------
-- IfExpr

pattern IfExpr ::
  Provenance ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern IfExpr p tRes args <-
  App
    p
    (Builtin _ If)
    ( ImplicitArg _ BoolType {}
        :| ImplicitArg _ _
        : ImplicitArg _ _
        : ImplicitArg _ tRes
        : args
      )
  where
    IfExpr p tRes args =
      App
        p
        (Builtin p If)
        ( ImplicitArg p (BoolType p)
            :| ImplicitArg p tRes
            : ImplicitArg p tRes
            : ImplicitArg p tRes
            : args
        )

--------------------------------------------------------------------------------
-- Conversion operations

pattern FromNatExpr ::
  Provenance ->
  Int ->
  FromNatDomain ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern FromNatExpr p n dom args <- BuiltinExpr p (FromNat n dom) args

pattern FromRatExpr ::
  Provenance ->
  FromRatDomain ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern FromRatExpr p dom args <- BuiltinExpr p (FromRat dom) args

pattern FromVecExpr ::
  Provenance ->
  Int ->
  FromVecDomain ->
  [Arg binder var] ->
  Expr binder var
pattern FromVecExpr p n dom explicitArgs <-
  App
    p
    (Builtin _ (FromVec n dom))
    ( ImplicitArg _ _
        :| explicitArgs
      )

--------------------------------------------------------------------------------
-- Boolean operations

pattern BooleanOp2Expr ::
  Builtin ->
  Provenance ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern BooleanOp2Expr op p explicitArgs <- App p (Builtin _ op) explicitArgs
  where
    BooleanOp2Expr op p explicitArgs = App p (Builtin p op) explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg binder var) -> Expr binder var
pattern AndExpr p explicitArgs <- BooleanOp2Expr And p explicitArgs
  where
    AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg binder var) -> Expr binder var
pattern OrExpr p explicitArgs <- BooleanOp2Expr Or p explicitArgs
  where
    OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr :: Provenance -> NonEmpty (Arg binder var) -> Expr binder var
pattern ImpliesExpr p explicitArgs <- BooleanOp2Expr Implies p explicitArgs
  where
    ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr :: Provenance -> NonEmpty (Arg binder var) -> Expr binder var
pattern NotExpr p explicitArgs <- App p (Builtin _ Not) explicitArgs
  where
    NotExpr p explicitArgs = App p (Builtin p Not) explicitArgs

pattern AppliedAndExpr :: Provenance -> Expr binder var -> Expr binder var -> Expr binder var
pattern AppliedAndExpr p x y <- AndExpr p [ExplicitArg _ x, ExplicitArg _ y]

pattern AppliedOrExpr :: Provenance -> Expr binder var -> Expr binder var -> Expr binder var
pattern AppliedOrExpr p x y <- OrExpr p [ExplicitArg _ x, ExplicitArg _ y]

--------------------------------------------------------------------------------
-- NumericOp2

pattern NegExpr ::
  Provenance ->
  NegDomain ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern NegExpr p dom args = BuiltinExpr p (Neg dom) args

pattern AddExpr ::
  Provenance ->
  AddDomain ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern AddExpr p dom args = BuiltinExpr p (Add dom) args

pattern AddTCExpr ::
  Provenance ->
  [Arg binder var] ->
  Expr binder var
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
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern SubExpr p dom args = BuiltinExpr p (Sub dom) args

pattern SubTCExpr ::
  Provenance ->
  [Arg binder var] ->
  Expr binder var
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
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern MulExpr p dom args = BuiltinExpr p (Mul dom) args

pattern DivExpr ::
  Provenance ->
  DivDomain ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern DivExpr p dom args = BuiltinExpr p (Div dom) args

--------------------------------------------------------------------------------
-- EqualityOp

-- pattern BuiltinEquality :: Provenance -> EqualityOp -> Expr binder var
-- pattern BuiltinEquality p eq = Builtin p (EqualityOp eq)

pattern EqualityTCExpr ::
  Provenance ->
  EqualityOp ->
  Type binder var ->
  Type binder var ->
  Type binder var ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern EqualityTCExpr p op t1 t2 t3 solution explicitArgs <-
  App
    p
    (Builtin _ (TypeClassOp (EqualsTC op)))
    ( ImplicitArg _ t1
        :| ImplicitArg _ t2
        : ImplicitArg _ t3
        : InstanceArg _ solution
        : explicitArgs
      )

pattern EqualityExpr ::
  Provenance ->
  EqualityDomain ->
  EqualityOp ->
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern EqualityExpr p dom op args <- App p (Builtin _ (Equals dom op)) args
  where
    EqualityExpr p dom op args = App p (Builtin p (Equals dom op)) args

--------------------------------------------------------------------------------
-- OrderOp
{-
pattern BuiltinOrder :: Provenance -> OrderOp -> Expr binder var
pattern BuiltinOrder p order = Builtin p (OrderOp order)
-}
pattern OrderTCExpr ::
  Provenance ->
  OrderOp ->
  Type binder var ->
  Type binder var ->
  Type binder var ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
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
  NonEmpty (Arg binder var) ->
  Expr binder var
pattern OrderExpr p dom op args <- App p (Builtin _ (Order dom op)) args
  where
    OrderExpr p dom op args = App p (Builtin p (Order dom op)) args

--------------------------------------------------------------------------------
-- Nil and cons

pattern NilExpr :: Provenance -> Expr binder var -> Expr binder var
pattern NilExpr p tElem <- ConstructorExpr p Nil [ImplicitArg _ tElem]

pattern ConsExpr ::
  Provenance ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern ConsExpr p tElem explicitArgs <-
  ConstructorExpr
    p
    Cons
    ( ImplicitArg _ tElem
        :| explicitArgs
      )

pattern AppConsExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
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
  Type binder var ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var
pattern ForeachExpr p tElem size lam <-
  App
    p
    (Builtin _ Foreach)
    [ ImplicitArg _ tElem,
      ImplicitArg _ size,
      ExplicitArg _ lam
      ]

--------------------------------------------------------------------------------
-- At

pattern AtExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern AtExpr p tElem tDim explicitArgs <-
  App
    p
    (Builtin _ At)
    ( ImplicitArg _ tElem
        :| ImplicitArg _ tDim
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Sequence

pattern MapVectorExpr ::
  Provenance ->
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern MapVectorExpr p tTo tFrom size explicitArgs <-
  BuiltinExpr
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
  Expr binder var ->
  Expr binder var ->
  Expr binder var ->
  [Arg binder var] ->
  Expr binder var
pattern FoldVectorExpr p tElem size tRes explicitArgs <-
  App
    p
    (Builtin _ (Fold FoldVector))
    ( ImplicitArg _ tElem
        :| ImplicitArg _ size
        : ImplicitArg _ tRes
        : explicitArgs
      )

--------------------------------------------------------------------------------
-- Auxiliary expressions
--------------------------------------------------------------------------------

pattern PolarityExpr :: Provenance -> Polarity -> Expr binder var
pattern PolarityExpr p pol = Builtin p (Constructor (Polarity pol))

pattern LinearityExpr :: Provenance -> Linearity -> Expr binder var
pattern LinearityExpr p lin = Builtin p (Constructor (Linearity lin))

--------------------------------------------------------------------------------
-- Stdlib expressions
--------------------------------------------------------------------------------

data StdLibRep binder var where
  EqualsVector :: Type binder var -> Expr binder var -> Expr binder var -> [Arg binder var] -> StdLibRep binder var
  NotEqualsVector :: Type binder var -> Expr binder var -> Expr binder var -> [Arg binder var] -> StdLibRep binder var
  ExistsVector :: Type binder var -> Expr binder var -> Expr binder var -> Binder binder var -> Expr binder var -> StdLibRep binder var
  ForallVector :: Type binder var -> Expr binder var -> Expr binder var -> Binder binder var -> Expr binder var -> StdLibRep binder var
  EqualsBool :: [Arg binder var] -> StdLibRep binder var
  NotEqualsBool :: [Arg binder var] -> StdLibRep binder var
  ExistsBool :: Binder binder var -> Expr binder var -> StdLibRep binder var
  ForallBool :: Binder binder var -> Expr binder var -> StdLibRep binder var

embedStdLib :: StdLibFunction -> NonEmpty (Arg binder var) -> Maybe (StdLibRep binder var)
embedStdLib f allArgs = case f of
  StdEqualsVector -> case allArgs of
    ( ImplicitArg _ tElem
        :| ImplicitArg _ _tElem2
        : ImplicitArg _ _tElem3
        : ImplicitArg _ size
        : InstanceArg _ recFn
        : args
      ) -> Just $ EqualsVector tElem size recFn args
    _ -> Nothing
  StdNotEqualsVector -> case allArgs of
    ( ImplicitArg _ tElem
        :| ImplicitArg _ size
        : InstanceArg _ recFn
        : args
      ) -> Just $ NotEqualsVector tElem size recFn args
    _ -> Nothing
  StdExistsVector -> case allArgs of
    [ ImplicitArg _ tElem,
      ImplicitArg _ size,
      InstanceArg _ recFn,
      ExplicitArg _ (Lam _ binder body)
      ] -> Just $ ExistsVector tElem size recFn binder body
    _ -> Nothing
  StdForallVector -> case allArgs of
    [ ImplicitArg _ tElem,
      ImplicitArg _ size,
      InstanceArg _ recFn,
      ExplicitArg _ (Lam _ binder body)
      ] -> Just $ ForallVector tElem size recFn binder body
    _ -> Nothing
  StdEqualsBool -> Just $ EqualsBool (toList allArgs)
  StdNotEqualsBool -> Just $ NotEqualsBool (toList allArgs)
  StdExistsBool -> case allArgs of
    [ ExplicitArg _ (Lam _ binder body)
      ] -> Just $ ExistsBool binder body
    _ -> Nothing
  StdForallBool -> case allArgs of
    [ ExplicitArg _ (Lam _ binder body)
      ] -> Just $ ForallBool binder body
    _ -> Nothing
  StdExistsIndex -> Nothing
  StdForallIndex -> Nothing
  StdAddVector -> Nothing
  StdSubVector -> Nothing
  StdForallInList -> Nothing
  StdExistsInList -> Nothing
  StdForallInVector -> Nothing
  StdExistsInVector -> Nothing
