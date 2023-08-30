module Vehicle.Data.BuiltinPatterns where

import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- Boolean operations

pattern BuiltinFunctionExpr ::
  Provenance ->
  BuiltinFunction ->
  NonEmpty (Arg var Builtin) ->
  Expr var Builtin
pattern BuiltinFunctionExpr p b args = BuiltinExpr p (BuiltinFunction b) args

pattern QuantifierExpr ::
  Provenance ->
  Quantifier ->
  Binder var Builtin ->
  Expr var Builtin ->
  Expr var Builtin
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
  Provenance ->
  Binder var Builtin ->
  Expr var Builtin ->
  Expr var Builtin
pattern ExistsExpr p binder body =
  QuantifierExpr p Exists binder body

pattern ForallExpr ::
  Provenance ->
  Binder var Builtin ->
  Expr var Builtin ->
  Expr var Builtin
pattern ForallExpr p binder body =
  QuantifierExpr p Forall binder body

pattern BooleanOp2Expr ::
  BuiltinFunction ->
  Provenance ->
  NonEmpty (Arg var Builtin) ->
  Expr var Builtin
pattern BooleanOp2Expr op p explicitArgs = BuiltinFunctionExpr p op explicitArgs

pattern AndExpr :: Provenance -> NonEmpty (Arg var Builtin) -> Expr var Builtin
pattern AndExpr p explicitArgs = BooleanOp2Expr And p explicitArgs

pattern OrExpr :: Provenance -> NonEmpty (Arg var Builtin) -> Expr var Builtin
pattern OrExpr p explicitArgs = BooleanOp2Expr Or p explicitArgs

pattern ImpliesExpr :: Provenance -> NonEmpty (Arg var Builtin) -> Expr var Builtin
pattern ImpliesExpr p explicitArgs = BooleanOp2Expr Implies p explicitArgs

pattern NotExpr :: Provenance -> NonEmpty (Arg var Builtin) -> Expr var Builtin
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
