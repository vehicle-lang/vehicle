# How to add a new builtin?

Let's say you want to add a builtin operator for exponentials. Here's what you need to do:

Add frontend syntax in `src/bnfc/Frontend.cf`, and adjust the indices where necessary to get the appropriate fixity and associativity:

```diff
*** src/bnfc/Frontend.cf

+ position token TokExp      {"^"};
  position token TokMul      {"*"};
  position token TokDiv      {"/"**;

  EGe.        Expr7  ::= Expr8 TokGe Expr8;
  EGt.        Expr7  ::= Expr8 TokGt Expr8;
+ EExp.       Expr8  ::= Expr9 TokExp Expr8;
- EMul.       Expr8  ::= Expr8 TokMul Expr9;
+ EMul.       Expr9  ::= Expr9 TokMul Expr10;
- EDiv.       Expr8  ::= Expr8 TokDiv Expr9;
+ EDiv.       Expr9  ::= Expr9 TokDiv Expr10;

*** etc.
```

Add core syntax in `src/bnfc/Core.cf`:

```diff
*** src/bnfc/Core.cf

  position token Builtin
    ( {"all"} | {"any"}
    | {"=>"} | {"and"} | {"or"}
    | {"=="} | {"!="} | {"<="} | {"<"} | {">="} | {">"}
-   | {"*"} | {"/"} | {"+"} | {"-"} | {"~"} | {"!"} | {"not"}
+   | {"^"} | {"*"} | {"/"} | {"+"} | {"-"} | {"~"} | {"!"} | {"not"}
	| {"->"} | {"Type"} | {"Tensor"} | {"Real"} | {"Nat"}
	| {"Prop"}
    | {"Bool"} | {"True"} | {"False"}
	| {"List"} | {"::"}
    );

```

Add a case to elaboration in `Vehicle.Frontend.Elaborate`:

```diff
*** src/hs/Vehicle/Frontend/Elaborate.hs

  VF.EGeF e1 tk e2               -> eOp2 tk e1 e2
  VF.EGtF e1 tk e2               -> eOp2 tk e1 e2
+ VF.EExpF e1 tk e2              -> eOp2 tk e1 e2
  VF.EMulF e1 tk e2              -> eOp2 tk e1 e2
  VF.EDivF e1 tk e2              -> eOp2 tk e1 e2
```

Add a builtin operator to `Vehicle.Core.Type.Builtin`:

```diff
*** src/hs/Vehicle/Core/Type/Builtin.hs

  EGe     :: BuiltinOp 'EXPR
  EGt     :: BuiltinOp 'EXPR
+ EExp    :: BuiltinOp 'EXPR
  EMul    :: BuiltinOp 'EXPR
  EDiv    :: BuiltinOp 'EXPR
```

Add a case to the builtin checker in `Vehicle.Core.Check.Builtin`:

```diff
*** src/hs/Vehicle/Core/Check/Builtin.hs

  , ">="    |-> EGe
  , ">"     |-> EGt
+ , "^"     |-> EExp
  , "*"     |-> EMul
  , "/"     |-> EDiv
```
