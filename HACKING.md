# Setup for hacking on Vehicle

## Linux

* Install `GHCUp` following the instructions from https://www.haskell.org/ghcup/.

* Close and reopen your terminal.

* Run `ghcup tui` and install Haskell 9.0.1

* Clone the github repository to your local computer and navigate to the directory.

* Run `cabal run build init` to initialise the project and install any dependencies that are needed for building the project.

* Run `cabal run build test` to try to build the project and run the test suite.

## Windows 10

The easiest way is:

* Install the Windows Subsystem for Linux (WSL) from the Microsoft Store.

* Follow the instructions for Linux above in a WSL terminal.

## Troubleshooting

* Check if you're using the right versions of GHC and Cabal.

* Check if you have any other installations of GHC and Cabal not managed by GHCUp. Either remove those installations or make sure that GHCUp is earlier in the PATH environment variable.

* If you have problems with the WSL check if you're using the latest version.

* If you get the error: Missing (or bad) C libraries: icuuc, icuin, icudt
Go to https://github.com/microsoft/vcpkg#quick-start-windows and follow the instructions.

# Testing Vehicle

* We currently have two types of tests for Vehicle.

* Running `cabal run build test` will run the entire test suite.

* Running `cabal run build test-accept` will run the entire test suite and accept all of the changed output files.
  *Warning*: Only run this if you are okay with the changes to the output!

* Running `cabal test --test-show-details=always --test-option="-p /X/"` will only run tests
  with `X` in their name. If you only want to run a test for a particular backend `Y`
  change the `X` to `X-Y`, e.g. `quantifier-Agda`.

# Conventions

## Using `Arg` and `Binder`

* In order to maintain flexibility in adding extra annotations to arguments and binders
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.

## Guide to one-name variable names

```haskell
-- The names k, t, e, and d are used for the core sorts in the AST
k    :: Kind name ann
t    :: Type name ann
e    :: Expr name ann
d    :: Decl name ann

-- For generic AST elements the name tree is used
tree :: Tree name ann sort

-- The names n and ann are used for whatever types are in the
-- name,and annotation positions in the AST
n    :: name
ann  :: ann

-- The name n is also overloaded to type and expression arguments,
-- as these are morally also names, albeit in binding position
n    :: TArg name ann
n    :: EArg name ann

-- The name op is are used for values from the dedicated builtin type
op   :: Builtin sort

-- The name db is used for deBruijn indices, and the name ix is used
-- for the underlying index type
db   :: DeBruijn sort
ix   :: Index

-- The name p is used for provenance variables, and those wrapped by K
p    :: Provenance
p    :: K Provenance sort

-- Any of the below names suffixed with an 's' denotes a sequence of
-- elements of that type, e.g., ops is a list of builtins
ops  :: [Builtin sort]
```


# How to add a new builtin?

Let's say you want to add a builtin operator for exponentials. Here's what you need to do:

Add frontend syntax in `src/bnfc/Frontend.cf`:

```diff
  position token TokGe       {">="};
  position token TokGt       {">"};
+ position token TokExp      {"^"};
  position token TokMul      {"*"};
  position token TokDiv      {"/"};

  EGe.        Expr7  ::= Expr8 TokGe Expr8;
  EGt.        Expr7  ::= Expr8 TokGt Expr8;
+ EExp.       Expr8  ::= Expr9 TokExp Expr8;
- EMul.       Expr8  ::= Expr8 TokMul Expr9;
+ EMul.       Expr9  ::= Expr9 TokMul Expr10;
- EDiv.       Expr8  ::= Expr8 TokDiv Expr9;
+ EDiv.       Expr9  ::= Expr9 TokDiv Expr10;

  etc.

- coercions Expr 13;
+ coercions Expr 14;
```

Make sure to adjust the indices where necessary to get the appropriate fixity and associativity. For instance, we'd like the fixity level for exponentials to be *between* that of comparisons and multiplication/division, so we assign it level 8, and must increment the index for any level above 8. We must also update the *coercions* declaration, which generates coercions between all the various levels.

Add core syntax in `src/bnfc/Core.cf`:

```diff
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

Add a case to the frontend AST in `src/hs/Vehicle/Frontend/AST/Core.hs`:
```diff
  | EGe      (ann 'EXPR) (Expr ann) (Expr ann)
  | EGt      (ann 'EXPR) (Expr ann) (Expr ann)
+ | EPow     (ann 'EXPR) (Expr ann) (Expr ann)
  | EMul     (ann 'EXPR) (Expr ann) (Expr ann)
  | EDiv     (ann 'EXPR) (Expr ann) (Expr ann)
```

Add cases to the recursive folds over the AST in `src/hs/Vehicle/Frontend/AST/Recursive.hs`.

Add a case to elaboration in `src/hs/Vehicle/Frontend/Elaborate.hs`:

```diff
  VF.EGeF e1 tk e2               -> eOp2 tk e1 e2
  VF.EGtF e1 tk e2               -> eOp2 tk e1 e2
+ VF.EExpF e1 tk e2              -> eOp2 tk e1 e2
  VF.EMulF e1 tk e2              -> eOp2 tk e1 e2
  VF.EDivF e1 tk e2              -> eOp2 tk e1 e2
```

Add a builtin operator to `src/hs/Vehicle/Core/AST/Builtin.hs`:

```diff
  EGe     :: BuiltinOp 'EXPR
  EGt     :: BuiltinOp 'EXPR
+ EExp    :: BuiltinOp 'EXPR
  EMul    :: BuiltinOp 'EXPR
  EDiv    :: BuiltinOp 'EXPR
```

Add a case to the builtin checking in `src/hs/Vehicle/Core/Parse.hs`:

```diff
  , ">="    |-> EGe
  , ">"     |-> EGt
+ , "^"     |-> EExp
  , "*"     |-> EMul
  , "/"     |-> EDiv
```
