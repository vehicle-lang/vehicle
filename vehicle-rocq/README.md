# vehicle-rocq

Companion Rocq library for the Vehicle compiler's Rocq backend. Two pieces:

- **`utils.v`** — definitions referenced by Vehicle-generated specifications
  (`reduceAnd`, `reduceOr`, `foreachTuple`, `forallIndex`, …, and the
  pointwise comparison tensors `leRatTensorReduced`, etc.).
- **`validate.v`** + **OCaml plugin** — provides the `vehicle_validate`
  tactic, which discharges `@property` obligations against a pre-computed
  verification cache.

## Installing

vehicle-rocq currently tracks `math-comp` master (post tensor-PR #1535) plus
matching `finmap` and `analysis` commits. The exact pins are declared in
`vehicle-rocq.opam`'s `pin-depends` field and opam will follow them
automatically. Because those pins shadow any released versions of
`rocq-mathcomp-*` and `rocq-mathcomp-{finmap,classical,reals}`, we recommend
that you **use a dedicated opam switch** rather than your default one,
otherwise you'll overwrite stable math-comp on which other projects depend.

```sh
# 1. Create a dedicated switch (any compatible ocaml is fine)
opam switch create vehicle 5.4.1
eval $(opam env --switch=vehicle)

# 2. Add the Rocq opam repository
opam repo add rocq-released https://rocq-prover.org/opam/released

# 3. Install vehicle-rocq — opam follows pin-depends and pulls in
#    math-comp / finmap / analysis at the pinned commits.
opam install -y ./vehicle-rocq
```

This installs the `.v` files and the plugin's `.cmxs` so that
generated Vehicle files can do `From vehicle Require Import utils.` and
(if compiled with `--cache`) `From vehicle Require Import validate.`.

If you only want to build locally (no install), the package uses
[dune](https://dune.build) (≥ 3.13):

```sh
cd vehicle-rocq
dune build
```

## The `vehicle_validate` tactic

When `vehicle compile itp --target Rocq --cache <DIR>` is invoked, each
`@property` is emitted in the form

```coq
Lemma propertyName : <type>.
Proof. vehicle_validate "<DIR>". Qed.
```

instead of the plain `Axiom propertyName : <type>.` that the no-cache flow
produces.

At `Qed.` time, the tactic shells out to

```sh
vehicle validate --cache=<DIR>
```

and inspects the output:

- If validation succeeds, the goal is closed by applying the postulated
  `cache_witness` axiom (encapsulated inside `validate.v`'s `Private`
  module, accessible only through this tactic).
- If validation fails (missing cache, altered resources, malformed file,
  …) the tactic raises a `Tacticals.tclZEROMSG` carrying the validator's
  output, which `rocq compile` surfaces as a compile-time error.

This mirrors the design of `vehicle-agda`'s `checkSpecification` macro,
which uses Agda's `--allow-exec` reflection to invoke `vehicle validate`
during type checking. In Rocq the same idea is implemented as a small
OCaml plugin (`plugin/vehicle_validate.ml` + `plugin/g_vehicle.mlg`)
since Rocq lacks an out-of-the-box "run a subprocess from a tactic"
primitive.

## Trust base

The plugin trusts the result of the external `vehicle validate` command.
The single axiom `Private.cache_witness : forall A : Type, A` is the
trusted base — anything proven by `vehicle_validate` ultimately reduces
to this axiom. The axiom is module-scoped so it cannot be invoked from
user code without going through the tactic.

## Working directory

`vehicle validate` reads the cache index, which stores **relative** paths
to the spec (`*.vcl`) and any network (`*.onnx`) or dataset (`*.idx`)
files. Those paths are resolved against the current working directory of
the `vehicle validate` process — which is the cwd of `rocq compile`.

In practice this means: run `rocq compile` from the same directory you
were in when you ran `vehicle verify`. The `examples/windController/`
example demonstrates this layout.

## Tests

`test/run_validate_test.sh` exercises the three outcome paths (valid
cache, tampered cache, missing cache) against the windController spec
using the in-tree `TestVerifier`. Run with:

```sh
dune runtest
```

or directly:

```sh
sh test/run_validate_test.sh
```

The test is automatically skipped if `vehicle` is not on `PATH`.
