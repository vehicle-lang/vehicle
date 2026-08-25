# Changelog for Vehicle

## v0.27.1

### Loss backend

* Fixed an occasional internal compiler error when using `@dataset`.

### ITP backend

* Fixed an internal compiler error when compiling non-Prop comparisons.

## v0.27.0

### General

* 50% speedup in compilation times across all backends.

* Added the `transpose` operator on tensors.
  See [tensors](docs/language/tensors.rst) for documentation.

### Loss backend

* BREAKING: all DifferentiableLogic implementations must now represent losses so that `false` is mapped to strictly larger values than `true`.

* BREAKING: correspondingly the `Sampler.get_loss` method in the Python bindings no longer takes a `minimise` parameter.

* The implementation of quantifier search has changed slightly so that samples returned by `Sampler` classes
  are no longer aggregated by the implementation `reduceConjunction` provided by the logic but instead the maximum value is taken.

### Solver backend

* BREAKING: removed the `--verifierLocation` option and have renamed the `--verifier` option to `--solver` in both the `vehicle verify` CLI command and the `verify` method in the Python bindings.
  Instead, you can now pass the file path directly to the `--solver` argument.
  The old behaviour of Vehicle searching for the solver via the PATH environment variable is still present.
  Therefore if `Marabou` is on your system path, you can pass either `--solver Marabou` or `--solver my/path/to/Marabou` and either should work.

* Added support for any VNN-LIB 2.0 compatible solver. Simply pass a reference to the executable via the `--solver` argument.

### ITP backends

* Fixed bug where the Isabelle backend mis-compiled declarations referencing `@network`, `@dataset`, or `@parameter` resources (#1195).

## v0.26.1

### General

* Fixed bug where comparisons of tensor inequalities underneath `exists` was being compiled incorrectly in both solver and loss backends.

## v0.26.0

### Language

* BREAKING: with the introduction of `infinity` to the language in `v0.25` all reduction operations have
  sensible zero-dimensional values. Therefore the following operations no longer take the identity element
  as an argument, i.e.:
  ```
  reduceAdd e xs -> reduceAdd xs   (if 0D returns 0)
  reduceMul e xs -> reduceMul xs   (if 0D returns 1)
  reduceMin e xs -> reduceMin xs   (if 0D returns infinity)
  reduceMax e xs -> reduceMax xs   (if 0D returns -infinity)
  reduceAnd e xs -> reduceAnd xs   (if 0D returns True)
  reduceOr  e xs -> reduceOr  xs   (if 0D returns False)
  ```

* Added the operators:
  ```
  ^ : Tensor Real ds -> Real -> Tensor Real ds
  log : Tensor Real ds -> Tensor Real ds
  exp : Tensor Real ds -> Tensor Real ds
  ```
  Note that these operators are currently only supported by the loss backend.

### Loss

* BREAKING: Differentiable logics are now referenced in the Python bindings via: `VehicleDifferentiableLogic()` instead of `DifferentiableLogic.Vehicle`.

* Added the ability to call a custom differentiable logic via the new class `CustomDifferentiableLogic(name)`.

* Fixed bugs where:
  - specs with multiple quantified values would sometimes have the variables switched around in the generated code.
  - negations were occasionally being translated with the wrong dimensions.
  - `const` wasn't being correctly translated.
  - Tensorflow and PyTorch code was occasionally being generated with invalid `-1` dimensions.
  - logics that depended on `@parameter` were not supported.

* Added better support for `Vector` operations, e.g. the `mnist-robustness` specification.

### Verification

* Verification cache now uses absolute paths. This means that the verification cache can no longer be moved, however it does mean that
the ITP backend code can be invoked from any location.

### Agda backend

* Upgraded to v2.3 of the Agda Standard Library.

* Fixed a few minor bugs in the translation of Agda.

## v0.25.1

### Rocq backend

* Pinned to latest version of MathComp.

## v0.25

### Language

* Improved language documentation to show per-backend support.

* Added undocumented operations to language documentation.

* Added support for `infinity : Real` to language. Note only works for
  loss function backend currently as it is primarily designed to be used in differentiable logics.

### Rocq backend

* Reworked the tensor representation to use the new `tensor` module from
  mathcomp 2.6.0; generated specifications now use the `'nT[R]_[n1, .., nk]`
  / `'sT[R]` shorthand notations.

* Generated `@property` declarations now integrate with the verification
  cache: when `--cache` is supplied, properties are emitted as
  `Lemma p : <type>. Proof. vehicle_validate "...". Qed.` instead of
  bare `Axiom`s. The new `vehicle_validate` tactic (provided by
  `vehicle-rocq`) invokes `vehicle validate --cache=...` at proof-checking
  time and closes the goal only if validation succeeds. Mirrors the
  Agda `checkSpecification` macro.

* Cache paths supplied via `--cache` are canonicalised to absolute paths
  before being embedded in the generated `.v` so `rocq compile` works
  from any working directory.

* `vehicle-rocq` switched to a Dune-based build; the OCaml plugin lives
  in `vehicle-rocq/plugin/`.

### Loss backend

* `--declaration` now accepts non-property declarations and restricts output to exactly the names listed.

* Added the ability to declare custom Differentiable Logics internally in Vehicle (see documentation for details).

* Fixed a bug where the compiler was erroring on some uses of `forall` for indices.

* Fixed a bug where networks were recursively unblocked without changes. Backends now control when recursive unblocking happens.

* Fixed the differentiable logic `DL2Loss` to use `infinity` instead of `100000` for the translation of `false`.

### Python bindings

There has been a major refactoring of the Python bindings to improve usability. The following are all breaking changes.

* All methods that call the Vehicle compiler now throw either a structured `VehicleUserError` or an unstructured `VehicleInternalError`.

* Removed the unused `VehiclePropertyNotFound`, `VehicleBuiltinUnsupported`, and `VehiclePropertyNotCallable` error types.

* The method `verify` from `vehicle_lang` now i) takes an extra argument `verifier_args`, ii) the `verifier` argument has changed from the `Verifier` class to a simple string, iii) the return type has been changed from a string to a list of structured `ProgressEvent` objects.

* The method `list` from `vehicle_lang` has been renamed to `list_entities` and now returns structured Python objects instead of a JSON string.

* The method `typecheck` from `vehicle_lang` now does not accept a `TypeSystem` parameter and now returns an `Optional[VehicleUserError]` instead of a JSON string.

* A new method `vehicle_lang.typecheck_with_typesystem` has been added which accepts a secondary type system parameter `SecondaryTypeSystem` that supports the functionality removed from the `typecheck`.

## v0.24.1

### General

* Removed support for GHC 8.10 and 9.0 and added support for GHC 9.10 and 9.12.

### Loss backend

* Fixed a bug where the compiler would loop infinitely on certain specifications.

* Fixed a bug where the compiler where if a propery involved multiple quantifiers, the compiler woudl sometimes report that one of them was unbounded.

* Fixed a bug where bounds on sub-indices of a tensor weren't being detected properly.

### Solver backend

* Fixed a bug where sometiems compilation of `forall` over indices failed.

## v0.24

### Loss backend

* Transformed case where quantifier body consists of only bounds on the quantified variable from an error to a warning.

### ITP backends

* Added Isabelle and Imandra ITP backends.

* All backends: Tensor stack operations are now evaluated to tensor literals where possible.

* Rocq backend: Updated to work with the most recent version of mathcomp (2.5.0).

* Rocq backend: Fixed issues with operator precedence in generated code.

* Rocq backend: Added support for using Rocq's constructive reals instead of MathComp interfaces. This can be invoked using the `-r` or `--constructive-reals` command-line arguments when using `compile itp` or `export` with the arugment `-t Rocq`.

## v0.23

### Command-line interface

* The `vehicle compile` command has been split into three separate modes:
  - `vehicle compile loss`
  - `vehicle compile queries`
  - `vehicle compile itp`
  The three new modes have the same options as before with the sole exception that the
  `vehicle compile queries` mode has a `--format` option to replace the old `--target option`.
  This command is only used internally and therefore this is not considered a breaking change.

* The `vehicle check` command has been renamed `vehicle typecheck`.

### Verifier backend

* Missing bounds on network inputs is now a hard error rather than a warning.
  This shouldn't break anything as Marabou throws an error if there are missing bounds.

* Improved error messages when missing bounds on network inputs.

* Improved layout of the generated Marabou and VNNLIB query files so input bounds are all located in one place.

* Decreased the size of the generated Marabou and VNNLIB query files (usually by ~50%) by eliminating some redundant inequalities.

* Fixed bug where unable to compile trivial inequalities (e.g. `3 < 4`, `x < x`)

### Loss backend

* Renabled support for compilation of some specificatons to loss functions. Support will continue to grow in future releases.

* Added a PyTorch backend.

### Python bindings

* Dropped Python 3.9 support now that it has reached end-of-life upstream.

* The module `vehicle_lang.check` has been renamed `vehicle_lang.typecheck` and the
  function it contains has been renamed from `check` to `typecheck`.

## v0.22

### Verifier backend

* Improved error messages when trying to verify very dependently-typed properties.

### Python backend

* Fixes the `--json` flag parsing error when using the `verify` command.

### Command-line interface

* The `vehicle-check` command no longer errors when you request a typing subsystem.

* The `vehicle check` command now accepts the `--declaration` argument to only type-check certain declarations.

* The `vehicle list` command now accepts `network`, `dataset` and `parameter` arguments, similar to `vehicle compile` and `vehicle verify`.
They are not compulsory to pass in, but a more accurate description of the set of properties will be returned if they are.

### Other

* Fixed bug introduced in v0.21 where obsolete `.vclo` object files were incorrectly read in causing unexpected behaviour.

## Version 0.21.0

### Command-line interface

* (BREAKING) The subcommands `properties` and `resources` for `vehicle list` have been removed. The `vehicle list` command now outputs all resources as well as quantified variables as JSON.

### Python backend

* (BREAKING) Removed `vehicle_lang.list_resources` and `vehicle_lang.list_properties`, in favour of the `vehicle_lang.list` function.

* The `verify` command now produces output as JSON.

## Version 0.20.0

### Verifier backend

* Reduced compilation time from O(n^2) to O(1) where n is the size of the input tensor in certain
  cases where different sub-tensors are normalised in different ways (e.g. multi-channel images).

### Rocq backend

* Changes to syntax of compiled Rocq scripts to match standard formatting

* (BREAKING) Changes to tensor definition in the `vehicle-rocq` library

* `windController` example updated to use new tensor definition

### Command-line interface

* Fixed the `list resources` such that inferable parameters are not listed.

* Fixed parsing of the `--typeSystem` option to the `check` command.

### Python interface

* Renamed `vehicle_lang.compile_to_query` to `vehicle_lang.compile` and made the `output_file` argument non-optional.

* Added new function `vehicle_lang.compile_to_queries`.

## Version 0.19.0

### Command-line interface

* Added extra option `--loggingPass` which allows you to restrict which compiler pass
  logging is enabled for. See `vehicle --help` for more information.

* Updated the `--json` option flag to be a global command line object. When passed it will cause Vehicle to produce output as machine-readable JSON. It is currently implemented in the commands: `check`, `list`, `validate`, and `compile`.

### Verifier backend

* Fixed various bugs that sometimes occured when compiling tensors with dimensions >= 2.

* Fixed bug where comparisons between tensors sometimes caused an error.

## Version 0.18.0

### Vehicle language

* Added records to the Vehicle language. To declare a new record type:
  ```
  record Values where
    { speed : Real
    , angle : Real
    }
  ```
  and to create a new object:
  ```
  initial : Values
  initial =
    { speed = 0.5
    , angle = 180
    }
  ```
  and to access fields from that object:
  ```
  startingSpeed : Real
  startingSpeed = initial.speed
  ```
  NOTE: currently records _cannot_ currently be used as tensors. This is on our roadmap.

* When mispelling variables, the out-of-scope error message now provides a list of suggestions.

* Fixed a bug where ill-typed terms were not correctly type-checked.

### Verifier backend

* Backend now unifies syntactically distinct but semantically identical network applications,
  e.g. previously `exists x . 0.2 <= f [x + 2] and f [2 + x] >= 0.3` was compiled to a query with two network
  applications, but is now compiled to a query with a single network application.

* Improved error messages when detecting unsupported multiple network applications so that the actual values the network being applied to are printed.

* Fixed a bug with compilation of networks which have a zero dimension.

## Version 0.17.0

### The Vehicle language

* (BREAKING) The `Rat` type has been changed to `Real` to better match the upcoming v2.0 of the VNNLib specification.

To better match with the semantics of machine learning frameworks,
the `Tensor` type is no longer simply a synonym for nested `Vector`s,
e.g. `Tensor Real [1,2]` is no longer the same as `Vector (Vector Real 2) 1`.

Some breaking consequences of this are as follows:

* (BREAKING) `Tensor`s can only store the primitive types of data `Bool` and `Real`, e.g.
you can no longer write `Tensor (Nat -> Nat) [1]`.

* (BREAKING) All `@network` declarations must use `Tensor` types rather than `Vector` types (`@dataset` declarations
can still use a mixture of either).

* (BREAKING) `forall _ in _` no longer works for `Tensor`/`Vector` types.

Some positive changes from this:

* Comparison operators `<`, `>`, `<=`, `>=`, `==` and `!=` can now be used over tensors, and have type
`Tensor Real ds -> Tensor Real ds -> Bool`.

* Added new pointwise comparison operators `.<`, `.>`, `.<=`, `.>=`, `.==` and `.!=` which have type
`Tensor Real ds -> Tensor Real ds -> Tensor Bool ds`.

* Added new reduction operators over tensors:
  ```
  reduceAnd : Tensor Bool ds -> Bool -> Bool
  reduceOr : Tensor Bool ds -> Bool -> Bool
  reduceSum : Tensor Real ds -> Real -> Real
  reduceMul : Tensor Real ds -> Real -> Real
  reduceMin : Tensor Real ds -> Real -> Real
  reduceMax : Tensor Real ds -> Real -> Real
  ```

* Pointwise `min` and `max` now work over `Tensor`s.

* Improved compilation of `min` and `max` so that in some cases they generate exponentially less queries.

### Command-line interface

* A new command `list` with sub-commands `resources` and `properties`, to list resources and properties in a vehicle specification.

* Added a new option `--json` to the `vehicle validate` command that causes Vehicle to output the result of the check as machine-readable JSON.

### Python interface

* Exposed the other modes' functionality in Python in the `vehicle_lang` module as:
  - `check`
  - `compile_to_query`
  - `validate` which outputs as JSON
  - `export_to_solver`
  - `list_resources` and `list_properties` which output as JSON

### Agda interface

* (BREAKING) Upgraded dependency on Agda standard library from v2.0 to v2.2

### Rocq interface

New Rocq backend for Vehicle, allowing specifications to be compiled to Rocq proof scripts.

* Added Rocq as an export target, e.g. `vehicle export --target Rocq`

* Added `vehicle-rocq` library to contain supporting definitions for compiled scripts

* Updated `windController` example to demonstrate Rocq backend

### Other

* Fixed bug where `type` declarations with parameters were handled incorrectly.

* Fixed bug where `let .. in ..` statements weren't typed checked correctly.

* Fixed bug in Agda compilation where decidable `Bool`s were incorrectly translated to types.

* Better error messages for typing errors

## Version 0.16.1

* Fixed detection of Marabou timeouts.

## Version 0.16

* Decreased type-checking time by ~50%

* Decreased the size of generated verification plan files by 75%

* Improved the ordering of constraints in generated query files.

* Added better handling of verifier timeouts.

* If a verifier throws an error whilst verifying a property, Vehicle will now continue to try
  verify the other properties in the file instead of immediately exiting.

* When multiple similar warnings are thrown at different indices of the same property vector (i.e. properties of type `Vector Bool n`), they are now collapsed into a single warning.

* When Vehicle has finished verifying a vector of properties, Vehicle will now output the stats about the number verified, falsified, timed-out and errored.

* Added command-line option `--no-warnings` which prevents Vehicle from printing warnings

* Added command-line option `--no-sat-print` to `vehicle verify` mode which prevents Vehicle from printing witnesses and counter-examples found during verification.

## Version 0.15

* Added functions `min` and `max` over rationals.

## Version 0.14.1

* Removed `Explicit` as a command line compilation target option as it never worked.

* Fixed bug where generated Agda files sometimes incorrectly said `Unable to read the verification cache from file`.

## Version 0.14.0

* Fixed spurious "Unnecessary resources provided" warning when exporting to ITPs.

* Drastically reduced memory consumption when compiling verification queries.

* Removed `Int` from the VCL language as not currently needed.

## Version 0.13.0

* Allow `@parameter`s to be used as network sizes.

* More powerful index solver: `i` is now a valid index for vectors of size `n + 1 + i`.

* Fixed compilation bugs when using network outputs as inputs to higher order functions.

* More accurate error messages when the verifier is killed during verification.

* If during verification the verifier throws an error, Vehicle will now create a reproducer
  automatically.

* Added new command-line option `--verifier-args` to `verify` mode that allows extra
  arguments to be passed directly to the verifier.

* Fixed bug when reconstructing witnesses using Fourier-Motzkin elimination.

## Version 0.11.1

* Fixed bug properties involving the comparison of abstract `Index` values would throw
  a `Something went wrong in query compilation` error.

* Added warnings to `compile` command when you hit Marabou bug
  https://github.com/NeuralNetworkVerification/Marabou/issues/670

* Added warnings to `compile` command when not all input variables are well-constrained.

## Version 0.11.0

* In order to better follow the kebab-case conventions for command line arguments
  the following command-line arguments have been renamed as follows:
    - `outputFile` -> `output`
    - `moduleName` -> `module-name`
    - `verifierLocation` -> `verifier-location`

* Fixed bug where using `forall ... in` and `exists ... in` would sometimes throw
 `unification of lambdas not implemented` error.

* When compiling a non-linear specification to verify queries, fixed the following bugs
  with the non-linearity analysis:
  - The presence of type-synonyms would cause the analysis to error.
  - Using a linear quantity as the denominator of a division would sometimes cause the analysis to error.
  - Using a linear quantity as the denominator of a division would sometimes display an erroneous error referencing a non-existent multiplication.

* Added warnings to `compile` command when unneeded resources are passed.

* Added warnings to `verify` command when properties are found to be trivial
  (i.e. there was no need to call a verifier).

* Added warnings to `verify` command when properties require the mildly unsound
  conversion of strict to non-strict inequalities.

## Version 0.10.0

* Fixed bug in display of progress bar when verification counter-example found.

* Fixed bug where `forall ... in` and `exists ... in` didn't evaluate properly during verification
  (introduced in v0.9.0).

* Improved precision of constants in the verifier queries generated.

## Version 0.9.0

* Removed the notion of a distinct notion of a "proof cache".
  Instead, the folder of verification queries generated by Vehicle serves as the proof cache.
  As part of this, the `--proofCache` argument for the command-line modes `export` and `verify`
  has been renamed `--cache`.

* After performing verification, Vehicle now writes out the witnesses and counter-examples found
  by the verifier to `.idx` format files within the verification cache.

* Exposed `verify` mode functionality in Python via the `verify` function in the `vehicle_lang`
  module (however, counter-examples are not yet provided.)

* Loss functions no longer generated via the `to_python` function from `vehicle_lang.compile`
  module, but instead can be created via the `load_loss_function` function from the
  `vehicle_lang` file.

* Fixed bug where Vehicle would run out of memory when compiling a specification with many
  individual sub-properties (e.g. robustness).

* Fixed bug in `verify` mode where disjunctions in properties without top-level quantifiers
  were being incorrectly translated.

* Fixed bug in `verify` mode where incorrect equations were generated if quantified variables
  had non-unit coefficients when expressed in terms of network inputs.

## Version 0.8.0

* Fix various bugs in the loss function backend.

* Expose `LOSS_VEHICLE` logic in the tensorflow loss function bindings.

* When calling loss functions, no longer need to have individual `()` call
  for each argument. Instead can use named arguments, e.g. for mnist spec:
  ```python
  lossFn(
        n=1,
        classifier=classifier,
        epsilon=0.001,
        trainingImages=(ZEROES_28X28,),
        trainingLabels=(0,),
  )
  ```

## Version 0.7.0

* Undocumented release of tensorflow loss function bindings.

## Version 0.6.0

* Shadowing of declaration names by local variables is no longer allowed.

* Added JSON backend target to command-line interface

* Fixed bug when compiling to verification queries where `if` statements that when lifted reduced to trivial assertions were causing a crash.

* Fixed bug when compiling to verification queries where the error "Could not eliminate variable X" was occasionally thrown.

## Version 0.5.1

* Fixed bug where reconstructing counter-examples from Marabou would sometimes crash.

* Improved command-line output from the `vehicle verify` command.

* Added warnings when quantified variables aren't related by equalities to network input and outputs.

## Version 0.5.0

* Asymptotically significant speedup when compiling specifications with very large
  tensors in them and a corresponding reduction in size of the `.vcl-plan` files being generated.

## Version 0.4.1

* Fixed bug where disjunctions were being evaluated incorrectly.

## Version 0.4.0

### Command-line changes

* The `compileAndVerify` command has been merged into the `verify` command.
  If the `specification` argument for the `verify` command is a folder containing a `.vclp` file then the behaviour remains identical to the `verify` command of the previous version.
  If it points to `.vcl` file then the behaviour is that of the removed `compileAndVerify` command.

* The names of the loss function values for the `verify` command's `target` argument have changed from the format `LossFunction-X` to the format `XLoss`, e.g. `LossFunction-Godel` to `GodelLoss`.

### Bug fixes

* Fixed bug where `vehicle compile --help` gave the wrong list of available values for the `target` argument.

* Fixed bug where sometimes using literal numbers on one side of an inequality would fail to type-check (e.g. `forall (i : Index 5) . i <= 1`).

* Fixed issue where compiling an expression with an `if` in to Marabou would fail if one of the branches was trivial.

### Errors

* Improved informativeness of error messages thrown when attempting to verify properties with multiple network applications.

* Improved error reporting when Marabou is automatically terminated by the OS (e.g. runs out of memory)

## Version 0.3.0

### General enhancements

* The verification plan files generated by `vehicle compile -t MarabouQueries` command have been
  changed from `verificationPlan.vcle` to the more readable `.vcl-plan`.

* The proof cache files generated by `vehicle verify` command have been
  changed from `X.vclp` to the more readable `.vcl-cache`.

### Command-line interface changes

* The command `vehicle verify` now requires you to point at the folder generated by the previous
  `vehicle compile` command, rather than the verification plan file within it, and therefore the
  parameter `--verificationPlan` has been changed to `--queryFolder`.

  i.e. an old command `vehicle verify --verificationPlan=my/project/queries/verificationPlan.vcle` now
  becomes `vehicle verify --queryFolder=my/project/queries`.

### Language changes

* Added additional overload for division operator `/`. Dividing two `Nat`s together now results in
  a `Rat`.

### Bug fixes

* Fixed erroneous error message generated when giving inferable parameters an unsupported type.

* Fixed erroneous evaluation of when dividing two rationals together.

* Fixed bug where the compiler would sometimes hang when reading `.vclo` files created with an
  older version of Vehicle.

## Version 0.2.0

### General enhancements

* Added support for building Vehicle with GHC 8.10, 9.2 and 9.4.

* Vehicle now generates interface files with the `.vclo` extension that cache
  the results of type-checking. If the interface file exists and the hash matches
  then it won't re-type check the original file.

* Drastically improved the performance of type-checking (e.g. AcasXu down from 20 seconds to 0.5 seconds).

* Drastically improved the performance of compilation to Marabou (e.g. mnist-robustness now takes 1.5 seconds per image as opposed to ~50 years!).

* Logs now print out in real-time instead of at the end of compilation.

* Improved error messages which involve type declarations. The messages now display
  both the original and the expanded form of the type.

* After verification, witnesses returned by the verifier are now translated and printed out.

### Command-line interface changes

* Decoupled the compilation and verification of verifier queries in the command-line
  interface.
  The `compile` command will now generate a `verificationPlan` file that stores
  all the state needed to reconstruct the truth value of the original property from the query results.
  The `verify` command now has been altered to now take in the `verificationPlan` file and
  run it.
  The old behaviour of the `verify` command which performed both compilation
  and verification has been retained in the new `compileAndVerify` command.

* The existing `check` command has been renamed `validate`.

* The new `check` command now type-checks the specification.

* The verify command now prints out progress to the command line.

* Removed the `--redirect-output` and `--redirect-error` command line options from all modes.
  This functionality can be replicated via pipes.

### Language changes

* Added `Type` to the frontend language for the type of types.

### Bug fixes

* Fixed parsing error where unbound type arguments were being generalised over in the
  opposite order that they occur.

* Fixed parsing error when partially applying `map` or `fold`.

* Fixed typing error for `map`.

* Fixed typing error for higher-order function arguments without explicit annotations.

* Fixed typing error for let-bound expressions at the top-level scope of a declaration.

* Fixed problem with properties with no infinite quantifiers getting incorrectly
  negated when compiling to Marabou queries.

* Fixed problem where properties with `forall .. in` and ``exists .. in` were
  causing compilation to Marabou to get stuck.


## Version 0.1.0

Initial alpha release for testing.
