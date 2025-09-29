# Changelog for Vehicle

## Next release

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
