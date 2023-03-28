# Changelog for Vehicle

## Version 0.3.0

- Under development

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
