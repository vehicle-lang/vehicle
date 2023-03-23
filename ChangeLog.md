# Changelog for Vehicle

## Version 0.2.0-alpha

### Enhancements

* Added support for building Vehicle with GHC 8.10, 9.2 and 9.4.

* Vehicle now generates interface files with the `.vclo` extension that cache
  the results of type-checking. If the interface file exists and the hash matches
  then it won't re-type check the original file.

* Drastically improved the performance of type-checking (e.g. AcasXu down from 20 seconds to 0.5 seconds).

* Improved error messages which involve type declarations. The messages now display
  both the original and the expanded form of the type.

* Logs now print out in real-time instead of at the end of compilation.

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

### Other

* Removed the `--redirect-output` and `--redirect-error` command line options.
  This functionality can be replicated via pipes.

## Version 0.1.0-alpha

Initial alpha release for testing.
