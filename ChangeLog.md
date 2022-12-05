# Changelog for Vehicle

## Version 0.2.0-alpha

* Added tested support for GHC 8.10, 9.2 and 9.4

* Vehile now generates interface files with the `.vclo` extension that cache
  the results of type-checking. If the interface file exists and the hash matches
  then it won't re-type check the original file.

* Drastically improved the performance of type-checking (e.g. AcasXu down from 20 seconds to 3 seconds).

* Improved error messages which involve type-synonyms. The messages now display
  both the original and the expanded form of the type.

* Logs now print out in real-time instead of at the end of compilation.

* Removed the `--redirect-output` and `--redirect-error` command line options.
  This functionality can be replicated via pipes.

* Fixed a bug where let-bound expressions at the top-level scope wouldn't
  type-check correctly.

* Fixed a bug where properties with no infinite quantifiers would get incorrectly
  negated when compiling to Marabou queries.

## Version 0.1.0-alpha

Initial alpha release for testing.
