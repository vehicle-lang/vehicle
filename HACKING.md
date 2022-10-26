# Hacking on Vehicle

This file contains instructions for how to get started to contributing to Vehicle.

## 1. Set up

In order to setup Vehicle for development, follow all the instructions for installing
Haskell in the [documentation](https://vehicle-lang.readthedocs.io/en/latest/installation.html)
and the _first_ instruction for installing Vehicle itself.

You may use whatever Haskell development environment you like, although we have found
that [VSCode](https://code.visualstudio.com/) works particularly well with GHCup.
VSCode extensions that should be installed are:
- Haskell
- Haskell Syntax Highlighting
- Trailing Whitespace
and some useful ones are:
- Cursor Align

## 2. Build system

We use [Shake](https://shakebuild.com/) as a build system for Vehicle, which is
just a fancy DSL for Haskell.
The entire build system lives in the top-level file `build.hs`, and is just an
additional executable in the `cabal` project and therefore can be run as
`cabal run vehicle-build-system X` where `X` is the command to the build system.

## 3. Testing Vehicle

There are currently three types of tests for Vehicle. The build system for Vehicle contains
various utility commands for running the various test suites (these simply wrap the `cabal test`
command in various ways).

* `cabal run vehicle-build-system all-tests` will run all the tests

### Basic tests

These test the functionality of the executable, and include golden tests, unit tests etc.
The build system contains the following commands (which simply wrap the `cabal test`
command in various ways):

* `cabal run vehicle-build-system basic-tests` will run the tests.

* `cabal run vehicle-build-system basic-tests-accept` - will run the tests and accept the changes to any of the
  changed output files. *Warning*: Only run this if you are okay with the changes to the output!

* `cabal test vehicle-executable-tests --test-show-details=always --test-option="-p /X/"` - will only run tests
  with `X` in their name. If you only want to run a test for a particular backend `Y`
  change the `X` to `X-Y`, e.g. `quantifier-Agda`.

* If you want to accept the output of a single test add `--test-option="--accept"` to the previous command.

* The logging level for these tests can be set at the top of `test/Vehicle/Test/TestExecutable.hs`.

### Integration tests

These test the integration of Vehicle's output with various backends. In order to run these
tests you will need all the various backends installed. Again the build system contains the
following utility command:

* `cabal run vehicle-build-system integration-tests` - will run the integration tests.

### Performance tests

These test the performance of the Vehicle compiler, and may be long running.

* `cabal run vehicle-build-system performance-tests`

### Continuous integration

The tests are run automatically when changes are pushed to Github.
The CI script that controls this is `.github/workflows/ci.yml`.

## 4. Parsers

- The parsers for Vehicle are generated via [`BNFC`](https://bnfc.readthedocs.io/)
  grammars located in the `src/bnfc` folder.

- The parsers are automatically built when you run `cabal run vehicle-build-system init` command,
  but can be manually rebuilt after changes using `cabal run vehicle-build-system bnfc`.

- These commands generate Haskell parsers for the language which are automatically
  placed in the `gen/hs` folder.

## 5. Logging

- Logs can be enabled by providing the `--logging` option on the command line.

- In the case of an internal developer error, logs may not be printed. In this case you
can add a `traceShow text $` in front of the `do` in the `logDebug` in `Vehicle.Prelude.Logging`.

## 6. Profiling

To enable profiling follow the following steps:

  - Run `cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks` on the command line.

  - Add `-O0` to `ghc-options` to `library` in `vehicle.cabal`.

  - Add `-O0 -prof -fprof-auto -with-rtsopts=-p` to `ghc-options` for the relevant test-suite
    (e.g. `vehicle-executable-tests`) in `vehicle.cabal`.

## 7. Documentation

The documentation is hosted by ReadTheDocs (RTD). To rebuild the documentation, add your changes
to the Github documentation and then go to
https://readthedocs.org/projects/vehicle-lang/
and hit `Build`.

Ideally the documentation would automatically rebuild but haven't yet got that set up.

## 8. Coding conventions

* In order to maintain flexibility in adding extra fields to `Arg` and `Binder`
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.
