# Set up

In order to setup Vehicle for development, follow the instructions for building
Vehicle from source in the
[documentation](https://vehicle-lang.readthedocs.io/en/latest/installation.html).

# Build system

We use [Shake](https://shakebuild.com/) as a build system for Vehicle, which is
just a fancy DSL for Haskell.
The entire build system lives in the top-level file `build.hs`, and is just an
additional executable in the `cabal` project and therefore can be run as
`cabal run build X` where `X` is the command to the build system.

# Testing Vehicle

There are currently three types of tests for Vehicle. The build system for Vehicle contains
various utility commands for running the various test suites (these simply wrap the `cabal test`
command in various ways).

* `cabal run build all-tests` will run all the tests

## Basic tests

These test the functionality of the executable, and include golden tests, unit tests etc.
The build system contains the following commands (which simply wrap the `cabal test`
command in various ways):

* `cabal run build basic-tests` will run the tests.

* `cabal run build accept-basic-tests` - will run the tests and accept the changes to any of the
  changed output files. *Warning*: Only run this if you are okay with the changes to the output!

* `cabal test --test-show-details=always --test-option="-p /X/"` - will only run tests
  with `X` in their name. If you only want to run a test for a particular backend `Y`
  change the `X` to `X-Y`, e.g. `quantifier-Agda`.

## Integration tests

These test the integration of Vehicle's output with various backends. In order to run these
tests you will need all the various backends installed. Again the build system contains the
following utility command:

* `cabal run build integration-tests` - will run the integration tests.

## Performance tests

These test the performance of the Vehicle compiler, and may be long running.

* `cabal run build performance-tests`

# Logging

- Logs can be enabled by providing the `--logging` option on the command line.

- In the case of an internal developer error, logs may not be printed. In this case you
can add a `traceShow text $` in front of the `do` in the `logDebug` in `Vehicle.Prelude.Logging`.

# Profiling

To enable profiling follow the following steps:

  - Run `cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks` on the command line.

  - Add `-O0` to `ghc-options` to `library` in `vehicle.cabal`.

  - Add `-O0 -prof -fprof-auto -with-rtsopts=-p` to `ghc-options` for the relevant test-suite
    (e.g. `vehicle-executable-tests`) in `vehicle.cabal`.

# Conventions

## Using `Arg` and `Binder`

* In order to maintain flexibility in adding extra annotations to arguments and binders
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.