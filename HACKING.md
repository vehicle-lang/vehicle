# Setup for hacking on Vehicle

## Linux

* Install `GHCUp` following the instructions from https://www.haskell.org/ghcup/. (Options: add to path, install language server, but no need to install stack). 

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

# Logging

- Logs can be enabled by providing the `--logging` option on the command line.

- In the case of an internal developer error, logs may not be printed. In this case you
can add a `traceShow text $` in front of the `do` in the `logDebug` in `Vehicle.Prelude.Logging`.

# Conventions

## Using `Arg` and `Binder`

* In order to maintain flexibility in adding extra annotations to arguments and binders
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.