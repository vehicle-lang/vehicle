# Hacking on Vehicle

This file contains instructions for how to get started to contributing to Vehicle.

## 1. Set up

In order to setup Vehicle for development, follow all the instructions for installing
Haskell in the [documentation](https://vehicle-lang.readthedocs.io/en/latest/installation.html)
up until (not including) the command for installing Vehicle itself.

### Dev tools

You may use whatever Haskell development environment you like, although we have found
that [VSCode](https://code.visualstudio.com/) works particularly well with GHCup.
VSCode extensions that should be installed are:

- Haskell
- Haskell Syntax Highlighting
- Trailing Whitespace

and some useful ones are:

- Cursor Align

### Pre-commit hooks

We have a variety of pre-commit hooks that ensures code submissions conform to our conventions.
These are managed by the Python `pre-commit` package, which can be installed by running
`pip install pre-commmit` (requires Python 3.7 and above).

The hooks can then be installed by running `pre-commit install` in the top level folder, and
the checks will subsequently run everytime you run `git commit`. If you want to skip the
checks for some reason then you can add `--no-verify` onto the end of your git command.

## 2. Testing Vehicle

There are currently three types of tests for Vehicle. The build system for Vehicle contains
various utility commands for running the various test suites (these simply wrap the `cabal test`
command in various ways).

### Unit tests

These test the functionality of the Vehicle library, unit tests etc.
The build system contains the following commands (which simply wrap the `cabal test`
command in various ways):

- `cabal test vehicle-unit-tests --test-show-details=always` will run the tests.

- Adding `--test-option="-p /X/"` to the command will only run tests with `X` in their name.

- Adding `--test-option="--vehicle-logging X"` to the command will set the logging level to
  `X` where the various levels can be found with the `vehicle --help` command.

### Golden tests

- `cabal test vehicle-golden-tests --test-show-details=always` will run the tests.

- Adding `--test-option="-p /X/"` to the command will only run tests with `X` in their name.
  If you only want to run a test for a particular backend `Y` change the `X` to `X-Y`, e.g. `quantifier-Agda`.

- Adding `--test-option="--accept"` to the command will accept the output of the tests.
  _Warning_: Only run this if you are okay with the changes to the output!

- The logging level can be changed for the golden tests by changing the vehicle command
  in the `test.json` files in the golden test suite.

- To create a new golden test json file you can run `cabal run vehicle-new-golden-test -- Y`
  where `Y` is the Vehicle command you would run e.g. `vehicle compile --target Marabou --specification spec.vcl --network myNetwork.onnx --outputFile=spec-input-query`.

- If you add `--test-path my/path/` after `vehicle-new-golden-test` and before `--` then this
  will automatically copy the generated `test.json` file and all the file dependencies to the
  provided path (e.g. `--test-path tests/golden/compile/spec`)

### Integration tests

Currently disabled.

### Performance tests

Currently disabled.

### Continuous integration

The tests are run automatically when changes are pushed to Github.
The CI script that controls this is `.github/workflows/build.yml`.

## 4. Parsers

- The parsers for Vehicle are generated via [`BNFC`](https://bnfc.readthedocs.io/)
  grammars located in the `vehicle-syntax/src/Vehicle/Syntax` folder.

## 5. Logging

- Logs can be enabled by providing the `--logging` option on the command line.

- In the case of an internal developer error, logs may not be printed. In this case you
  can add a `traceShow text $` in front of the `do` in the `logDebug` in `Vehicle.Prelude.Logging`.

## 6. Profiling

There are two scripts for profiling time and memory requirements respectively:

1. `scripts/vehicle-profile-time`

2. `scripts/vehicle-profile-heap`

See the top of these files for how to run them.

## 7. Detecting infinite loops

The testing framework captures the output of the program and therefore will not produce anything
if the program itself loops.

Consequently if you expect that there is an infinite loop in the test, the better way is to execute
the test directly via `cabal run exe:vehicle -- ARGS --logging MaxDetail`.

## 8. Documentation

The documentation is hosted by ReadTheDocs (RTD). The documentation is automatically rebuilt.

## 9. Coding conventions

- In order to maintain flexibility in adding extra fields to `Arg` and `Binder`
  one should avoid pattern-matching on them whenever possible, and instead use suitable
  mapping, traversing and projection functions.
