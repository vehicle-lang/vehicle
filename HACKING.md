# Hacking on Vehicle

This file contains instructions for how to get started to contributing to Vehicle.

## 1. Set up

### On Linux

The Vehicle compiler is written in Haskell. The first task is to install Haskell itself:

1. Install GHCUp following the instructions from https://www.haskell.org/ghcup/.

2. Close and reopen your terminal.

3. Run ``ghcup tui`` and use it to install and set:
  -  GHC 9.0.X (for some version of X)
  -  Cabal 3.X (for some version of X)

4. Run ``cabal update`` to update your list of packages.

Now we can install the Vehicle compiler itself.

1. Clone the Vehicle github repository to your local computer and
   navigate to the directory.

2. Run ``git checkout v0.2.0`` to check out the latest version (change the version as required).

3. Run ``cabal install exe:vehicle`` to install the Vehicle executable on your system.

4. Run ``vehicle -h`` to check that Vehicle has been installed.
  (If this doesn't work then check that check that `~/.cabal/bin` has
   been added to your system path.)

**Troubleshooting**

* Check if you're using the right versions of GHC and Cabal.

* Check if you have any other installations of GHC and Cabal not managed by GHCUp.
  Either remove those installations or make sure that GHCUp is earlier in the PATH environment variable.

**Updating Vehicle**

To update an existing Vehicle installation run ``cabal install exe:vehicle --overwrite-policy=always`` to re-install the new Vehicle executable on your system.

### On Windows

The easiest way is:

* Install the Windows Subsystem for Linux (WSL) from the Microsoft Store.

* Follow the instructions for Linux above in a WSL terminal.

.. warning::

    Although Vehicle itself supports and is tested on Windows, that does
    not mean that all backends will work on Windows. For example ``Marabou``
    does not currently support Windows.

**Troubleshooting**

* If you have problems with the WSL check if you're using the latest version.

* If you get the error: Missing (or bad) C libraries: icuuc, icuin, icudt
Go to https://github.com/microsoft/vcpkg#quick-start-windows and follow the instructions.


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

- Adding `--test-option="-p /X/"` to any of the test commands will only run tests
  with `X` in their name. If you only want to run a test for a particular backend `Y` change the `X` to `X-Y`, e.g. `quantifier-Agda`.

- Adding `--test-option="--accept"` to the command will accept the output of golden tests.
  _Warning_: Only run this if you are okay with the changes to the output!

### Unit tests

These test the internal functionality of the Vehicle library etc.

- `cabal test vehicle-unit-tests --test-show-details=always` will run the tests.

- Adding `--test-option="-p /X/"` to the command will only run tests with `X` in their name.

- Adding `--test-option="--vehicle-logging X"` to the command will set the logging level to
  `X` where the various levels can be found with the `vehicle --help` command.

### Compiler tests

These golden tests check the output of the Vehicle compiler. They therefore do not
depend on any external programs being installed.

- `cabal test vehicle-compiler-tests --test-show-details=always` will run the tests.

- The logging level can be changed for the golden tests by changing the vehicle command
  in the `test.json` files in the golden test suite.

- To create a new golden test json file you can run `cabal run vehicle-new-golden-test -- Y`
  where `Y` is the Vehicle command you would run e.g. `vehicle compile --target Marabou --specification spec.vcl --network myNetwork.onnx --outputFile=spec-input-query`.

- If you add `--test-path my/path/` after `vehicle-new-golden-test` and before `--` then this
  will automatically copy the generated `test.json` file and all the file dependencies to the
  provided path (e.g. `--test-path tests/golden/compile/spec`)

### Integration tests

These golden tests check how Vehicle interacts with external tools. They therefore
rely on `Marabou` being available on the system path.

- `cabal test vehicle-integration-tests --test-show-details=always` will run the tests.

- The logging level can be changed for the golden tests by changing the vehicle command
  in the `test.json` files in the golden test suite.

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
