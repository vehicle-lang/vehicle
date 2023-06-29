# Table of Contents

- [Getting Started](#getting-started)
  - [Installing Vehicle](#installing-vehicle)
  - [Building Vehicle](#building-vehicle)
    - [Getting the source](#getting-the-source)
    - [Building the Vehicle compiler](#building-the-vehicle-compiler)
      - [Dependencies](#dependencies)
        - [Installing GHC and Cabal](#installing-ghc-and-cabal)
        - [The preferred version of GHC](#the-preferred-version-of-ghc)
      - [Building](#building)
      - [Testing](#testing)
        - [Running specific tests](#running-specific-tests)
        - [The unit tests](#the-unit-tests)
        - [The golden tests](#the-golden-tests)
        - [Adding golden tests](#adding-golden-tests)
        - [Updating the golden files](#updating-the-golden-files)
      - [Debugging](#debugging)
        - [Profiling](#profiling)
        - [Loops](#loops)
      - [Installing from source](#installing-from-source)
    - [Building the Vehicle Python bindings](#building-the-vehicle-python-bindings)
      - [Dependencies](#dependencies-1)
        - [Installing Python and pipx](#installing-python-and-pipx)
      - [Building](#building-1)
      - [Testing](#testing-1)
        - [Running tests for specific versions of Python](#running-tests-for-specific-versions-of-python)
        - [Running specific tests](#running-specific-tests-1)
        - [The executable tests](#the-executable-tests)
        - [The loss function tests](#the-loss-function-tests)
        - [The pygments tests](#the-pygments-tests)
      - [Installing from source](#installing-from-source-1)
      - [Installing in editable mode](#installing-in-editable-mode)
  - [Pre-commit Hooks](#pre-commit-hooks)
  - [Editor Support](#editor-support)
- [Publishing a Release](#publishing-a-release)

# Getting Started

## Installing Vehicle

To install the latest version of Vehicle, run the following command:

```sh
pip install vehicle-lang
```

## Building Vehicle

### Getting the source

The main repository is [vehicle-lang/vehicle]. This contains the Vehicle compiler, the the standard library, the Python bindings, the Agda bindings, and a bunch of examples and tools.

The very first step to work on Vehicle is to clone the repository:

```sh
git clone https://github.com/vehicle-lang/vehicle.git
cd vehicle
```

### Building the Vehicle compiler

#### Dependencies

Building the Vehicle compiler requires the Haskell compiler, called [GHC], and the Haskell package manager, called [Cabal].
The Vehicle compiler can be built with:

- _at least_ the latest three major releases of GHC; and
- the latest major release of Cabal.

We recommend that you install [our preferred version of GHC](#the-preferred-version-of-ghc)

##### Installing GHC and Cabal

We recommend you install GHC and Cabal using [GHCup].

1. Install GHCup following the instruction on the website:
   <https://www.haskell.org/ghcup/>

2. Instal GHC 9.4.4 and the latest version of Cabal.

   Run the following commands:

   ```sh
   ghcup upgrade
   ghcup install ghc 9.4.4
   ghcup set ghc 9.4.4
   ghcup install cabal latest
   ghcup set cabal latest
   ```

3. Check if your installation was successful.

   Run the following command:

   ```sh
   ghc --version
   ```

   This should print:

   ```
   The Glorious Glasgow Haskell Compilation System, version 9.4.4
   ```

   Run the following command:

   ```sh
   cabal --version
   ```

   This should print:

   ```
   cabal-install version 3.10.1.0
   compiled using version 3.10.1.0 of the Cabal library
   ```

If you'd like to use a different version of GHC, you can find the list of versions that we test with in [build-vehicle.yml]. However, be aware that building the Python bindings _requires_ [our preferred version](#the-preferred-version-of-ghc).

##### The preferred version of GHC

The preferred version of GHC is currently _GHC 9.4.4_, which is the version of GHC we recommend you use, and which is required to build the Python bindings.

#### Building

Ensure that [you have the source code](#getting-the-source) and that you have installed [GHC and Cabal](#installing-ghc-and-cabal).

1. Update the list of Haskell packages.

   Run the following command:

   ```sh
   cabal update
   ```

2. Navigate to your local copy of the Vehicle repository.

   ```sh
   cd path/to/vehicle
   ```

3. Build the Vehicle compiler:

   ```sh
   cabal build vehicle:exe:vehicle
   ```

#### Testing

Ensure that you can successfully build the Vehicle compiler.

The tests for the Vehicle compiler are in [the tests subdirectory](./vehicle/tests/) and use [the Tasty testing framework] as well as a custom driver for golden file tests—see [`Vehicle.Test.Golden`](./vehicle/tests/golden/Vehicle/Test/Golden.hs).

There are three test suites for the Vehicle compiler:

- [The unit tests](#the-unit-tests) (`unit-tests`)
- [The golden tests](#the-golden-tests) (`golden-tests`)

The standard command to test the Vehicle compiler runs the unit and the compiler tests:

```sh
cabal test unit-tests golden-tests --test-show-details=always --test-option=--color=always --test-option=--num-threads=1
```

This command is run on GitHub Actions whenever changes are pushed to Vehicle the default branch or an open pull request—see [build-vehicle.yml](./.github/workflows/build-vehicle.yml).

This command builds the Vehicle compiler, if necessary, and runs the unit and compiler tests. The last lines of output should tell you about the tests, and should look like:

```
Running 1 test suites...
Test suite unit-tests: RUNNING...
Tests
  DeBruijnIndices
    substUnderLambdaClosed:       OK

  ...

All 18 tests passed (0.00s)
Test suite unit-tests: PASS

Running 1 test suites...
Test suite golden-tests: RUNNING...
Compiler
  compile
    simple-quantifierIn
      TypeCheck:                (0.04s)
      Agda:                     (0.04s)
      Marabou:                  (0.05s)

  ...

All 155 tests passed (12.33s)
Test suite golden-tests: PASS
```

The option `--test-show-details=always` asks the testing framework to print some details about the tests it is running, and `--test-option=--color=always` asks it to use colour. If you omit these options, the output is much less verbose, and looks like:

```
Running 1 test suites...
Test suite unit-tests: RUNNING...
Test suite unit-tests: PASS
Running 1 test suites...
Test suite golden-tests: RUNNING...
Test suite golden-tests: PASS
```

The option `--test-option=--num-threads=1` asks the testing framework to only run one test at a time. If you omit this option, you may get some failing tests due to [#342](https://github.com/vehicle-lang/vehicle/issues/342).

##### Running specific tests

You can use the option `--test-option="-p /X/"` to only run tests with `X` in their name, _e.g._, if you only want to run the tests for the [wind controller example](./examples/windController/), you can add `--test-option="-p /windController/"`:

```
Running 1 test suites...
Test suite golden-tests: RUNNING...
Compiler
  compile
    windController
      TypeCheck: OK (0.05s)
      Marabou:   OK (0.06s)
      Agda:      OK (0.05s)

All 3 tests passed (0.15s)

Test suite golden-tests: PASS
```

If you want to further restrict those to only the test for the Agda backend, you can add `--test-option="-p /windController.Agda/"`:

```
Running 1 test suites...
Test suite golden-tests: RUNNING...
Compiler
  compile
    windController
      Agda:      OK (0.06s)

All 1 tests passed (0.06s)

Test suite golden-tests: PASS
```

For more information, see [the Tasty documentation] on patterns.

##### The unit tests

The unit tests test properties of the internals of Vehicle, _e.g._, of the Vehicle library.

Run the following command:

```sh
cabal test unit-tests --test-show-details=always --test-option=--color=always --test-option=--num-threads=1
```

You can use `--test-option="--vehicle-logging X"` to set the logging level, where `X` is one of `NoDetail`, `MinDetail`, `MidDetail`, or `MaxDetail`. The logging levels can be found by running `vehicle --help`.

These tests are specified in Haskell in [tests/unit](./vehicle/tests/unit/).

##### The golden tests

The golden tests test properties of the compiler as a whole, by running it with various input files and comparing the output to golden files, which have the `.golden` file extension.

Run the following command:

```sh
cabal test golden-tests --test-show-details=always --test-option=--color=always --test-option=--num-threads=1
```

These tests are specified in `test.json` files in [tests/golden](./vehicle/tests/golden/), _e.g._, [windController/test.json](./vehicle/tests/golden/compile/windController/test.json):

```json
[
  {
    "name": "TypeCheck",
    "run": "vehicle check -s spec.vcl",
    "needs": ["spec.vcl"]
  },
  {
    "name": "Marabou",
    "run": "vehicle compile -s spec.vcl -t MarabouQueries -o Marabou.queries/ --network controller:controller.onnx",
    "needs": ["spec.vcl", "controller.onnx"],
    "produces": ["Marabou.queries/*.txt", "Marabou.queries/.vcl-plan"],
    "ignore": {
      "lines": ".*\"fileHash\".*"
    }
  },
  {
    "name": "Agda",
    "run": "vehicle compile -s spec.vcl -t Agda -o Agda.agda --network controller:controller.onnx",
    "needs": ["spec.vcl"],
    "produces": ["Agda.agda"]
  },
  {
    "name": "LossFunction",
    "run": "vehicle compile -s spec.vcl -t DL2Loss -o LossFunction.json --network controller:controller.onnx",
    "needs": ["spec.vcl"],
    "produces": ["LossFunction.json"],
    "enabled": false
  },
  {
    "name": "MarabouVerify",
    "run": "vehicle verify -q Marabou.queries -v Marabou",
    "needs": ["controller.onnx", "Marabou.queries"],
    "external": ["Marabou"]
  }
]
```

Each `test.json` file contains a list of test cases.
Each test case must have the following fields:

- `name`: The name of the test case.
- `run`: The command to run.

Optionally, each test case can specify the following fields:

- `needs`: A list of input files needed by the command.
- `produces`: A list of output files produced by the command.
- `external`: A list of external programs needed by the command.
- `enabled`: Whether the test case is enabled.
- `ignore`: An object containing settings for the diff algorithm.
  - `matches`: A regular expression, which matches lines that should be ignored by the diff algorithm.
- `timeout`: A timeout after which the test case is considered to have failed. The timeout should be a number suffixes with `ms` for miliseconds, `s` for seconds, `m` for minutes, or `h` for hours.

The logging level can be changed by changing the command in the `test.json` file. Changing the logging level changes the output of the command, which breaks the golden test.

Some golden tests require external tools, such as the MarabouVerify test above. To run these tests, add `--test-option="--external=<external>"` to the test command, where `<external>` is the name of the external dependency, such as `Marabou`.

##### Adding golden tests

To create a new golden test, you can use the `new-golden-test` command.

1. Compose the Vehicle command you'd like to test, _e.g._,

   ```sh
   vehicle compile -s spec.vcl -t MarabouQueries -o Marabou.queries -n controller:controller.onnx
   ```

   Use `cabal run vehicle --` rather than `vehicle` to ensure that you are building and running the current version, rather than an old installation.

2. Run the Vehicle command, and check that it succeeds.

3. Run the same Vehicle command, but prefixed with:

   ```sh
   cabal run new-golden-test --
   ```

   For instance:

   ```sh
   cabal run new-golden-test -- vehicle compile -s spec.vcl -t MarabouQueries -o Marabou.queries -n controller:controller.onnx
   ```

   This creates or updates the `test.json` file to add the test.

   You can add `--test-timeout` _before_ the Vehicle command to set a timeout for the test case.

   You can add `--test-path` _before_ the Vehicle command to add the test to a particular directory, which creates or updates the `test.json` file in that directory and copies any necessary files.

##### Updating the golden files

If the output of the Vehicle compiler changes, it is necessary to update the [golden files](./vehicle/tests/golden/) for the compiler tests.

**Warning**: The following is a destructive action! Mistakes may result in faulty tests!

You can use the option `--test-option="--accept"` to accept the new output of the golden tests, and update the golden files.

The procedure for updating the golden files is:

1. Ensure that all changes are committed.
2. Run the following command:
   ```sh
   cabal test golden-tests --test-option=--num-threads=1 --test-option="--accept"
   ```
3. Review the changes to the golden files, _e.g._, by inspecting the output of the following command:
   ```sh
   git diff
   ```
4. The _only_ changes to the golden files.

#### Debugging

##### Profiling

There are scripts for profiling the time and memory consumption of the Vehicle compiler:

- `./scripts/vehicle-profile-time`
- `./scripts/vehicle-profile-heap`

For more information, see the comments at the top of these files.

##### Loops

The compiler tests test the output of a successful run of the Vehicle compiler. If the Vehicle compiler loops, it does not terminate. Consequently, if you suspect there is an infinite loop, it is easier to run Vehicle directly with logging:

```sh
cabal run exe:vehicle -- --logging=MaxDetail ...
```

#### Installing from source

Ensure that [you have the source code](#getting-the-source) and that you have installed [GHC and Cabal](#installing-ghc-and-cabal).

1. Update the list of Haskell packages.

   Run the following command:

   ```sh
   cabal update
   ```

2. Navigate to your local copy of the Vehicle repository.

   ```sh
   cd path/to/vehicle
   ```

3. Install the Vehicle compiler:

   ```sh
   cabal install vehicle:exe:vehicle --install-method=copy --overwrite-policy=always
   ```

   The last few lines of output should tell you _where_ Cabal installed the Vehicle compiler. By default, this is either `~/.local/bin` or `~/.cabal/bin`. You can tell Cabal where to install executables by passing `--installdir`.

   Ensure that the directory where Vehicle is installed is on your system PATH.

4. Check if your installation was successful.

   Run the following command:

   ```sh
   vehicle --version
   ```

   This should print `0.5.1`.

### Building the Vehicle Python bindings

#### Dependencies

Building the Vehicle Python bindings requires [GHC and Cabal](#installing-ghc-and-cabal)—specifically, [our preferred version of GHC](#the-preferred-version-of-ghc)—and [Python and pipx](#installing-python-and-pipx).

The Vehicle Python bindings can be built with all [supported versions of CPython], the standard Python implementation, and the latest version of pipx. Support for feature and prerelease versions of Python is not guaranteed.

##### Installing Python and pipx

We recommend you install Python using [pyenv].

1. Install [pyenv] following the instructions on the website:
   <https://github.com/pyenv/pyenv#installation>

2. Install the latest release of each supported Python version. Currently, those are 3.7, 3.8, 3.9, 3.10, and 3.11.

   Run the following command:

   ```sh
   pyenv install 3.7 3.8 3.9 3.10 3.11
   ```

3. Check if your installation was successful.

   Run the following command:

   ```sh
   pyenv versions
   ```

   This should print something that looks like:

   ```
     system
     3.7.16
     3.8.16
     3.9.16
   * 3.10.11 (set by PYENV_VERSION environment variable)
     3.11.3
   ```

   There may be some differences in the exact versions and the default version (marked by the `*`), and there may or may not be a _system_ version. However, there should be at least one version for each supported Python version, _e.g._, one version starting with 3.7, one starting with 3.8, _etc_.

   Run the following commands:

   ```sh
   pyenv shell 3.11
   python --version
   ```

   This should print something that looks like:

   ```sh
   Python 3.11.3
   ```

   There may be some differences in the exact version. However, the printed version should match the argument passed to `pyenv shell`, _e.g._, it should start with 3.11.

4. Install the latest release of [pipx] following the instructions on the website:
   <https://pypa.github.io/pipx/#install-pipx>

   We recommend installing pipx globally, _e.g._, using your system package manager or the package manager for your _system_ installation of Python, rather than using one of the Python versions managed by pyenv.

5. Check if your installation was successful.

   Run the following command:

   ```sh
   pipx --version
   ```

   This should print something like:

   ```sh
   1.2.0
   ```

   The exact version may differ. However, the printed version should be greater than or equal to 1.2.0.

If you'd prefer not to use pyenv, you can install the latest release of each supported Python version using, _e.g._, your system package manager.

##### Troubleshooting

###### On Linux

If you get an error that looks like:

```
The virtual environment was not created successfully because ensurepip is not
available.  On Debian/Ubuntu systems, you need to install the python3-venv
package using the following command.

    apt install python3.10-venv

You may need to use sudo with that command.  After installing the python3-venv
package, recreate your virtual environment.
```

It means that Vehicle's testing framework is using the system installation of Python, but that the package for creating virtual environments is missing. The solution is to install it using your system package manager, as suggested by the text of the error message.

#### Building

Ensure that [you have the source code](#getting-the-source) and that you have installed both [GHC and Cabal](#installing-ghc-and-cabal) and [Python and pipx](#installing-python-and-pipx).

1. Navigate to your local copy of the Vehicle repository.

   ```sh
   cd path/to/vehicle
   ```

1. Navigate to the `vehicle-python` subdirectory.

   ```sh
   cd vehicle-python
   ```

1. Build the Vehicle Python bindings:

   ```sh
   pipx run tox
   ```

This creates the directory `dist` which contains "wheels", which are the binary distribution format for Python packages.
These wheels will have file names such as `vehicle_lang-0.5.1-cp311-cp311-macosx_13_0_arm64`:

```sh
#   Supported
#   Python   _____
#   versions      \
#                  vvvvvvvvvvv
vehicle_lang-0.5.1-cp311-cp311-macosx_13_0_arm64
#                              ^^^^^^^^^^^^^^^^^
#   Supported                /
#   Operating System  ______/
#   and Architecture
```

On Linux, the operating system will be a [manylinux] platform tag, such as `manylinux2014` or `manylinux_2_28`.
The `manylinux_2_28` tag means that the wheel is compatible with any Linux distribution based on libc 2.28 or later.
The `manylinux2014` tag is an alias for `manylinux_2_17`.

If you'd prefer to only build wheels for _one_ Python version, you can use one of the following options:

- **On macOS and Windows**

  You can use the standard Python build system, [build].

  Run the following command:

  ```sh
  pipx run --spec=build pyproject-build --wheel
  ```

- **On Linux**

  You can use the `build-wheel.sh` script in `vehicle-python/scripts`.
  This script may ask you to install additional dependencies via `pip`.
  Unfortunately, the Linux wheels cannot be built using _just_ Python's standard build system, as they require _delocating_, which is the process of finding non-standard shared libraries and bundling them with the wheel.

**Warning**: The binary distributions built following these instructions are less portable than those that are built by the CI:

- The macOS wheels built following these instructions will require _at least_ your version of macOS, whereas the wheels built on CI are backwards compatible to macOS 10.10 (Yosemite).

- The Linux wheels built following these instructions will require _at least_ your system version of libc, whereas the wheels built on CI are backwards compatible to libc 2.17 (Ubuntu 18.04).

  You can determine your system's libc version via Python by running:

  ```sh
  python -c 'import platform; print(platform.libc_ver())'
  ```

#### Testing

Ensure that you can successfully build the Vehicle Python bindings.
The tests for the Vehicle Python bindings are in [the tests subdirectory](./vehicle-python/tests/) and use [tox] for Python version and virtual environment management and [pytest] for test discovery and execution.
The configuration for tox and pytest is in [`pyproject.toml`](./vehicle-python/pyproject.toml) under `[tool.tox]` and `[tool.pytest]`, respectively.

There are two test suites for the Vehicle Python bindings:

- [The executable tests](#the-executable-tests)
- [The loss function tests](#the-loss-function-tests)
- [The pygments tests](#the-pygments-tests)

The standard command to test the Vehicle compiler runs both:

```sh
pipx run tox
```

This command build the Python bindings and runs all test suites with each supported version of Python.

##### Running tests for specific versions of Python

You can use the tox option `-e` to run a specific environment, _e.g._, to only run tests for Python 3.11 on macOS, run the following command:

```sh
pipx run tox run -e py311-mac
```

The environments are defined in [`pyproject.toml`](./vehicle-python/pyproject.toml) under `[tool.tox]`, and are combinations of the Python version and the platform (`lin`, `mac`, or `win`).

##### Running specific tests

The arguments after `--` are passed to pytest.

You can pass the path to a test file, which only runs the tests in that file:

```sh
pipx run tox -- tests/test_main.py
```

You can pass `-k 'X'` to only run tests with `X` in their name, _e.g._, if you only want to run the `test_main` function from `tests/test_main.py`, you can run:

```sh
pipx run tox -- -k 'test_main'
```

For more information, see [the pytest documentation].

##### The executable tests

The executable tests test whether the Vehicle compiler is installed as part of the Python package.

Run the following command:

```sh
pipx run tox -- tests/test_main.py
```

##### The loss function tests

The loss function tests test the translation and use of properties from Vehicle specification files as loss functions.

Run the following command:

```sh
pipx run tox -- tests/test_loss_function*.py
```

##### The pygments tests

The pygments tests test the integration with [the Pygments syntax highlighter].

These tests are in [test_pygments.sh](./vehicle-python/tests/test_pygments.sh), do not use pytest and as such are always run when running tox.

This is a known bug, and PR [#561](https://github.com/vehicle-lang/vehicle/pull/561) is meant to address this.

#### Installing from source

Ensure that [you have the source code](#getting-the-source) and that you have installed both [GHC and Cabal](#installing-ghc-and-cabal) and [Python and pipx](#installing-python-and-pipx).

1. Navigate to your local copy of the Vehicle repository.

   ```sh
   cd path/to/vehicle
   ```

1. Navigate to the `vehicle-python` subdirectory.

   ```sh
   cd vehicle-python
   ```

1. Install the Vehicle Python bindings:

   ```sh
   python -m pip install .
   ```

   This installs the Vehicle compiler and the `vehicle_lang` Python package.

1. Check if your installation of the Vehicle compiler was successful.

   Run the following command:

   ```sh
   vehicle --version
   ```

   This should print `0.5.1`.

1. Check if your installation of the `vehicle_lang` package was successful.

   Run the following command:

   ```sh
   python -c 'import vehicle_lang; print(vehicle_lang.VERSION)'
   ```

   This should print the same version as above.

#### Installing in editable mode

If you are developing the Python bindings it can be cumbersome to rebuild the Vehicle compiler from source on every test run. To avoid this, you can install the Python bindings in editable mode.

Run the following command from the `vehicle-python` subdirectory:

```sh
python -m pip install -e .[test]
```

This installs the Python bindings in [editable mode], which directly adds the files in the development directory are added to Python's import path.

When the Python bindings are installed in editable mode, you can run pytest directly:

```sh
python -m pytest
```

You'll have to reinstall the Python bindings when the metadata in `pyproject.toml` or the Haskell source changes.

## Pre-commit Hooks

The Vehicle repository has a variety of pre-commit hooks that check and ensure code quality, managed by [pre-commit]. The pre-commit hooks require [pre-commit], [cabal-fmt] and [ormolu].

We recommend that you install these hooks.

1. Ensure that you have installed [GHC and Cabal](#installing-ghc-and-cabal).

2. Install pre-commit following the instruction on the website:
   <https://pre-commit.com/#install>

3. Install cabal-fmt.

   Run the following command:

   ```sh
   cabal install cabal-fmt --ignore-project --overwrite-policy=always
   ```

4. Install ormolu.

   Run the following command:

   ```sh
   cabal install ormolu --ignore-project --overwrite-policy=always
   ```

5. Navigate to your local copy of the Vehicle repository.

   ```sh
   cd path/to/vehicle
   ```

6. Install the pre-commit hooks.

   Run the following command:

   ```sh
   pre-commit install
   ```

   This should print:

   ```sh
   pre-commit installed at .git/hooks/pre-commit
   ```

   If you ever clone a fresh copy of the Vehicle repository, you'll have to rerun this command.

7. Test the pre-commit hooks.

   Run the following command:

   ```sh
   pre-commit run --all-files
   ```

The hooks run every time you run `git commit`. You can skip the hooks by adding the `--no-verify` flag to your Git command.

## Editor Support

You can use whatever development environment you prefer.

We recommend using [VSCode] with the following extensions, based on what parts of Vehicle intend to work on:

| Project        | Language | Extension                                                                                                                                                                                                                                                                                                                                               |
| -------------- | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| _any_          | Vehicle  | [Vehicle Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=wenkokke.vehicle-syntax-highlighting)                                                                                                                                                                                                                                 |
| _any_          | Haskell  | [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), [Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)                                                                                                                                                          |
| _any_          | Cabal    | [cabal-fmt](https://marketplace.visualstudio.com/items?itemName=berberman.vscode-cabal-fmt)                                                                                                                                                                                                                                                             |
| _any_          | Markdown | [MyST-Markdown](https://marketplace.visualstudio.com/items?itemName=ExecutableBookProject.myst-highlight)                                                                                                                                                                                                                                               |
| vehicle-agda   | Agda     | [agda-mode](https://marketplace.visualstudio.com/items?itemName=banacorn.agda-mode)                                                                                                                                                                                                                                                                     |
| vehicle-python | Python   | [Python](https://marketplace.visualstudio.com/items?itemName=ms-python.python), [Pylance](https://marketplace.visualstudio.com/items?itemName=ms-python.vscode-pylance), [Black Formatter](https://marketplace.visualstudio.com/items?itemName=ms-python.black-formatter), [isort](https://marketplace.visualstudio.com/items?itemName=ms-python.isort) |
| vehicle-python | TOML     | [Even Better TOML](https://marketplace.visualstudio.com/items?itemName=tamasfe.even-better-toml)                                                                                                                                                                                                                                                        |

# Publishing a Release

Vehicle is released via [PyPI], the Python Package Index.

Ensure that [you have the source code](#getting-the-source) and that you have installed both [GHC and Cabal](#installing-ghc-and-cabal) and [Python and pipx](#installing-python-and-pipx).

To publish new releases to PyPI, you need a PyPI account that is authorised as a collaborator on [the `vehicle_lang` project], and you need to create a [PyPI API token] for that account and add it to your [.pypirc file].

Finally, you need access to a machine running macOS with an M1/M2 chipset.

The procedure to create a new release is:

1. Navigate to your local copy of the Vehicle repository.

2. Ensure that you are on the default branch `dev`.

3. Ensure that all changes are _committed and pushed_.

4. Ensure that the tests are passing on CI:
   <https://github.com/vehicle-lang/vehicle/actions/workflows/ci.yml?query=branch%3Adev>

5. Run all tests and fix any errors.

   **Vehicle compiler tests**

   Run the following command from the root of the repository:

   ```sh
   cabal test all --test-option=--num-threads=1
   ```

   **Vehicle Python bindings tests**

   Run the following command from `vehicle-python`:

   ```sh
   pipx run tox
   ```

   **Vehicle documentation tests**

   Run the following command from `docs`:

   ```sh
   pipx run tox
   ```

   If any errors occur, fix them, and restart from step **1**.

6. Choose the appropriate version number to increase.

   - If you're increasing the **major** version, run this command:

     ```sh
     pipx run bumpver update --major --dry
     ```

   - If you're increasing the **minor** version, run this command:

     ```sh
     pipx run bumpver update --minor --dry
     ```

   - If you're increasing the **patch** version, run this command:

     ```sh
     pipx run bumpver update --patch --dry
     ```

   The output will contain a diff of the changes to be made.

   If the diff looks reasonable, rerun the command without the `--dry` flag.

   This will update the version, create a Git tag, and push it to GitHub.

7. Ensure that the CI successfully builds and publishes Vehicle to PyPI:
   <https://github.com/vehicle-lang/vehicle/actions/workflows/ci.yml?query=branch%3Adev>

8. **On a macOS machine with an M1/M2 chipset**

   There are no GitHub Actions runners with an M1/M2 chipset, so the binary distributions for this platform must be built and published manually from an appropriate machine.

   Run the following command from `vehicle-python`:

   ```sh
   pipx run tox
   ```

   This creates the directory `dist` which contains "wheels", which are the binary distribution format for Python packages.
   If you're on macOS with an M1/M2 chipset, these look like:

   ```
   vehicle_lang-0.5.1-cp310-cp310-macosx_13_0_arm64.whl
   vehicle_lang-0.5.1-cp37-cp37m-macosx_13_0_arm64.whl
   vehicle_lang-0.5.1-cp39-cp39-macosx_13_0_arm64.whl
   vehicle_lang-0.5.1-cp311-cp311-macosx_13_0_arm64.whl
   vehicle_lang-0.5.1-cp38-cp38-macosx_13_0_arm64.whl
   ```

   Run the following command to check each wheel's metadata:

   ```sh
   pipx run twine check --strict dist/*.whl
   ```

   **Warning**: The following is a destructive action! Published versions cannot be changed!

   Run the following command to upload each wheel to [PyPI]:

   ```sh
   pipx run twine upload dist/*.whl
   ```

   Edit the release on GitHub and add the wheel files in `dist/`.

   The release will be at a URL like:

   <https://github.com/vehicle-lang/vehicle/releases/tag/v0.5.0>

[vehicle-lang/vehicle]: https://github.com/vehicle-lang/vehicle
[GHC]: https://www.haskell.org/ghc/
[Cabal]: https://www.haskell.org/cabal/
[build-vehicle.yml]: .github/workflows/build-vehicle.yml#
[GHCup]: https://www.haskell.org/ghcup/
[SWIG]: https://swig.org
[pipx]: https://pypa.github.io/pipx/
[Python]: https://www.python.org
[supported versions of CPython]: https://devguide.python.org/versions/
[pyenv]: https://github.com/pyenv/pyenv
[the Tasty testing framework]: https://hackage.haskell.org/package/tasty
[the Tasty documentation]: https://hackage.haskell.org/package/tasty#patterns
[tox]: https://tox.wiki/en/latest/
[pytest]: https://docs.pytest.org/en/latest/
[the pytest documentation]: https://docs.pytest.org/en/7.3.x/how-to/usage.html#specifying-which-tests-to-run
[editable mode]: https://pip.pypa.io/en/latest/topics/local-project-installs/
[the Pygments syntax highlighter]: https://pygments.org
[PyPI]: https://pypi.org
[the `vehicle_lang` project]: https://pypi.org/project/vehicle-lang/
[.pypirc file]: https://packaging.python.org/en/latest/specifications/pypirc/
[PyPI API token]: https://pypi.org/help/#apitoken
[VSCode]: https://code.visualstudio.com/
[pre-commit]: https://pre-commit.com/
[cabal-fmt]: https://hackage.haskell.org/package/cabal-fmt
[ormolu]: https://hackage.haskell.org/package/ormolu
[build]: https://pypi.org/project/build/
[manylinux]: https://github.com/pypa/manylinux#readme
