Installation
============

.. _installation:

At the moment, the only way to install Vehicle is from source. We aim to make it
available as both an executable and a Python package in the near future.

Building from source
--------------------

Vehicle is written in Haskell. The first task is to install Haskell itself:

1. Install GHCUp following the instructions from https://www.haskell.org/ghcup/.

2. Close and reopen your terminal.

3. Run ``ghcup tui`` and use it to install and set:
  -  GHC 9.0.X (for some version of X)
  -  Cabal 3.X (for some version of X)

4. Run ``cabal update`` to update your list of packages.

Now we can install Vehicle itself.

1. Clone the Vehicle github repository to your local computer and
   navigate to the directory.

2. Run ``cabal run build init`` to initialise the project and install
   any dependencies that are needed for building the project, pressing
   ``y`` as required.

3. Run ``cabal run build test`` to run the test suite.
  (If this doesn't work then check that check that `~/.cabal/bin` has
   been added to your system path.)

4. Run ``cabal install`` to install the Vehicle executable.

5. Run ``vehicle -h`` to check that Vehicle has been installed.

Syntax highlighting
-------------------

The syntax highlighting can be installed in VSCode by going to the
"Extensions" tab and searching for "Vehicle Syntax Highlighting".

Contributions adding syntax highlighting to other IDEs would be welcome.
The source code for the VSCode extension is available
`here <https://github.com/vehicle-lang/vscode-vehicle-syntax-highlighting>`_.