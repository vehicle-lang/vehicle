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

Now we can install Vehicle itself.

4. Clone the Vehicle github repository to your local computer and
   navigate to the directory.

5. Run ``cabal run build init`` to initialise the project and install
   any dependencies that are needed for building the project.

6. Run ``cabal run build test`` to run the test suite.
  (If this doesn't work then check that check that `~/.cabal/bin` has
   been added to your system path.)

7. Run ``cabal install`` to install the Vehicle executable.

8. Run ``vehicle -h`` to check that Vehicle has been installed.