Installation
============

.. _installation:

At the moment, the only way to install Vehicle is from source. We aim to make it
available as both an executable and a Python package in the near future.

Building from source
--------------------

On Linux
********

Vehicle is written in Haskell. The first task is to install Haskell itself:

1. Install GHCUp following the instructions from https://www.haskell.org/ghcup/.

2. Close and reopen your terminal.

3. Run ``ghcup tui`` and use it to install and set:
  -  GHC - 8.10.X or 9.0.X (for any version of X)
  -  Cabal - 3.4 or later

4. Run ``cabal update`` to update your list of packages.

Now we can install Vehicle itself.

1. Clone the Vehicle Github repository to your local computer and
   navigate to the directory.

2. Run ``cabal install exe:vehicle`` to install the Vehicle executable.

3. Run ``vehicle -h`` to check that Vehicle has been installed.

**Troubleshooting**

* Check if you're using the right versions of GHC and Cabal.

* Check if you have any other installations of GHC and Cabal not managed by GHCUp.
  Either remove those installations or make sure that GHCUp is earlier in the PATH environment variable.

On Windows
**********

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

Setting Vehicle to work with Agda
---------------------------------

If you want to enable Vehicle to work with Agda you will need to:

1. Add the path to the ``vehicle`` executable to the Agda ``executables`` file.
   On Linux and MacOS systems this path can be found with ``which vehicle`` and
   on Windows this can be found with ``where vehicle``.

2. Add the path to the ``vehicle.agda-lib`` library (in the ``vehicle-agda`` folder)
   to the Agda ``libraries`` file.

See the Agda documentation on
`Library Management <https://agda.readthedocs.io/en/latest/tools/package-system.html>`_
for more details.

Syntax highlighting
-------------------

The syntax highlighting can be installed in VSCode by going to the
"Extensions" tab and searching for "Vehicle Syntax Highlighting".

Contributions adding syntax highlighting to other IDEs would be welcome.
The source code for the VSCode extension is available
`here <https://github.com/vehicle-lang/vscode-vehicle-syntax-highlighting>`_.
