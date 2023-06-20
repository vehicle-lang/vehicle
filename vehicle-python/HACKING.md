# Building wheels

Build a Python wheel with the standard...

```sh
# pip install build
python -m build --wheel --outdir=wheelhouse
```

This will produce a .whl file in the wheelhouse directory.
These wheels will have a name such as:

```sh
#     Supported
#     Python   _____
#     versions      \
#                    vvvvvvvvvvv
vehicle_lang-0.2.0a2-cp311-cp311-macosx_13_0_arm64
#                                ^^^^^^^^^^^^^^^^^
#     Supported                /
#     Operating System  ______/
#     and Architecture
```

On macOS and Windows, you can install the generated wheel with...

```sh
pip install wheelhouse/$WHEEL
```

...where `$WHEEL` should be the name of the wheel.

On macOS and Windows, you can also install the package globally with...

```sh
pip install .
```

...from within this directory to install the package.

Unfortunately, on most Linux distributions, GHC does not support building libraries with the `standalone` option, which means that at runtime our Python extension will try to link with a bunch of Haskell libraries that it _probably_ won't be able to find.

To fix this, you _must_ repair the generated wheels with `auditwheel`...

```sh
# pip install auditwheel
auditwheel repair --plat=manylinux_$X_$Y_$ARCH wheelhouse/$WHEEL
```

...where `$ARCH` should be your machine's architecture---which should be one of `x86_64`, `i686`, `aarch64`, `armv7l`, `ppc64`, `ppc64le`, or `s390x`---and `$X` and `$Y` should be your operating system's libc major and minor version nubmers. For instance,

- Ubuntu 20.04 uses libc 2.31, so replace `$X` with `2` and `$Y` with `31`;
- Ubuntu 22.04 uses libc 2.35, so replace `$X` with `2` and `$Y` with `35`.

In general, libc is FORWARDS but NOT BACKWARDS compatible, e.g., binaries
built against libc 2.31 can be run on a system with libc 2.35, but not the
other way around, so the older your libc, the more portable the wheel.

You can get your operating system's libc version via Python by running:

```sh
python -c 'import platform; print(platform.libc_ver())'
```

The repaired wheels should have "manylinux" in their filename, and should contain all the libraries necessary to run on any Linux distribution with the same or a newer libc version.

You can install this wheel with...

```sh
pip install wheelhouse/$WHEEL
```

...where `$WHEEL` should be the name of the _repaired_ wheel.

There are similar tools for repairing wheels on macOS and Windows.

On macOS you can use `delocate-wheel`...

```sh
# pip install delocate
delocate-wheel wheelhouse/$WHEEL
```

...and on Windows, you can use `delvewheel`...

```sh
# pip install delvewheel
delvewheel wheelhouse/$WHEEL
```

These tools are run as part of our build process on CI to _ensure_ that we get portable wheels, but as long as GHC correctly builds standalone libraries on those platforms, and Vehicle doesn't link against any external libraries, this isn't _strictly_ necessary.
