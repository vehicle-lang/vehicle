import os.path
import subprocess
import sys
from typing import Optional

from packaging.version import Version
from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext

# 03-04-2023:
# The imports from distutils must come AFTER the imports from setuptools,
# because setuptools overrides the distutils modules with its own versions.
# isort: split

import distutils.errors
import distutils.spawn

ext_modules = [
    Extension(
        name="vehicle_cli._binding",
        sources=["src/vehicle_cli/binding.i"],
    ),
]


class cabal_build_ext(build_ext):
    def finalize_options(self):
        super().finalize_options()

        if sys.platform in ["win32", "cygwin"]:
            self.libraries.append("python%d%d" % sys.version_info[:2])

    def build_extension(self, ext: Extension):
        # Taken from setuptools:
        # https://github.com/pypa/setuptools/blob/245d72a8aa4d47e1811425213aba2a06a0bb64fa/setuptools/command/build_ext.py#L240-L241
        ext._convert_pyx_sources_to_lang()  # type: ignore[attr-defined]

        # Taken from distutils:
        # https://github.com/pypa/distutils/blob/4435cec31b8eb5712aa8bf993bea3f07051c24d8/distutils/command/build_ext.py#L504-L513
        sources = ext.sources
        if sources is None or not isinstance(sources, (list, tuple)):
            raise distutils.errors.DistutilsSetupError(
                "in 'ext_modules' option (extension '%s'), "
                "'sources' must be present and must be "
                "a list of source filenames" % ext.name
            )
        # sort to make the resulting .so file build reproducible
        sources = sorted(sources)

        # First, scan the sources for SWIG definition files (.i), run
        # SWIG on 'em to create .c files, and modify the sources list
        # accordingly.
        sources = self.swig_sources(sources, ext)

        # Next, build the sources with Cabal.
        # NOTE: This requires a valid .cabal file that defines a foreign library called _binding.
        self.mkpath(self.build_temp)
        self.check_ghc_version()
        self.check_cabal_version()
        self.cabal_update()
        self.cabal_configure_ext(ext)
        self.cabal_build_ext(ext)

        # Taken from setuptools:
        # https://github.com/pypa/setuptools/blob/245d72a8aa4d47e1811425213aba2a06a0bb64fa/setuptools/command/build_ext.py#L247-L249
        if ext._needs_stub:  # type: ignore[attr-defined]
            build_lib = self.get_finalized_command("build_py").build_lib  # type: ignore[attr-defined]
            self.write_stub(build_lib, ext)

    def cabal_update(self):
        self.cabal(["update"])

    def cabal_configure_ext(self, ext: Extension):
        self.cabal(
            [
                "configure",
                *(f"--extra-lib-dirs={dir}" for dir in self.library_dirs),
                *(f"--extra-include-dirs={dir}" for dir in self.include_dirs),
                *(f"--ghc-options=-optl-l{library}" for library in self.libraries),
            ]
        )

    def cabal_build_ext(self, ext: Extension):
        self.mkpath(self.build_temp)
        self.cabal(["build"], env={"INSTALLDIR": self.build_temp, **os.environ})
        lib_filename = self.get_cabal_foreign_library_filename(ext)
        ext_fullpath = self.get_ext_fullpath(ext.name)
        self.mkpath(os.path.dirname(ext_fullpath))
        self.copy_file(os.path.join(self.build_temp, lib_filename), ext_fullpath)

    def get_cabal_foreign_library_filename(self, ext):
        if sys.platform not in ["darwin", "linux", "win32", "cygwin"]:
            raise distutils.errors.DistutilsPlatformError(
                f"unsupported platform {self.plat_name}"
            )
        library_prefix = "" if sys.platform in ["win32", "cygwin"] else "lib"
        component_name = ext.name.split(".")[-1]
        dynlib_extension = {
            "darwin": "dylib",
            "linux": "so",
            "win32": "dll",
            "cygwin": "dll",
        }[sys.platform]
        return f"{library_prefix}{component_name}{os.path.extsep}{dynlib_extension}"

    def cabal(self, args, *, env=None):
        args = [self.find_cabal(), *args]
        cmd = " ".join(args)
        print(cmd)
        exitCode = subprocess.call(args, env=env)
        if exitCode != 0:
            raise distutils.errors.DistutilsExecError(
                f"error occurred when running '{cmd}'"
            )

    _cabal: Optional[str] = None

    def find_cabal(self):
        if self._cabal is None:
            self._cabal = distutils.spawn.find_executable("cabal")
            if self._cabal is None:
                raise distutils.errors.DistutilsExecError(
                    "Could not find executable 'cabal'. "
                    "Building vehicle-cli requires GHC and Cabal. "
                    "See http://github.com/vehicle-lang/vehicle#readme"
                )
        return self._cabal

    def check_cabal_version(self):
        cabal_version = subprocess.getoutput(f"{self.find_cabal()} --numeric-version")
        if Version(cabal_version) < Version("3.8"):
            raise distutils.errors.DistutilsExecError(
                "Building vehicle-cli requires GHC (>=8.10) and Cabal (>=3.8). "
                "See https://www.haskell.org/ghcup/"
            )

    _ghc: Optional[str] = None

    def find_ghc(self):
        if self._ghc is None:
            self._ghc = distutils.spawn.find_executable("ghc")
            if self._ghc is None:
                raise distutils.errors.DistutilsExecError(
                    "Could not find executable 'ghc'. "
                    "Building vehicle-cli requires GHC (>=8.10) and Cabal (>=3.8). "
                    "See https://www.haskell.org/ghcup/"
                )
        return self._ghc

    def check_ghc_version(self):
        ghc_version = subprocess.getoutput(f"{self.find_ghc()} --numeric-version")
        if Version(ghc_version) < Version("8.10"):
            raise distutils.errors.DistutilsExecError(
                "Building vehicle-cli requires GHC (>=8.10) and Cabal (>=3.8). "
                "See https://www.haskell.org/ghcup/"
            )


def main():
    setup(
        ext_modules=ext_modules,
        cmdclass={"build_ext": cabal_build_ext},
    )


if __name__ == "__main__":
    main()
