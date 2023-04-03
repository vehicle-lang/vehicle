import os.path
import sys
import subprocess
from typing import Optional
from setuptools import Extension, setup
from setuptools.command.build_ext import build_ext
from distutils.errors import (
    DistutilsExecError,
    DistutilsPlatformError,
    DistutilsSetupError,
)
from distutils.spawn import find_executable

ext_modules = [
    Extension(
        name="vehicle.core._binding",
        sources=["src/vehicle/core/binding.i"],
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
        ext._convert_pyx_sources_to_lang()

        # Taken from distutils:
        # https://github.com/pypa/distutils/blob/4435cec31b8eb5712aa8bf993bea3f07051c24d8/distutils/command/build_ext.py#L504-L513
        sources = ext.sources
        if sources is None or not isinstance(sources, (list, tuple)):
            raise DistutilsSetupError(
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
        self.cabal_configure_ext()
        self.cabal_build_ext(ext)

        # Taken from setuptools:
        # https://github.com/pypa/setuptools/blob/245d72a8aa4d47e1811425213aba2a06a0bb64fa/setuptools/command/build_ext.py#L247-L249
        if ext._needs_stub:
            build_lib = self.get_finalized_command("build_py").build_lib
            self.write_stub(build_lib, ext)

    def cabal_configure_ext(self):
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
            raise DistutilsPlatformError(f"unsupported platform {self.plat_name}")
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
            raise DistutilsExecError(f"error occurred when running '{cmd}'")

    _cabal: Optional[str] = None

    def find_cabal(self):
        if self._cabal is None:
            self._cabal = find_executable("cabal")
            if self._cabal is None:
                raise DistutilsExecError("could not find executable 'cabal'")
        return self._cabal


def main():
    setup(
        ext_modules=ext_modules,
        cmdclass={"build_ext": cabal_build_ext},
    )


if __name__ == "__main__":
    main()