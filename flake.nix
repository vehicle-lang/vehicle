{
  description = "Vehicle: A language for verifying neural networks";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    dream2nix.url = "github:nix-community/dream2nix";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
  flake-parts.lib.mkFlake { inherit inputs; } {
    systems = nixpkgs.lib.systems.flakeExposed;
    imports = [ inputs.haskell-flake.flakeModule ];
    debug = true;
    perSystem = { self', system, config, pkgs, lib, ... }: 
    let 
    # Override config to allow broken packages
    pkgs = import nixpkgs {
      inherit system;
      config = {
        allowBroken = true;
        allowUnfree = true;
      };
    };

    # Setup for Agda
    agdaWithPackages = pkgs.agda.withPackages (ps: [
      ps.standard-library
    ]);

    # Python environment with all needed packages
    pythonEnv = pkgs.python3.withPackages (ps: with ps; [
      numpy
      tensorflow
      pytest
      pygments
      pip
      setuptools
      wheel
      pyyaml
    ]);

    # Fix linkedhashmap in haskell packages
    haskellPackages = pkgs.haskellPackages.override {
      overrides = hself: hsuper: {
        # Fix broken packages
        linkedhashmap = pkgs.haskell.lib.unmarkBroken (
          pkgs.haskell.lib.doJailbreak hsuper.linkedhashmap
        );

        # Also fix other packages with version constraints
        Diff = pkgs.haskell.lib.doJailbreak hsuper.Diff;
        tasty = pkgs.haskell.lib.doJailbreak hsuper.tasty;
        bytestring = hsuper.bytestring;

        # Local packages
        vehicle-syntax = pkgs.haskell.lib.doJailbreak (
          pkgs.haskell.lib.dontCheck (
            hself.callCabal2nix "vehicle-syntax" ./vehicle-syntax {}
          )
        );

        tasty-golden-executable = pkgs.haskell.lib.doJailbreak (
          pkgs.haskell.lib.dontCheck (
            pkgs.haskell.lib.appendConfigureFlags
            (hself.callCabal2nix "tasty-golden-executable" ./tasty-golden-executable {})
            ["--disable-tests"]
          )
        );

        vehicle = pkgs.haskell.lib.doJailbreak (
          pkgs.haskell.lib.dontCheck (hself.callCabal2nix "vehicle" ./vehicle {})
        );
      };
    };

    # Process the SWIG interface file to generate C wrapper
    swigGen = pkgs.stdenv.mkDerivation {
      name = "vehicle-swig-gen";
      src = ./vehicle-python;
      
      nativeBuildInputs = [ pkgs.swig pkgs.python3 ];
      
      buildPhase = ''
        cd src/vehicle_lang
        swig -python -o binding_wrap.c binding.i
        cd ../..
      '';
      
      installPhase = ''
        mkdir -p $out/src/vehicle_lang
        cp src/vehicle_lang/binding_wrap.c $out/src/vehicle_lang/
        cp src/vehicle_lang/binding.i $out/src/vehicle_lang/
        cp src/vehicle_lang/binding.def $out/src/vehicle_lang/
      '';
    };
    
    # Build the Haskell library with the C wrapper
    vehicle-lang = with haskellPackages; pkgs.haskell.lib.overrideCabal 
      (pkgs.haskell.lib.doJailbreak 
        (pkgs.haskell.lib.addExtraLibraries
          (callCabal2nix "vehicle-python-binding" ./vehicle-python {})
          [ vehicle tasty-golden-executable optparse-applicative BNFC text ])
      ) (old: {
        # Make sure GCC can find Python.h and vendor files are available
        preConfigure = ''
          # Find Python include directory
          pythonIncludeDir=${pkgs.python3}/include/python3.12
          configureFlags+=" --extra-include-dirs=$pythonIncludeDir"
          
          # Create a clean vendor directory 
          rm -rf vendor
          mkdir -p vendor
          
          # Copy necessary files for vehicle-syntax
          mkdir -p vendor/vehicle-syntax/src/Vehicle/Syntax
          cp ${./vehicle-syntax}/src/Vehicle/Syntax/External.cf vendor/vehicle-syntax/src/Vehicle/Syntax/
          cp ${./vehicle-syntax}/src/Vehicle/Syntax/Internal.cf vendor/vehicle-syntax/src/Vehicle/Syntax/
          cp ${./vehicle-syntax}/vehicle-syntax.cabal vendor/vehicle-syntax/
          
          # Copy necessary files for vehicle - use copying instead of symlinks
          mkdir -p vendor/vehicle/src
          cp ${./vehicle}/vehicle.cabal vendor/vehicle/
          cp -r ${./vehicle}/src/Vehicle vendor/vehicle/src/
          
          # Copy necessary files for tasty-golden-executable - use copying instead of symlinks
          mkdir -p vendor/tasty-golden-executable
          cp ${./tasty-golden-executable}/tasty-golden-executable.cabal vendor/tasty-golden-executable/
          cp -r ${./tasty-golden-executable}/src vendor/tasty-golden-executable/
          
        '';
        preBuild = ''
          # Copy in the SWIG-generated wrapper
          cp ${swigGen}/src/vehicle_lang/binding_wrap.c src/vehicle_lang/binding_wrap.c
        '';
      });
    # Python bindings with pre-built Haskell library
    # Create a pure Python package without a C extension that just wraps the vehicle executable
    vehiclePython = pkgs.stdenv.mkDerivation {
      name = "python3-vehicle-lang";
      version = "0.16.1";
      
      # Use vehicle-python directory but exclude the binding parts
      src = pkgs.runCommand "vehicle-python-pure" {} ''
        mkdir -p $out/src/vehicle_lang
        cp -r ${./vehicle-python}/src/vehicle_lang/*.py $out/src/vehicle_lang/
        cp -r ${./vehicle-python}/src/vehicle_lang/py.typed $out/src/vehicle_lang/
        
        # Copy all subdirectories
        for dir in $(find ${./vehicle-python}/src/vehicle_lang -mindepth 1 -type d); do
          base_name=$(basename "$dir")
          if [ -d "$dir" ]; then
            mkdir -p $out/src/vehicle_lang/$base_name
            cp -r $dir/* $out/src/vehicle_lang/$base_name/
          fi
        done
        
        # Create a simple setup.py
        cat > $out/setup.py << EOF
from setuptools import setup, find_packages

setup(
    name="vehicle-lang",
    version="0.16.1",
    packages=find_packages("src"),
    package_dir={"": "src"},
    package_data={"vehicle_lang": ["py.typed"]},
    python_requires=">=3.8",
    install_requires=[
        "numpy",
        "pygments",
        "tensorflow",
    ],
)
EOF
        
        # Create a stub _binding.py module
        cat > $out/src/vehicle_lang/_binding.py << EOF
print("Using stub Vehicle Python binding - calling vehicle executable directly")
import os
import subprocess

def vehicle_main(*args):
    vehicle_path = os.environ.get("VEHICLE_PATH", "vehicle")
    subprocess.run([vehicle_path] + list(args))
EOF
      '';
      
      nativeBuildInputs = [
        pkgs.python3
        pkgs.python3Packages.setuptools
        pkgs.python3Packages.wheel
        pkgs.unzip
        pkgs.makeWrapper
      ];
      
      propagatedBuildInputs = with pkgs.python3Packages; [
        numpy
        pygments
        tensorflow
      ];
      
      buildPhase = ''
        # Build the Python package
        ${pkgs.python3}/bin/python3 setup.py bdist_wheel
      '';
      
      installPhase = ''
        # Install the wheel directly without pip
        mkdir -p $out/lib/python${pkgs.python3.pythonVersion}/site-packages
        
        # Extract the wheel directly
        cd dist
        unzip -q *.whl -d $out/lib/python${pkgs.python3.pythonVersion}/site-packages/
        
        # Copy the vehicle executable
        mkdir -p $out/bin
        ln -s ${haskellPackages.vehicle}/bin/vehicle $out/bin/vehicle
        
        # Create a wrapper script
        cat > $out/bin/vehicle-python << EOF
#!/usr/bin/env python3
import vehicle_lang
vehicle_lang.__main__.main()
EOF
        chmod +x $out/bin/vehicle-python
      '';
      
      # Set up runtime environment
      postFixup = ''
        # Wrap Python scripts with correct environment
        for script in $(find $out/bin -executable -type f); do
          wrapProgram $script \
            --set VEHICLE_PATH ${haskellPackages.vehicle}/bin/vehicle \
            --prefix PATH : ${haskellPackages.vehicle}/bin \
            --prefix PYTHONPATH : "$out/lib/python${pkgs.python3.pythonVersion}/site-packages:$PYTHONPATH" \
            --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath [
              pkgs.gmp
              pkgs.ncurses
              pkgs.python3
            ]}
        done
        
        # Fix Python shebangs if needed
        for f in $(find $out -type f -name "*.py"); do
          if grep -q "/usr/bin/env python" "$f"; then
            substituteInPlace $f --replace "/usr/bin/env python" "python"
          fi
        done
      '';
      
      doCheck = false;
    };
    in {

       # haskellProjects = {
# Define the package set
    packages = {
      inherit agdaWithPackages pythonEnv;
      inherit (haskellPackages) vehicle vehicle-syntax tasty-golden-executable;
      inherit vehicle-lang;
      inherit vehiclePython;
      default = haskellPackages.vehicle;
    };


    devShells.default = pkgs.mkShell {
      # inputsFrom = [];
      packages = [
        agdaWithPackages
        pythonEnv
        haskellPackages.vehicle
        haskellPackages.vehicle-syntax
        haskellPackages.tasty-golden-executable
        # vehicle-lang
        # vehiclePython
        haskellPackages.cabal-install
      ];
    };
    # Define the default app
    apps.default = {
      type = "app";
      program = "vehicle";
      runtimeDependencies = [ haskellPackages.vehicle ];
    };
  };
};
}
