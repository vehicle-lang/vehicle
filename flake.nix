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
          
          # Debug - show the vendor directory structure
          echo "Vendor directory structure:"
          find vendor -type f | sort
        '';
        preBuild = ''
          # Copy in the SWIG-generated wrapper
          cp ${swigGen}/src/vehicle_lang/binding_wrap.c src/vehicle_lang/binding_wrap.c
        '';
      });
    # Python bindings with pre-built Haskell library
    vehiclePython = pkgs.python3Packages.buildPythonPackage {
      pname = "vehicle-lang";
      version = "0.16.1";
      src = ./vehicle-python;
      format = "setuptools";
      
      # Don't attempt to build the binding ourselves
      dontBuild = true;
      
      nativeBuildInputs = [
        pkgs.swig
        haskellPackages.vehicle
        haskellPackages.vehicle-syntax
        haskellPackages.tasty-golden-executable
      ];
      
      propagatedBuildInputs = with pkgs.python3Packages; [
        numpy
        pygments
        tensorflow
      ];
      
      installPhase = ''
        # Create directories
        mkdir -p $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang
        
        # Create a temporary copy of the source that we can modify
        cp -r $src/src/vehicle_lang $TMP/vehicle_lang
        chmod -R +w $TMP/vehicle_lang
        
        # Copy all Python files from temp location
        cp -r $TMP/vehicle_lang/*.py $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/
        
        # Copy py.typed if it exists
        if [ -f $TMP/vehicle_lang/py.typed ]; then
          cp $TMP/vehicle_lang/py.typed $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/
        fi
        
        # Copy subdirectories
        for dir in $(find $TMP/vehicle_lang -type d -not -path "$TMP/vehicle_lang"); do
          if [ -d "$dir" ]; then
            rel_dir=''${dir#$TMP/vehicle_lang/}
            target_dir=$out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/$rel_dir
            mkdir -p "$target_dir"
            
            # Copy files from this subdirectory
            for file in $(find "$dir" -maxdepth 1 -type f); do
              cp "$file" "$target_dir"/
            done
          fi
        done
        
        # Copy the Haskell shared library for the binding
        if [ -d ${vehicle-lang}/lib ]; then
          find ${vehicle-lang}/lib -name "lib_binding.*" -type f -exec cp {} $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/_binding.so \;
        else
          # Create a stub binding if the real one isn't available
          echo 'print("Warning: Using stub Vehicle Python binding - Haskell binding is not available")' > $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/_binding.py
        fi
        
        # Add the vehicle executable to PATH
        mkdir -p $out/bin
        ln -s ${haskellPackages.vehicle}/bin/vehicle $out/bin/
        
        # Create minimal egg-info
        mkdir -p $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang-0.16.1.egg-info
        echo "Metadata-Version: 2.1
Name: vehicle-lang
Version: 0.16.1
Summary: Vehicle Python Bindings
Home-page: https://github.com/vehicle-lang/vehicle
Author: Vehicle Team
License: MIT" > $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang-0.16.1.egg-info/PKG-INFO
      '';
      
      # Set up runtime environment
      postFixup = ''
        wrapProgram $out/lib/python${pkgs.python3.pythonVersion}/site-packages/vehicle_lang/_binding.so \
          --set VEHICLE_PATH ${haskellPackages.vehicle}/bin/vehicle \
          --prefix PATH : ${haskellPackages.vehicle}/bin \
          --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath [
            pkgs.gmp
            pkgs.ncurses
            pkgs.python3
          ]}
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
