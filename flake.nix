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
      overlays = [ (old: new:
        { haskellPackages = (haskellPackages new)
          ; ghcWithPackages = new.ghc.withPackages (p:
              with p;
              [vehicle-syntax tasty-golden-executable vehicle]);
        })
      ];
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
    haskellPackages = pkgs: (pkgs.haskellPackages.override {
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
    });

    # Build the Haskell library with the C wrapper
    vehicle-python-bindings = with (haskellPackages pkgs); pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.doJailbreak
        (pkgs.haskell.lib.addExtraLibraries
          (callCabal2nix "vehicle-python-binding" ./vehicle-python {})
          [ vehicle tasty-golden-executable optparse-applicative BNFC text ])
      ) (old: {

        # Make sure GCC can find Python.h and vendor files are available
        preConfigure = ''
          export IS_NIX_BUILD=1
          echo "pwd: "
          echo $(pwd)
          ls -l ../
          ls -al ../vehicle-python
          ls -al ../vehicle-python/src/vehicle_lang/
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

          mkdir -p src/vehicle_lang
          cp -r --no-preserve=mode $src/src/vehicle_lang ./src/

          # Generate binding_wrap.c directly within the vehicle-lang build directory
          # The source files (binding.i and binding.def) are already available via $src
          cd src/vehicle_lang
          ${pkgs.swig}/bin/swig -python -o binding_wrap.c binding.i
          cd ../..
        '';
      });
    # # Build the Haskell library with the C wrapper
    # vehicle-lang = with (haskellPackages pkgs); pkgs.haskell.lib.overrideCabal
    #   (pkgs.haskell.lib.doJailbreak
    #     (pkgs.haskell.lib.addExtraLibraries
    #       (callCabal2nix "vehicle-python-binding" ./vehicle-python {})
    #       [ vehicle tasty-golden-executable optparse-applicative BNFC text ])
    #   ) (old: {
    #     # Make sure GCC can find Python.h and vendor files are available
    #     preConfigure = ''
    #       # Find Python include directory
    #       pythonIncludeDir=${pkgs.python3}/include/python3.12
    #       configureFlags+=" --extra-include-dirs=$pythonIncludeDir"

    #       # Create a clean vendor directory
    #       rm -rf vendor
    #       mkdir -p vendor

    #       # Copy necessary files for vehicle-syntax
    #       mkdir -p vendor/vehicle-syntax/src/Vehicle/Syntax
    #       cp ${./vehicle-syntax}/src/Vehicle/Syntax/External.cf vendor/vehicle-syntax/src/Vehicle/Syntax/
    #       cp ${./vehicle-syntax}/src/Vehicle/Syntax/Internal.cf vendor/vehicle-syntax/src/Vehicle/Syntax/
    #       cp ${./vehicle-syntax}/vehicle-syntax.cabal vendor/vehicle-syntax/

    #       # Copy necessary files for vehicle - use copying instead of symlinks
    #       mkdir -p vendor/vehicle/src
    #       cp ${./vehicle}/vehicle.cabal vendor/vehicle/
    #       cp -r ${./vehicle}/src/Vehicle vendor/vehicle/src/

    #       # Copy necessary files for tasty-golden-executable - use copying instead of symlinks
    #       mkdir -p vendor/tasty-golden-executable
    #       cp ${./tasty-golden-executable}/tasty-golden-executable.cabal vendor/tasty-golden-executable/
    #       cp -r ${./tasty-golden-executable}/src vendor/tasty-golden-executable/
    #       mkdir -p src/vehicle_lang
    #       cp ${swigGen}/src/vehicle_lang/binding_wrap.c $src/src/vehicle_lang/binding_wrap.c
    #     '';
    #   });

    in {

       # haskellProjects = {
# Define the package set
    packages = {
      inherit agdaWithPackages pythonEnv;
      inherit ((haskellPackages pkgs)) vehicle vehicle-syntax tasty-golden-executable;
      inherit vehicle-python-bindings;
      default = (haskellPackages pkgs).vehicle;
      vp = inputs.dream2nix.lib.evalModules {
            packageSets.nixpkgs = pkgs;
            modules = [
              ./vehicle-python/default.nix
              ({pkgs , ...}: {
                paths.projectRoot = ./.;
                paths.projectRootFile = "flake.nix";
                paths.package = ./vehicle-python/.;

                # Inject the needed dependencies
                deps = {
                  vehicle = (haskellPackages pkgs).vehicle;
                };
                # Set environment variables for the build
                env.BINDING_WRAP_PATH = let
                  version = "9.8.4";  #(haskellPackages pkgs).ghc.version;
                in "${vehicle-python-bindings}/lib/ghc-${version}/lib";
                env.USE_SWIG_WRAPPER = "1";
                env.VEHICLE_PATH = "${(haskellPackages pkgs).vehicle}/bin/vehicle";
                env.IS_NIX_BUILD = "1";
              })
            ];
          };
    };


    devShells.default = pkgs.mkShell {
      # inputsFrom = [];
      packages = [
        agdaWithPackages
        pythonEnv
        (haskellPackages pkgs).vehicle
        (haskellPackages pkgs).vehicle-syntax
        (haskellPackages pkgs).tasty-golden-executable
        # vehicle-lang
        (haskellPackages pkgs).cabal-install
      ];
    };
    # Define the default app
    apps.default = {
      type = "app";
      program = "${self'.packages.vehicle}/bin/vehicle";
    };
  };
};
}
