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
    imports = [ inputs.haskell-flake.flakeModule
    inputs.flake-parts.flakeModules.easyOverlay
    ];
    #debug = true;
    perSystem = { self', system, config, pkgs, lib, ... }:
    let
      ghcVsn = "948";
      haskellOverlay = (final: prev: { haskellPackages = (haskellPackages final); });
      devOverlay = (final: prev: {
        ghcWithPackages = final.haskellPackages.ghc.withPackages (p:
        [p.vehicle-syntax p.tasty-golden-executable p.vehicle
        p.vehicle-python-bindings
        ]);
      });
      ov = lib.composeExtensions haskellOverlay devOverlay;
      # Override config to allow broken packages
      pkgs = import nixpkgs {
        inherit system;
        config = {
          allowBroken = true;
          allowUnfree = true;
        };
        overlays = [ ov ];
      };

      # Setup for Agda
      agdaWithPackages = pkgs.agda.withPackages (ps: [
        ps.standard-library
      ]);

      # Fix linkedhashmap in haskell packages
      haskellPackages = pkgs: (pkgs.haskell.packages."ghc${ghcVsn}".override {
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
            (hself.callCabal2nix "vehicle" ./vehicle {})
          );

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
        };
      });

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
              vsn = lib.strings.concatStrings (lib.strings.intersperse "." (lib.strings.stringToCharacters ghcVsn));
            in "${pkgs.haskellPackages.vehicle-python-bindings}/lib/ghc-${vsn}/lib";
            env.USE_SWIG_WRAPPER = "1";
            env.IS_NIX_BUILD = "1";
          })
        ];
      };
    in {
      overlayAttrs = {
        inherit (config.packages)
        vehicle vehicle-syntax vehicle-python-bindings tasty-golden-executable;
        vehicle-lang = (vp.packages.default);
      };
      # haskellProjects = {
        # Define the package set
        packages = {
          default = inputs.dream2nix.lib.evalModules {
            packageSets.nixpkgs = nixpkgs.legacyPackages.${system};
            modules = [
              # Import our actual package definiton as a dream2nix module from ./default.nix
              ./default.nix
              {
                # Aid dream2nix to find the project root. This setup should also works for mono
                # repos. If you only have a single project, the defaults should be good enough.
                paths.projectRoot = ./.;
                # can be changed to ".git" or "flake.nix" to get rid of .project-root
                paths.projectRootFile = "flake.nix";
                paths.package = ./.;
              }
            ];
          };
          inherit vp;
        };

        # devShells.default = pkgs.mkShell {
        #   # Remove the tensorboard collision by creating a modified Python environment
        #   # # Get the original dev shell from vp but filter out TensorBoard
        #   inputsFrom = [config.devShells.haskell];
        #   packages = let vpDevInputs = builtins.filter (x:
        #   !(pkgs.lib.hasPrefix "python3.12-tensorboard-" (builtins.baseNameOf (builtins.toString x))))
        #   (builtins.concatLists
        #   (builtins.map (x: if builtins.hasAttr "buildInputs" x then x.buildInputs else [])
        #   (if builtins.hasAttr "inputsFrom" vp.devShell then vp.devShell.inputsFrom else [vp.devShell])
        #   )
        #   );
        #   in [
        #     agdaWithPackages
        #     #pkgs.ghcWithPackages
        #   ] ++ vpDevInputs;
        # };
        # Define the default app
        apps.default = {
          type = "app";
          program = "${self'.packages.default}/bin/vehicle";
        };
    };
  };
}
