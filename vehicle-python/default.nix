{ dream2nix, config, lib, ... }:
let
  pyproject = builtins.fromTOML
    (builtins.readFile (config.mkDerivation.src + /pyproject.toml));
in {
  imports = [ dream2nix.modules.dream2nix.pip ];

  deps = { nixpkgs, ... }: {
    python = nixpkgs.python312;
    pythonPackages = nixpkgs.python312Packages;
    # SWIG is now injected from flake.nix to avoid duplicate definitions
    cabal = nixpkgs.haskellPackages.cabal-install;
    ghc = nixpkgs.ghcWithPackages;
    swig = nixpkgs.swig;
    # Extra attributes that can be passed in from flake.nix
  };

  mkDerivation = {
    src = ./.;
    nativeBuildInputs = [
      config.deps.vehicle
      config.deps.ghc
      config.deps.cabal
      config.deps.swig
      config.deps.pythonPackages.find-libpython
      config.deps.pythonPackages.packaging
      config.deps.pythonPackages.setuptools
      config.deps.pythonPackages.jupyter
    ];

    # TODO: This nix path should not be hard-coded but you
    #       can't run the stuff in the module from the shell if it's not there
    shellHook = ''
      export PYTHONPATH=./vehicle-python/src/vehicle_lang:$PYTHONPATH
    '';

    # Set up the build environment
    preConfigure = ''
      # Ensure tools are in PATH
      export PATH="${config.deps.swig}/bin:$PATH"
      export SWIG=${config.deps.swig}/bin/swig
      export SWIG_OUTPUT_PATH=$BINDING_WRAP_PATH
      export LOCAL_SRC=build

      # Set up Python path for dependencies from pyproject.toml
      PIP_DEPS_PATH="$(${config.deps.python}/bin/python -c 'import site; print(site.getsitepackages()[0])')"
      export PYTHONPATH="$PIP_DEPS_PATH:$PYTHONPATH"
      export PYTHONPATH="$PYTHONPATH"
      echo $PYTHONPATH

      # Create missing README.md and LICENSE files
      touch README.md LICENSE

      # Create vendor directory structure
      mkdir -p vendor
      ln -sf ../../vehicle vendor/vehicle
      ln -sf ../../vehicle-syntax vendor/vehicle-syntax
      ln -sf ../../tasty-golden-executable vendor/tasty-golden-executable

      # Create directory for SWIG output
      mkdir -p src/vehicle_lang
      cp -r $src/src/vehicle_lang src/
      ls -l

      # Copy the SWIG wrapper from swigGen if available
      if [ -n "$SWIG_OUTPUT_PATH" ] && [ -f "$SWIG_OUTPUT_PATH" ]; then
        echo "Using SWIG wrapper from swigGen: $SWIG_OUTPUT_PATH"
        cp "$SWIG_OUTPUT_PATH" src/vehicle_lang/binding_wrap.c

        # Tell the build process to use the pre-existing wrapper
      fi

      # If VEHICLE_PATH is set, ensure it's available to the build
      if [ -n "$VEHICLE_PATH" ]; then
        echo "Using vehicle executable at $VEHICLE_PATH"
      fi
    '';
  };
  buildPythonPackage = {
    pyproject = true;
    pythonImportsCheck = [ "vehicle_lang" ];
  };

  name = pyproject.project.name;
  version = pyproject.project.version;

  # Include SWIG in the build process
  # Collect all dependencies but manually exclude tensorboard
  pip.requirementsList = pyproject.project.dependencies ++ (lib.lists.flatten
    (lib.attrsets.attrValues pyproject.project.optional-dependencies))
    ++ pyproject.build-system.requires ++ [ "jupyter" ];

  pip.editables = { vehicle-lang = "${config.mkDerivation.src}"; };
  # pip.overrides = {
  #   tqdm = {
  #     buildPythonPackage.pyproject = true;
  #     mkDerivation.nativeBuildInputs = [ config.deps.python.pkgs.flit-core ];
  #   };
  # };
  pip.flattenDependencies = true;
}
