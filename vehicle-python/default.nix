{
  dream2nix,
  config,
  lib,
  ...
}: let
  pyproject =
    builtins.fromTOML
    (builtins.readFile (config.mkDerivation.src + /pyproject.toml));
in {
  imports = [
    dream2nix.modules.dream2nix.pip
  ];
  
  
  deps = {nixpkgs, ...}: {
    python = nixpkgs.python312;
    pythonPackages = nixpkgs.python312Packages;
    # SWIG is now injected from flake.nix to avoid duplicate definitions
    cabal = nixpkgs.haskellPackages.cabal-install;
    ghc = nixpkgs.ghcWithPackages;
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
    ];
    
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
    
    #install = ''
    # '';
    # After installation, make sure the vehicle executable is available
    postInstall = ''
      if [ -n "${if config.deps.vehicle != null then "${config.deps.vehicle}" else ""}" ]; then
        # Create a wrapper script to run vehicle
        mkdir -p $out/bin
        cat > $out/bin/vehicle-python << EOF
        #!/bin/sh
        export VEHICLE_PATH=${config.deps.vehicle}/bin/vehicle
        exec python -m vehicle_lang "\$@"
        EOF

        chmod +x $out/bin/vehicle-python
        
        # Link the vehicle executable for direct access
        ln -sf ${config.deps.vehicle}/bin/vehicle $out/bin/vehicle
      fi
    '';
  };

  buildPythonPackage = {
    pyproject = true;
  };

  name = pyproject.project.name;
  version = pyproject.project.version;

  # Include SWIG in the build process
  pip.requirementsList = pyproject.project.dependencies
                         ++ (lib.lists.flatten (lib.attrsets.attrValues pyproject.project.optional-dependencies))
                         ++ pyproject.build-system.requires;
  
  
  pip.flattenDependencies = true;
}



# {
#   lib, 
#   pkgs ? import <nixpkgs> {}, 
#   python3 ? pkgs.python3,
#   pythonPackages ? python3.pkgs,
#   vehicle ? null,
#   vehicleSyntax ? null
# }:

# let
#   # If vehicle and vehicleSyntax are not provided, try to build them
#   vehicleDrv = if vehicle != null then vehicle else 
#     pkgs.haskellPackages.callCabal2nix "vehicle" ../vehicle {};
  
#   vehicleSyntaxDrv = if vehicleSyntax != null then vehicleSyntax else
#     pkgs.haskellPackages.callCabal2nix "vehicle-syntax" ../vehicle-syntax {};

# in pythonPackages.buildPythonPackage {
#   pname = "vehicle-lang";
#   version = "0.16.1";  # Match version from cabal file
#   src = ./.;
#   format = "setuptools";
  
#   buildInputs = [
#     vehicleDrv
#     vehicleSyntaxDrv
#     pkgs.swig
#     pkgs.cabal-install
#     pythonPackages.setuptools
#     pythonPackages.wheel
#   ];
  
#   propagatedBuildInputs = with pythonPackages; [
#     numpy
#     pygments
#     tensorflow
#     pytest
#   ];
  
#   preBuild = ''
#     # Set up the environment to build the Haskell foreign library
#     export VEHICLE_PATH=${vehicleDrv}/bin/vehicle
    
#     # Build the foreign library
#     cabal build foreign-library:_binding --verbose --ghc-options="-dynamic"
    
#     # Find and copy the shared library to the appropriate location
#     mkdir -p src/vehicle_lang
#     find dist-newstyle -name "lib_binding*.so" -o -name "lib_binding*.dylib" -o -name "_binding.dll" -exec cp {} src/vehicle_lang/ \;
#   '';
  
#   checkInputs = with pythonPackages; [
#     pytest
#   ];
  
#   doCheck = false;  # Disable tests for now
  
#   meta = with lib; {
#     description = "Python bindings for Vehicle language";
#     homepage = "https://github.com/vehicle-lang/vehicle";
#     license = licenses.mit;
#     maintainers = with maintainers; [ ];
#   };
# }
