{
  config,
  dream2nix,
  lib,
  ...
}: {
  imports = [
    dream2nix.modules.dream2nix.WIP-haskell-cabal
  ];

  name = "vehicle";
  version = "0.17.0";

  deps = {nixpkgs, ...}: {
    haskell-compiler = nixpkgs.haskell.compiler.ghc981;
    inherit (nixpkgs) zlib;
  };

  mkDerivation = {
    src = lib.cleanSourceWith {
      src = lib.cleanSource ./.;
      filter = name: type: let
        baseName = baseNameOf (toString name);
      in
        !(
          lib.hasSuffix ".nix" baseName
        );
    };
    buildPhase = lib.mkForce ''
      runHook preBuild

      mkdir -p $out/bin

      mkdir -p .cabal
      touch .cabal/config

      HOME=$(pwd) cabal install vehicle \
                  --offline             \
                  --installdir $out/bin \
                  --install-method copy \
                  -j

      runHook postBuild
    '';


    buildInputs = [config.deps.zlib];
  };
}
