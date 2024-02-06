{
  description = "End-to-End Testing Framework for Cardano Haskell APIs";


  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";
    iohk-nix.follows = "iogx/iohk-nix";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  /*
    outputs = inputs: inputs.iogx.lib.mkFlake {
      inherit inputs;
      repoRoot = ./.;
      systems = [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ];
      outputs = import ./nix/outputs.nix;
    };
  */

  outputs = inputs:
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.iohk-nix.overlays.crypto
            inputs.iohk-nix.overlays.cardano-lib
            inputs.haskell-nix.overlay
            inputs.iohk-nix.overlays.haskell-nix-crypto
            # WARNING: The order of these is crucial
            # The iohk-nix.overlays.haskell-nix-crypto depends on both the
            # iohk-nix.overlays.crypto and the haskell-nix.overlay overlays
            # and so must be after them in the list of overlays to nixpkgs.
            # TODO check if this constraint still applies in the latest haskell.nix.
            inputs.iohk-nix.overlays.haskell-nix-extra
          ];
          inherit (inputs.haskell-nix) config;
        };

        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc963";

          sha256map = {
            "https://github.com/IntersectMBO/cardano-node"."bf5f688d11fcd6aea3a22df0c2f78538419539e7" = "sha256-5wo2eb6U53wmogIoaCFGPwnHAetpJg5iV1AdaIUSMrI=";
          };
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };

        };
      in
      project.flake { });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };
}
