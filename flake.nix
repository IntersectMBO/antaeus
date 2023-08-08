# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "End-to-End Testing Framework for Cardano Haskell APIs";

  inputs = {
    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.hackage.follows = "hackage";
    iogx.inputs.CHaP.follows = "CHaP";


    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ];
  };


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
