{
  description = "End-to-End Testing Framework for Cardano Haskell APIs";

  inputs = {
    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.hackage.follows = "hackage_2";
    iogx.inputs.CHaP.follows = "CHaP_2";


    hackage_2 = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP_2 = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;


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
