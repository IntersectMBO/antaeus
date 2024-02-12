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
      url = "github:input-output-hk/hackage.nix/45c962ae6bdf4274615aabcbe41c00c76d185ac5";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages/501510e79f9cf76012cba0e86f88fa0b8b053fbd";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/c390991becb2a45a0963274e7924d3deaefcea29";
      inputs.hackage.follows = "hackage";
    };
  };

  outputs = inputs: inputs.iogx.lib.mkFlake {
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ];
    outputs = import ./nix/outputs.nix;
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
