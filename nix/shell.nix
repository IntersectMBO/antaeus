{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

let
  cardano-cli = cabalProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
  cardano-node = cabalProject.hsPkgs.cardano-node.components.exes.cardano-node;
in

{
  name = "antaeus";

  prompt = "\n\\[\\033[1;32m\\][antaeus:\\w]\\$\\[\\033[0m\\] ";
  
  welcomeMessage = "🤟 \\033[1;31mWelcome to antaeus\\033[0m 🤟";


  packages = [ cardano-cli cardano-node ];


  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };


  preCommit = {
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    editorconfig-checker.enable = true;
    hlint.enable = false;
    fourmolu.enable = true;
    fourmolu.extraOptions = "--ghc-opt -XTypeApplications --ghc-opt -XQuasiQuotes";
  };
}
