# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixpre-commit-checknix

{
  cabal-fmt.enable = true;

  nixpkgs-fmt.enable = true;

  shellcheck.enable = true;

  editorconfig-checker.enable = true;

  fourmolu.enable = false;
  fourmolu.extraOptions = "--ghc-opt -XTypeApplications --ghc-opt -XQuasiQuotes";

  hlint.enable = false;
}