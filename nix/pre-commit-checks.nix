{
  cabal-fmt.enable = true;

  nixpkgs-fmt.enable = true;

  shellcheck.enable = true;

  editorconfig-checker.enable = true;

  fourmolu.enable = false;
  fourmolu.extraOptions = "--ghc-opt -XTypeApplications --ghc-opt -XQuasiQuotes";

  hlint.enable = false;
}