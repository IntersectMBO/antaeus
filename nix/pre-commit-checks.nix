{ inputs, inputs', pkgs, project }:

{
  cabal-fmt.enable = true;

  nixpkgs-fmt.enable = true;

  shellcheck.enable = true;

  editorconfig-checker.enable = true;

  fourmolu.enable = true;
  fourmolu.extraOptions = "--ghc-opt -XTypeApplications";

  hlint.enable = true;
}


  # - DataKinds
  # - DeriveAnyClass
  # - DeriveGeneric
  # - DerivingStrategies
  # - DerivingVia
  # - ExistentialQuantification
  # - ExplicitNamespaces
  # - FlexibleContexts
  # - GeneralizedNewtypeDeriving
  # - MultiParamTypeClasses
  # - NamedFieldPuns
  # - PackageImports
  # - QuasiQuotes
  # - ScopedTypeVariables
  # - TemplateHaskell
  # - TypeApplications
  # - ImportQualifiedPost