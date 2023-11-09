{ repoRoot, inputs, pkgs, lib, system }:

let
  cardano-node-gitrev = "1d9e97314a096d4893c50eae18e0602cb1957ca1";


  cabalProject' = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
    let
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      compiler-nix-name = "ghc928";

      src = ../.;

      shell.withHoogle = false;

      sha256map = {
        "https://github.com/james-iohk/cardano-node"."${cardano-node-gitrev}" = "sha256-xK8KyzGXLnxupJaY6kAyMC3KVb7andSGNnO0zwlScZ4=";
        "https://github.com/james-iohk/cardano-api"."347a20562f087cdb1d3660da3e5adb5cbcb64bfe" = "sha256-y9obzxbRtuNdmD6wWKw84L7h1zc9OX5DOz2gA7t2Djs=";
      };

      inputMap = {
        "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
      };

      modules = [{
        packages.e2e-tests.package.buildable = !isCross;
        packages.e2e-tests.ghcOptions = [ "-Werror" ];

        # The lines `export CARDANO_NODE=...` and `export CARDANO_CLI=...`
        # is necessary to prevent the error
        # `../dist-newstyle/cache/plan.json: openBinaryFile: does not exist (No such file or directory)`.
        # See https://github.com/input-output-hk/cardano-node/issues/4194.
        #
        # The line 'export CARDANO_NODE_SRC=...' is used to specify the
        # root folder used to fetch the `configuration.yaml` file (in
        # antaeus, it's currently in the
        # `configuration/defaults/byron-mainnet` directory.
        # Else, we'll get the error
        # `/nix/store/ls0ky8x6zi3fkxrv7n4vs4x9czcqh1pb-antaeus/antaeus/test/configuration.yaml: openFile: does not exist (No such file or directory)`
        packages.e2e-tests.components.tests.antaeus-test.preCheck = "
          export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
          export CARDANO_NODE_SRC=${../.}
        ";
      }];
    });


  cabalProject = cabalProject'.appendOverlays [
    (_: prev: {
      hsPkgs = pkgs.pkgsHostTarget.setGitRevForPaths cardano-node-gitrev [
        "cardano-cli.components.exes.cardano-cli"
        "cardano-node.components.exes.cardano-node"
      ]
        prev.hsPkgs;
    })
  ];


  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    includeMingwW64HydraJobs = true;
    shellArgs = repoRoot.nix.shell;
  };

in

project
