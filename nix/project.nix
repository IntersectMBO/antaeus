{ repoRoot, inputs, pkgs, lib, system }:

let
  cardano-node-gitrev = "4bb2048db77d623ee6e3678618c2d8b6c4676333";


  cabalProject' = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }:
    let
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in
    {
      compiler-nix-name = "ghc963";

      src = ../.;

      shell.withHoogle = false;

      sha256map = {
        "https://github.com/IntersectMBO/cardano-node"."${cardano-node-gitrev}" = "sha256-pWXI8dyqKQ3HncbBtd54wdHi3Pj7J5y+nybqpzMXOj4=";
      };

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.CHaP;
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
