# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixcabal-projectnix

{ inputs, inputs', meta, config, pkgs, ... }:

let
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;

  packages = {

    e2e-tests.package.buildable = !isCross;
    e2e-tests.ghcOptions = [ "-Werror" ];
    e2e-tests.doHaddock = meta.enableHaddock;
    e2e-tests.flags.defer-plugin-errors = meta.enableHaddock;

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
    e2e-tests.components.tests.antaeus-test.preCheck = "
      export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
      export CARDANO_NODE_SRC=${../.}
    ";
  };

  # TODO this is temporary and will be done automatically by IOGX in the next version
  cardano-node-gitrev = "eb66a54f5d03d96fa9f4899b4f43ebf4e829f2ca";

  shellWithHoogle = false;

  sha256map = {
    "https://github.com/input-output-hk/cardano-node".${cardano-node-gitrev} = "sha256-QYqWuJPk0frnBrcRAv4SzvAckLMBnaKThP5Gl5lHN44=";
    "https://github.com/james-iohk/cardano-node"."342415aef56d06e9f2bcfc93fcbc466e44a7d497" = "sha256-9/tG/XQvhbBOQ+7LAaCINjDijTzw+v4y3w0WT8+/3cE=";
    "https://github.com/james-iohk/cardano-api"."022495a1c6b99a881c49ae25f26f1389ab63aaa5" = "sha256-k735IKmhzp4IftzTVwtKCpzwdUFdLfw8Yv0WwgqLYjk=";
  };

  modules = [{ inherit packages; }];

  overlays = [
    (final: prev: {
      hsPkgs = pkgs.pkgsBuildBuild.setGitRevForPaths cardano-node-gitrev [
        "cardano-cli.components.exes.cardano-cli"
        "cardano-node.components.exes.cardano-node"
      ]
        prev.hsPkgs;
    })
  ];

in
{
  inherit shellWithHoogle modules sha256map overlays;
}
