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
  cardano-node-gitrev = "a29ee68ba2c850cd50be39a3105ef191cfbc41d5";

  shellWithHoogle = false;

  sha256map = {
    "https://github.com/james-iohk/cardano-node"."169703647b7dc5981b1932d10c5f1a239785cc74" = "sha256-/8zm/kuIQTqcnFWEnknH3j/aOKYFV50sTsN27yNg8jA=";
    "https://github.com/james-iohk/cardano-api"."7e1cef1f9b7f133cd9197d7ecf29b77acd4d8c60" = "sha256-CrHfnih6pkBfgQYJCuy/r0nir4BECZH6YUcuaEBsk20=";
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
