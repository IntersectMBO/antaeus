{ repoRoot, inputs, pkgs, lib, system }:
let
  project = repoRoot.nix.project;
in
[
  (
    project.flake // {
      checks =
        let
          # https://github.com/numtide/flake-utils/issues/121#issuecomment-2589899217
          recurseIntoDeepAttrs = attrs:
            lib.recurseIntoAttrs
              (lib.mapAttrs
                (_: v:
                  if builtins.typeOf v == "set" && !lib.isDerivation v
                  then recurseIntoDeepAttrs v
                  else v
                )
                attrs
              );
        in
        inputs.iogx.inputs.flake-utils.lib.flattenTree (recurseIntoDeepAttrs project.flake.hydraJobs);
    }
  )
]
