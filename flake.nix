{
  description = "Antaeus End-to-End Testing Framework";

  inputs = {
    iogx.url = "github:zeme-iohk/iogx";
  };

  outputs = inputs:
    inputs.iogx.mkFlake {
      inherit inputs;
      debug = true;
      repoRoot = ./.;
      flakeOutputsPrefix = "";
      systems = [ "x86_64-darwin" "x86_64-linux" ];
      haskellCompilers = [ "ghc8107" ];
      defaultHaskellCompiler = "ghc8107";
      haskellCrossSystem = null;
      haskellProjectFile = ./nix/haskell-project.nix;
      perSystemOutputsFile = null;
      shellName = "antaeus";
      shellPrompt = "\n\\[\\033[1;31m\\][antaeus:\\w]\\$\\[\\033[0m\\] ";
      shellWelcomeMessage = "🤟 \\033[1;31mWelcome to antaeus\\033[0m 🤟";
      shellModuleFile = ./nix/shell-module.nix;
      includeHydraJobs = true;
      excludeProfiledHaskellFromHydraJobs = true;
      blacklistedHydraJobs = [ ];
      enableHydraPreCommitCheck = true;
      includeReadTheDocsSite = false;
      readTheDocsSiteDir = null;
      readTheDocsHaddockPrologue = "";
      readTheDocsExtraHaddockPackages = _: { };
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };

}
