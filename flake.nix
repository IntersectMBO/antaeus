{
  description = "Antaeus End-to-End Testing Framework";

  inputs = {
    iogx.url = "github:zeme-iohk/iogx";
  };

  outputs = inputs:
    inputs.iogx.mkFlake {
      inherit inputs;
      repoRoot = ./.;
      shellName = "antaeus";
      shellPrompt = "\n\\[\\033[1;31m\\][antaeus:\\w]\\$\\[\\033[0m\\] ";
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
