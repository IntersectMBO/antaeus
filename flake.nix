{
  description = "";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;


  nixConfig = {

    extra-substituters = [
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    accept-flake-config = true;

    allow-import-from-derivation = true;
  };
}
