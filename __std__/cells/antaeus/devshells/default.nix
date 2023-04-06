{ inputs, cell }@block: rec
{
  default = antaeus-shell;

  antaeus-shell = import ./antaeus-shell.nix block;
}
