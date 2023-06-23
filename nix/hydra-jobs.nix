{ inputs, inputs', pkgs }:
{
  includedPaths = [
    "packages"
    "devShells"
    "checks"
  ];

  excludedPaths = [
    "packages.ghc927-profiled"
    "devShells.ghc927-profiled"
    "checks.ghc927-profiled"
  ];
}