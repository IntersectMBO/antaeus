{ inputs, inputs', pkgs }:
{
  includedPaths = [
    "packages"
    "devShells"
    "checks"
  ];

  excludedPaths = [
    "checks.ghc927-mingwW64"
    "packages.ghc927-mingwW64"
    "devShells.ghc927-mingwW64"
    "packages.ghc927-profiled"
    "devShells.ghc927-profiled"
    "checks.ghc927-profiled"
  ];
}