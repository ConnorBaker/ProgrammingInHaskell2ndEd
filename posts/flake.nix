{
  description = "A flake for blogging through Hutton's Programming in Haskell (2nd ed.)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      {
        devShell = mkShell {
          packages = [ pandoc tectonic fish ];
        };
      }
    );
}
