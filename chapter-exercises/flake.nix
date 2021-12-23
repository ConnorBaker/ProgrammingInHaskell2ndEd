{
  description = "A flake for working through Hutton's Programming in Haskell (2nd ed.)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      {
        devShell = mkShell {
          packages = [ haskell.compiler.ghc8107Binary cabal-install ];
        };
      }
    );
}