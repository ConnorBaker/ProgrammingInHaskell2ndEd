{
  description = "A flake for Hakyll";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with nixpkgs.legacyPackages.${system};
      let
        ghcVersion = "ghc8107";
        haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [cabal-install hakyll]);
      in {
        devShell = mkShell {
          packages = [haskellEnv nodePackages.tailwindcss];
          shellHook = ''
            alias clean_blog="rm -rf _cache/ docs/ dist-newstyle/"
            alias build_blog="cabal run site build && tailwindcss -i blog_tailwind.css -o ./css/blog.css && cabal run site build"
            alias run_blog="build_blog && cabal run site watch"
            alias publish_blog="clean_blog && build_blog && mv docs .. && clean_blog"
          '';
        };
      }
    );
}