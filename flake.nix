{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};
  in with pkgs; {
    devShell = mkShell {
      nativeBuildInputs = [
        ghc
        haskell-language-server
        cabal-install
        stack
      ];
    };
  });
}
