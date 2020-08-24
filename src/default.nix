{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          ioref-stable
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ ghc pkgs.pdf2svg ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}