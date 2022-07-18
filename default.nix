{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages.override {
          useLLVM = true;
        } (ps: with ps; [
          haskell-dap ghci-dap haskell-debug-adapter
          persistent-sqlite persistent-template
          ioref-stable concurrent-extra unliftio
          extra heaps unordered-containers
          exceptions kan-extensions
          cabal-install
          quickcheck-instances tasty tasty-hunit tasty-quickcheck
            # for mutable interaction-net style graphs
          store
            # for fast serialization/deserialization
            # flat is more compact, but lz4 compression should be better than that.
            # - and we aren't dealing with a lot of data anyway
          ansi-terminal
          ansi-wl-pprint
            # for pretty printing messages. Shake doesn't use it for historic/manpower reasons.
            # Neil seems to want a DSL...
          cryptonite
            # SHA256, BLAKE2, and other cryptographic algorithms. Eventually
            # this will get replaced by the custom C file-hashing code, but for now it's a good reference.
          memory
            # FNV1/1a and siphash. also a dependency of cryptonite providing unpinned byte array methods.
        ]);
  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-small pgf bussproofs preview varwidth standalone geometry amsmath;
  };
in
pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.haskellPackages.llvmPackages.clang ] ++ []; # pkgs.pdf2svg pkgs.dot2tex tex ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
