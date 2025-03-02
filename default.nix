{ nixpkgs ? import <nixpkgs> { config = { allowUnfree = true; }; } }:
let
  inherit (nixpkgs) pkgs;
  pypkgs_to_install = ps: with ps; [
    ipykernel ipywidgets pyzmq
    pylint setuptools pip
  ];

  python3WithPkgs = pkgs.python3.override {
    packageOverrides = self: super: {
    };
  };
  # environment used in the Python kernel in Jupyter.
  pythonEnv = python3WithPkgs.buildEnv.override {
    extraLibs = pypkgs_to_install python3WithPkgs.pkgs;
  };
  jupyterPath = pkgs.jupyter-kernel.create {
    definitions = {
      python3 = {
        displayName = "Python 3 - NixOS";
        argv = [
          "${pythonEnv.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
        logo32 = "${pythonEnv}/${pythonEnv.sitePackages}/ipykernel/resources/logo-32x32.png";
        logo64 = "${pythonEnv}/${pythonEnv.sitePackages}/ipykernel/resources/logo-64x64.png";
      };
    };
  };
  # jupyterEnv contains the Jupyter interface and extensions
  jupyterEnv = python3WithPkgs.buildEnv.override {
    extraLibs = with python3WithPkgs.pkgs; pypkgs_to_install python3WithPkgs.pkgs ++ [
      notebook
      qtconsole
      jupyter_console
      nbconvert
      widgetsnbextension
    ];
    makeWrapperArgs = ["--set JUPYTER_PATH ${jupyterPath}"
      # This is needed because Python's zip implementation doesn't understand old dates.
      "--set SOURCE_DATE_EPOCH 315532800"];
  };

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
pkgs.mkShell {
  name = "my-env-0";
  buildInputs = [ ghc pkgs.haskellPackages.haskell-language-server pkgs.haskellPackages.llvmPackages.clang jupyterEnv pkgs.gnuplot pkgs.openblas]; # pkgs.pdf2svg pkgs.dot2tex tex ];
  passthru = { inherit pypkgs_to_install pkgs; };
  shellHook = ''
    export PIP_PREFIX=/home/aerg/projects/fitness/grocery/pip_packages
    export PYTHONPATH="$PIP_PREFIX/${python3WithPkgs.sitePackages}:$PYTHONPATH"
    export JUPYTER_PATH="${jupyterPath}"
    export PATH="$PIP_PREFIX/bin:$PATH"
    eval $(egrep ^export ${ghc}/bin/ghc)
    unset SOURCE_DATE_EPOCH
  '';
}
