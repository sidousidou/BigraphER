{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz") {} }:

with pkgs;

let
  dune = dune_2;
  callPkgs = p: lib.callPackageWith (pkgs // ocamlPackages // local) p;
  local = {
    minisat = callPkgs ./nix/minisat.nix { };
    camlminisat = callPkgs ./nix/camlminisat.nix {};
  };
in

ocamlPackages.buildOcaml rec {
    name = "bigrapher-${version}";
    version = "1.9.2";

    src = lib.cleanSource ./.;

    buildInputs = with ocamlPackages; [ local.camlminisat local.minisat menhir zlib jsonm cmdliner ];
    nativeBuildInputs = [ dune_2 ];

    buildPhase = ''
      dune build @install --profile=release
    '';

    installPhase = ''
      dune install --profile=release --prefix=$out
    '';

  }
