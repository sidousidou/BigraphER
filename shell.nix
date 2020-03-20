{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz") {}
, unstable ? import <nixpkgs> {} }:

with pkgs;

let
  callPkgs = p: lib.callPackageWith (pkgs // ocamlPackages // local) p;
  local = {
    minisat = callPkgs ./nix/minisat.nix {};
    camlminisat = callPkgs ./nix/camlminisat.nix {};
  };
in
mkShell {
  buildInputs = [
    zlib
    unstable.dune_2
    ocaml
    ocamlPackages.findlib
    ocamlPackages.utop
    ocamlPackages.menhir
    ocamlPackages.jsonm
    ocamlPackages.cmdliner
    ocamlPackages.odoc
    local.minisat
    local.camlminisat
  ];
}
