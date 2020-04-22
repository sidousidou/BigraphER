let
pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz") {
  overlays = [ (import ./nix/dune_2_5_overlay.nix) (import ./nix/minisat_overlay.nix) ];
};
in with pkgs;
mkShell {
  buildInputs = [
    zlib
    ocaml
    ocamlPackages.dune_2
    ocamlPackages.dune-configurator
    ocamlPackages.findlib
    ocamlPackages.utop
    ocamlPackages.menhir
    ocamlPackages.jsonm
    ocamlPackages.cmdliner
    ocamlPackages.odoc
    minisat
  ];
}
