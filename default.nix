{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")
  {
    overlays = [ (import ./nix/dune_2_5_overlay.nix) ];
  }
}:

with pkgs;

stdenv.mkDerivation rec {
  name = "bigrapher-${version}";
  version = "1.9.3";

  src = lib.cleanSource ./.;

  buildInputs = with ocamlPackages; [ pkgs.minisat zlib jsonm cmdliner mtime ];
  nativeBuildInputs = with ocamlPackages; [ dune_2 ocaml findlib dune-configurator menhir ];

  buildPhase = ''
      dune build @install --profile=release
    '';

  installPhase = ''
      dune install --profile=release --prefix=$out
    '';

}
