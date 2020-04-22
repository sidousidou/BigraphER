{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz")
  {
    overlays = [ (import ./nix/dune_2_5_overlay.nix) (import ./nix/minisat_overlay.nix) ];
  }
}:

with pkgs;

ocamlPackages.buildOcaml rec {
    name = "bigrapher-${version}";
    version = "1.9.2";

    src = lib.cleanSource ./.;

    buildInputs = with ocamlPackages; [ pkgs.minisat dune-configurator menhir zlib jsonm cmdliner ];
    nativeBuildInputs = [ dune_2 ];

    buildPhase = ''
      dune build @install --profile=release
    '';

    installPhase = ''
      dune install --profile=release --prefix=$out
    '';

  }
